package typed

import fastparse.noApi._
import shapeless.{:+:, CNil, Coproduct}

object SigmaTypedParser extends App {
  import SigmaApi._

  type ExpressionWith = FieldUpdate :+: MethodUpdate :+: Function :+: Parameter :+: Call :+: CNil

  implicit def rr(x: Product): ExpressionWith = x match {
    case a: FieldUpdate => Coproduct[ExpressionWith](a)
    case a: MethodUpdate => Coproduct[ExpressionWith](a)
    case a: Function => Coproduct[ExpressionWith](a)
    case a: Parameter => Coproduct[ExpressionWith](a)
    case a: Call => Coproduct[ExpressionWith](a)
  }

  type MethodBody = Lambda :+: ObjectType :+: Expression :+: CNil

  implicit def ff(x: Product): MethodBody = x match {
    case a: Lambda => Coproduct[MethodBody](a)
    case a: ObjectType => Coproduct[MethodBody](a)
    case a: Expression => Coproduct[MethodBody](a)
  }

  type Value = ObjectType :+: Expression :+: InputValue :+: CNil

  implicit def vv(x: Object): Value = x match {
    case a: ObjectType => Coproduct[Value](a)
    case a: Expression => Coproduct[Value](a)
    case a: InputValue => Coproduct[Value](a)
  }

  type ParameterValue = InputValue :+: String :+: CNil

  implicit def ll(x: Object): ParameterValue = x match {
    case a: InputValue => Coproduct[ParameterValue](a)
    case a: String => Coproduct[ParameterValue](a)
  }

  final case class Parameter(param: ParameterValue, methodCall: Option[Call])
  final case class Function(param1: Parameter, operator: String, param2: Parameter)

  final case class Type(args: Seq[Either[Type, String]])

  sealed trait InputValue
  final case class StringValue(string: String) extends InputValue
  final case class IntValue(int: Int) extends InputValue
  final case class RealValue(real: Double) extends InputValue

  final case class LambdaArg(argName: String, argType: Type)
  final case class Lambda(params: Seq[Seq[LambdaArg]], body: Expression)

  sealed trait Property
  final case class Field(name: String, typ: Type, value: Value) extends Property
  final case class Method(methodName: String, methodType: Type, contextName: String, methodBody: MethodBody) extends Property

  sealed trait Transform
  final case class FieldUpdate(contextName: Option[String], propertyName: String, typ: Type, value: Value) extends Transform
  final case class MethodUpdate(oldContextName: Option[String], propertyName: String, typ: Type, newContextName: String, body: MethodBody) extends Transform
  final case class Call(contextName: Option[String], propertyName: String, arguments: Seq[Seq[InputValue]]) extends Transform

  final case class Expression(innerExpr: Option[Expression], exprWith: Seq[ExpressionWith])

  sealed trait SigmaObject
  final case class ObjectType(props: Seq[Property]) extends SigmaObject
  final case class Sigma(properties: SigmaObject, transforms: Transform) extends SigmaObject

  val operation: P[String] = P(CharIn("+", "-", "*", "/").!)
  val intValue: P[IntValue] = P(((CharIn('1' to '9') ~ CharIn('0' to '9').rep) | "0").!).map(int => IntValue(int.toInt))
  val realValue: P[RealValue] = P((intValue ~ "." ~ CharIn('0' to '9').rep(1)).!).map(real => RealValue(real.toDouble))
  val stringValue: P[StringValue] = P("'" ~ CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep(1).! ~ "'").map(StringValue)
  val string: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep(1).!)
  val inputValue: P[InputValue] = P(realValue | intValue | stringValue)
  val parameterValue: P[ParameterValue] = P(inputValue | string).map(ll)
  val parameter: P[Parameter] = P(parameterValue ~ call.?).map {
    case (param, call) => Parameter(param, call)
  }
  val function: P[Function] = P(parameter ~ operation ~ parameter).map {
    case (param1, op, param2) => Function(param1, op, param2)
  }
  val typeName: P[String] = P(StringIn("Int", "Real", "String", "Obj").!)
  val typ: P[Type] = P(":" ~ ("(" ~ typ ~ ")" | typeName) ~ ("->" ~ ("(" ~ typ ~ ")" | typeName)).rep).map {
    case (inner, rest) => inner +: rest match {
      case x: Seq[Either[Type, String]] => Type(x)
    }
  }
  val lambdaName: P[String] = P(CharIn(('a' to 'z') :+ '_').rep(1).!)
  val contextName: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep(1).!)
  val propertyName: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep(1).!)
  val lambdaArg: P[LambdaArg] = P(lambdaName ~ typ).map {
    case (name, t) => LambdaArg(name, t)
  }
  val lambda: P[Seq[LambdaArg]] = P("\\" ~ "(" ~ lambdaArg.rep(1) ~ ")")
  val lambdaFunction: P[Lambda] = P((lambda ~ "=>").rep(1) ~ expr).map {
    case (params, exp) => Lambda(params, exp)
  }
  val value: P[Value] = P(objectType | lambdaFunction | inputValue | expr).map(vv)
  val context: P[String] = P("@" ~ contextName)
  val field: P[Field] = P(propertyName ~ typ ~ ":=" ~ value).map {
    case (name, t, v) => Field(name, t, v)
  }
  val fieldUpdate: P[FieldUpdate] = P(contextName.? ~ "." ~ propertyName ~ typ ~ ":=" ~ value).map {
    case (cName, pName, typ, v) => FieldUpdate(cName, pName, typ, v)
  }
  val method: P[Method] = P(propertyName ~ typ ~ "=" ~ context ~ "=>" ~ methodBody).map {
    case (prop, t, ctx, body) => Method(prop, t, ctx, body)
  }
  val property: P[Property] = P(method | field)
  val objectType: P[ObjectType] = P("[" ~ property ~ ("," ~ property).rep ~ "]").map {
    case (head, tail) => ObjectType(head +: tail)
  }
  val exprWith: P[ExpressionWith] = P(fieldUpdate | methodUpdate | function | call | parameter).map(rr)
  val expr: P[Expression] = P(("(" ~ expr ~ ")").? ~ exprWith.rep(1)).map {
    case (inner, exp) => Expression(inner, exp)
  }
  val methodBody: P[MethodBody] = P(lambdaFunction | objectType | expr).map(ff)
  val methodUpdate: P[MethodUpdate] = P(contextName.? ~ "." ~ propertyName ~ typ ~ "<=" ~ context ~ "=>" ~ methodBody).map {
    case (cName, prop, typ, ctx, body) => MethodUpdate(cName, prop, typ, ctx, body)
  }
  val arguments: P[Seq[InputValue]] = P("(" ~ inputValue ~ ("," ~ inputValue).rep ~ ")").map {
    case (head, tail) => head +: tail
  }
  val call: P[Call] = P(contextName.? ~ "." ~ propertyName ~ arguments.rep).map {
    case (сName, prop, args) => Call(сName, prop, args)
  }
  val sigmaExpr: P[Sigma] = P((objectType | "(" ~ sigmaExpr ~ ")")  ~ (fieldUpdate | methodUpdate | call)).map {
    case (props, trans) => Sigma(props, trans)
  }
  val sigma: P[Sigma] = P(Start ~ sigmaExpr ~ End)
  try {
    println(function.parse("x + 2"))
    println(typ.parse(": Int -> (Int -> Int)"))
    println(lambdaFunction.parse("""\(x: Int) => \(y: Int) => x + y"""))
    println(field.parse("move_x: Int := 5"))
    println(expr.parse("(this.acc: Real := this.equals).equals: Real"))
    println(fieldUpdate.parse("this.x: Int := this.x + dx"))
    println(lambdaFunction.parse("""\(dx: Int) => this.x := this.x + dx"""))
    println(method.parse("""move_x: Int -> Int = @this => \(dx: Int) => this.x := this.x + dx"""))
    println(methodUpdate.parse("outer.move: Obj <= @this => [x: Int := 5]"))
    println(arguments.parse("('arg', 5)"))
    println(call.parse(".someFunction(ass, 5, 3.2)"))
    println(objectType.parse("[ move_x: Real := 5, move_y: Int := 5 ]"))
    println(sigma.parse("([x: Int := 0, move: Int -> Obj = @this => \\(dx: Int) => this.x: Int := this.x + dx].move(5)).x"))
    println(sigma.parse("""(([arg: Real := 0.0, acc: Real := 0.0, clear: Obj = @this => ((this.arg: Real := 0.0).acc: Real := 0.0).equals: Real <= @self => self.arg, enter: Real -> Obj = @this => \(n: Real) => this.arg: Real := n, add: Obj = @this => (this.acc: Real := this.equals).equals: Real <= @self => self.acc + self.arg, sub: Obj = @this => (this.acc: Real := this.equals).equals: Real <= @self => self.acc - self.arg, equals: Real = @this => this.arg].enter(5.0)).add).equals"""))
    println(sigma.parse("(((((([retrieve: Obj = @s => s, backup: Obj = @b => b.retrive: Obj <= @b => b, value: Int := 10].backup).value: Int := 15).backup).value: Int := 25).retrieve).retrieve).value"))
    println(sigma.parse("""[zero: Obj = @global => [succ: Obj = @this => ((this.ifzero: Obj := global.false).pred: Obj := this).num: Int := this.num + 1, ifzero: Obj := global.true, num: Int := 0], true: Obj = @global => [then: Obj = @this => this, val: Obj = @this => this.then], false: Obj = @global => [else: Obj = @this => this, val: Obj = @this => this.else], prog: Int = @global => global.zero.succ.succ.succ.pred.num].prog"""))
/*    println(sigma.parse("""[numeral: Obj = @top => [zero: Obj = @numeral => [case: Obj -> Obj -> Obj = @zero => \(z: Obj) => \(s: Obj) => z, succ: Obj = @zero => (zero.case: Obj -> Obj -> Obj <= @tt => \(z: Obj) => \(s: Obj) => s.zero).val: Int := zero.val + 1, val: Int := 0, pred = @this => this.case(numeral.zero)((\x: Obj -> Obj) => x), add: Obj -> Int = @this => \(that: Obj) => this.case(that)(\(x: Obj) => x.add(that.succ))
                          |
                          |        ],
                          |
                          |        fib = @ numeral => \ n => n.case (numeral.zero) (\ x => x.case (n) (
                          |
                          |            \y => (numeral.fib x).add (numeral.fib y)))
                          |
                          |    ],
                          |
                          |    main = @ top => ((top.numeral.fib) (top.numeral.zero.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ)).val
                          |
                          |].main"""))*/
    println(sigma.parse("([a: Int := 0].a: Int := 5).a"))
  } catch {
    case e: ParseError => println(e)
  }
}
