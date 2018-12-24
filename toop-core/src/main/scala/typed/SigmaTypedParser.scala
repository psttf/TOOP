package typed

import fastparse.noApi._
import shapeless.{:+:, CNil, Coproduct}

object SigmaTypedParser extends App {
  import SigmaApi._

/*  type ExpressionBody = FieldUpdate :+: MethodUpdate :+: Function :+: Parameter :+: Call :+: CNil

  implicit def rr(x: Product): ExpressionBody = x match {
    case a: FieldUpdate => Coproduct[ExpressionBody](a)
    case a: MethodUpdate => Coproduct[ExpressionBody](a)
    case a: Function => Coproduct[ExpressionBody](a)
    case a: Parameter => Coproduct[ExpressionBody](a)
    case a: Call => Coproduct[ExpressionBody](a)
  }*/

  sealed trait ExpressionBody
  sealed trait CutExpression extends ExpressionBody
  sealed trait Body extends Value
  sealed trait Value
  sealed trait Argument

/*  type CutExpression = Call :+: MethodUpdate :+: FieldUpdate :+: CNil*/

/*  implicit def cc(x: Product): CutExpression = x match {
    case a: FieldUpdate => Coproduct[CutExpression](a)
    case a: MethodUpdate => Coproduct[CutExpression](a)
    case a: Call => Coproduct[CutExpression](a)
  }*/

/*  type Body = Lambda :+: ObjectType :+: Expression :+: CNil

  implicit def ff(x: Product): Body = x match {
    case a: Lambda => Coproduct[Body](a)
    case a: ObjectType => Coproduct[Body](a)
    case a: Expression => Coproduct[Body](a)
  }*/

/*  type Value = Body :+: InputValue :+: CNil

  implicit def vv(x: Object): Value = x match {
    case a: Body => Coproduct[Value](a)
    case a: InputValue => Coproduct[Value](a)
  }*/

  type ParameterValue = InputValue :+: String :+: CNil

  implicit def ll(x: Object): ParameterValue = x match {
    case a: InputValue => Coproduct[ParameterValue](a)
    case a: String => Coproduct[ParameterValue](a)
  }

  type MethodCtxType = ObjectType :+: String :+: CNil

  implicit def cc(x: Object): MethodCtxType = x match {
    case a: ObjectType => Coproduct[MethodCtxType](a)
    case a: String => Coproduct[MethodCtxType](a)
  }

/*  type Argument = Lambda :+: InputValue :+: Expression :+: CNil

  implicit def aa(x: Object): Argument = x match {
    case a: Lambda => Coproduct[Argument](a)
    case a: InputValue => Coproduct[Argument](a)
    case a: Expression => Coproduct[Argument](a)
  }*/

  sealed trait Operation
  final case class Add() extends Operation
  final case class Substitute() extends Operation
  final case class Multiply() extends Operation
  final case class Divide() extends Operation

  final case class Parameter(value: ParameterValue, methodCall: Option[Call]) extends ExpressionBody
  final case class Function(param1: Parameter, operator: Operation, param2: Parameter) extends ExpressionBody

  final case class Type(args: Seq[Either[Type, String]])

  sealed trait InputValue extends Value with Argument
  final case class IntValue(int: Int) extends InputValue
  final case class RealValue(real: Double) extends InputValue

  final case class LambdaArg(argName: String, argType: Type)
  final case class Lambda(params: Seq[LambdaArg], body: Expression) extends Body with Argument

  sealed trait Property extends { val name: String; val typ: Type }
  final case class Field(name: String, typ: Type, value: Value) extends Property
  final case class Method(name: String, typ: Type, context: MethodCtxType, methodBody: Body) extends Property

  final case class FieldUpdate(contextName: Option[String], propertyName: String, typ: Type, value: Value) extends CutExpression
  final case class MethodUpdate(oldContextName: Option[String], propertyName: String, typ: Type, newContextName: String, body: Body) extends CutExpression
  final case class Call(contextName: Option[String], propertyName: String, arguments: Option[Seq[Argument]]) extends CutExpression

  final case class Expression(innerExpr: Option[Expression], args: Seq[ExpressionBody]) extends Body with Argument

  sealed trait SigmaObject
  final case class ObjectType(props: Seq[Property]) extends SigmaObject with Body
  final case class Sigma(properties: SigmaObject, call: Option[CutExpression]) extends SigmaObject

  val addOperation: P[Add] = P("+".!).map(_ => Add())
  val substituteOperation: P[Substitute] = P("-".!).map(_ => Substitute())
  val multiplyOperation: P[Multiply] = P("*".!).map(_ => Multiply())
  val divideOperation: P[Divide] = P("/".!).map(_ => Divide())
  val operation: P[Operation] = P(addOperation | substituteOperation | multiplyOperation | divideOperation)
  val intValue: P[IntValue] = P(("-".? ~ (CharIn('1' to '9') ~ CharIn('0' to '9').rep) | "0").!).map(int => IntValue(int.toInt))
  val realValue: P[RealValue] = P((intValue ~ "." ~ CharIn('0' to '9').rep(1)).!).map(real => RealValue(real.toDouble))
  val string: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep(1).!)
  val inputValue: P[InputValue] = P(realValue | intValue)
  val parameterValue: P[ParameterValue] = P(inputValue | string).map(ll)
  val parameter: P[Parameter] = P(parameterValue ~ call.?).map {
    case (param, c) => Parameter(param, c)
  }
  val function: P[Function] = P(parameter ~ operation ~ parameter).map {
    case (param1, op, param2) => Function(param1, op, param2)
  }
  val typeName: P[String] = P(StringIn("Int", "Real", "Obj").!)
  val typ: P[Type] = P(("(" ~ typ ~ ")" | typeName) ~ ("->" ~ ("(" ~ typ ~ ")" | typeName)).rep).map {
    case (inner, rest) => inner +: rest match {
      case x: Seq[Either[Type, String]] => Type(x)
    }
  }
  val lambdaName: P[String] = P(CharIn(('a' to 'z') :+ '_').rep(1).!)
  val contextName: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep(1).!)
  val propertyName: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep(1).!)
  val lambdaArg: P[LambdaArg] = P(lambdaName ~ ":" ~ typ).map {
    case (name, t) => LambdaArg(name, t)
  }
  val lambda: P[LambdaArg] = P("\\" ~ "(" ~ lambdaArg ~ ")")
  val lambdaFunction: P[Lambda] = P((lambda ~ "=>").rep(1) ~ expr).map {
    case (params, exp) => Lambda(params, exp)
  }
  val value: P[Value] = P(inputValue | body)
  val context: P[String] = P("@" ~ contextName)
  val field: P[Field] = P(propertyName ~ ":" ~ typ ~ ":=" ~ value).map {
    case (name, t, v) => Field(name, t, v)
  }
  val fieldUpdate: P[FieldUpdate] = P(contextName.? ~ "." ~ propertyName ~ ":" ~ typ ~ ":=" ~ value).map {
    case (cName, pName, t, v) => FieldUpdate(cName, pName, t, v)
  }
  val method: P[Method] = P(propertyName ~ ":" ~ typ ~ "=" ~ context ~ "=>" ~ body).map {
    case (prop, t, ctx, b) => Method(prop, t, ctx, b)
  }
  val property: P[Property] = P(method | field)
  val objectType: P[ObjectType] = P("[" ~ property ~ ("," ~ property).rep ~ "]").map {
    case (head, tail) => ObjectType(head +: tail)
  }
  val exprBody: P[ExpressionBody] = P(fieldUpdate | methodUpdate | function | call | parameter)
  val expr: P[Expression] = P(("(" ~ expr ~ ")").? ~ exprBody.rep(1)).map {
    case (inner, exp) => Expression(inner, exp)
  }
  val cutExpr: P[CutExpression] = P(fieldUpdate | methodUpdate | call)
  val body: P[Body] = P(lambdaFunction | objectType | expr)
  val methodUpdate: P[MethodUpdate] = P(contextName.? ~ "." ~ propertyName ~ ":" ~ typ ~ "<=" ~ context ~ "=>" ~ body).map {
    case (cName, prop, t, ctx, b) => MethodUpdate(cName, prop, t, ctx, b)
  }
  val argument: P[Argument] = P(lambdaFunction | inputValue | expr)
  val arguments: P[Seq[Argument]] = P("(" ~ argument ~ ("," ~ argument).rep ~ ")").map {
    case (head, tail) => head +: tail
  }
  val call: P[Call] = P(contextName.? ~ "." ~ propertyName ~ arguments.?).map {
    case (сName, prop, args) => Call(сName, prop, args)
  }
  val sigmaExpr: P[Sigma] = P((objectType | "(" ~ sigmaExpr ~ ")")  ~ cutExpr.?).map {
    case (props, trans) => Sigma(props, trans)
  }
  val sigma: P[Sigma] = P(Start ~ sigmaExpr ~ End)


  val sigma1 = sigma.parse("(([x: Int := 0, move: Int -> Obj = @this => \\(dx: Int) => this.x: Int := this.x + dx].move(5)).move(-3)).x")
  val sigma2 = sigma.parse("""(([arg: Real := 0.0, acc: Real := 0.0, clear: Obj = @this => ((this.arg: Real := 0.0).acc: Real := 0.0).equals: Real <= @self => self.arg, enter: Real -> Obj = @this => \(n: Real) => this.arg: Real := n, add: Obj = @this => (this.acc: Real := this.equals).equals: Real <= @self => self.acc + self.arg, sub: Obj = @this => (this.acc: Real := this.equals).equals: Real <= @self => self.acc - self.arg, equals: Real = @this => this.arg].enter(5.0)).add).equals""")

  try {
    println(function.parse("x + 2"))
    println(typ.parse("Int -> (Int -> Int)"))
    println(lambdaFunction.parse("""\(x: Int) => \(y: Int) => x + y"""))
    println(field.parse("move_x: Int := 5"))
    println(expr.parse("(this.acc: Real := this.equals).equals"))
    println(expr.parse("((this.ifzero: Obj := global.false).pred: Obj := this).num: Int := this.num + 1"))
    println(fieldUpdate.parse("this.x: Int := this.x + dx"))
    println(lambdaFunction.parse("""\(dx: Int) => this.x := this.x + dx"""))
    println(method.parse("""move_x: Int -> Int = @this => \(dx: Int) => this.x := this.x + dx"""))
    println(methodUpdate.parse("outer.move: Obj <= @this => [x: Int := 5]"))
    println(arguments.parse("(-234.321, 5)"))
    println(call.parse(".someFunction(ass, 5, 3.2)"))
    println(objectType.parse("[ move_x: Real := 5, move_y: Int := 5 ]"))
    println(sigma1)
    println(sigma.parse("""(([arg: Real := 0.0, acc: Real := 0.0, clear: Obj = @this => ((this.arg: Real := 0.0).acc: Real := 0.0).equals: Real <= @self => self.arg, enter: Real -> Obj = @this => \(n: Real) => this.arg: Real := n, add: Obj = @this => (this.acc: Real := this.equals).equals: Real <= @self => self.acc + self.arg, sub: Obj = @this => (this.acc: Real := this.equals).equals: Real <= @self => self.acc - self.arg, equals: Real = @this => this.arg].enter(5.0)).add).equals"""))
    println(sigma.parse("(((((([retrieve: Obj = @s => s, backup: Obj = @b => b.retrive: Obj <= @b => b, value: Int := 10].backup).value: Int := 15).backup).value: Int := 25).retrieve).retrieve).value"))
    println(sigma.parse("""[zero: Obj = @global => [succ: Obj = @this => ((this.ifzero: Obj := global.false).pred: Obj := this).num: Int := this.num + 1, ifzero: Obj := global.true, num: Int := 0], true: Obj = @global => [then: Obj = @this => this, val: Obj = @this => this.then], false: Obj = @global => [else: Obj = @this => this, val: Obj = @this => this.else], prog: Int = @global => global.zero.succ.succ.succ.pred.num].prog"""))
    println(sigma.parse("""[numeral: Obj = @top => [zero: Obj = @numeral => [case: Obj -> Obj -> Obj = @zero => \(z: Obj) => \(s: Obj) => z, succ: Obj = @zero => (zero.case: Obj -> Obj -> Obj <= @tt => \(z: Obj) => \(s: Obj) => s.zero).val: Int := zero.val + 1, val: Int := 0, pred: Obj = @this => this.case(numeral.zero, \(x: Obj -> Obj) => x), add: Obj -> Int = @this => \(that: Obj) => this.case(that, \(x: Obj) => x.add(that.succ))], fib: Obj = @ numeral => \(n: Obj) => n.case(numeral.zero, \(x: Obj) => x.case(n, \(y: Obj) => (numeral.fib(x)).add(numeral.fib(y))))], main: Int = @ top => (top.numeral.fib(top.numeral.zero.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ)).val].main"""))
    println(sigma.parse("([a: Int := 0].a: Int := 5).a"))
    println(sigma.parse("[a: Int := 0]"))
  } catch {
    case e: ParseError => println(e)
  }

  println("__________")
  println("Calculation")

/*  new TypedCalculator().evalMain(sigma1.get.value)*/
  /*new TypedCalculator().evalMain(sigma2.get.value)*/

}
