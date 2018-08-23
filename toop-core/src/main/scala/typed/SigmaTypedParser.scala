package typed

import fastparse.noApi._
import shapeless.{:+:, CNil, Coproduct}

object SigmaTypedParser extends App {
  import SigmaApi._

  type ExpressionWith = FieldUpdate :+: MethodUpdate :+: Function :+: MethodBodyWithCall :+: MethodCall :+: CNil

  implicit def rr(x: Product): ExpressionWith = x match {
    case a: FieldUpdate => Coproduct[ExpressionWith](a)
    case a: MethodUpdate => Coproduct[ExpressionWith](a)
    case a: MethodBodyWithCall => Coproduct[ExpressionWith](a)
    case a: Function => Coproduct[ExpressionWith](a)
    case a: MethodCall => Coproduct[ExpressionWith](a)
  }

  type MethodBody = Lambda :+: ObjectType :+: Expression :+: CNil

  implicit def ff(x: Product): MethodBody = x match {
    case a: Lambda => Coproduct[MethodBody](a)
    case a: ObjectType => Coproduct[MethodBody](a)
    case a: Expression => Coproduct[MethodBody](a)
  }

  type Value = String :+: ObjectType :+: Expression :+: CNil

  implicit def vv(x: java.io.Serializable): Value = x match {
    case a: String => Coproduct[Value](a)
    case a: ObjectType => Coproduct[Value](a)
    case a: Expression => Coproduct[Value](a)
  }

  final case class Parameter(param: String, methodCall: Option[MethodCall])
  final case class Function(param1: Parameter, operator: String, param2: Parameter)
  final case class Lambda(params: Seq[String], expression: Expression)

  sealed trait Property
  final case class Field(name: String, typ: String, value: Value) extends Property
  final case class Method(methodName: String, methodType: String, contextName: String, methodBody: MethodBody) extends Property
  final case class FieldUpdate(contextName: Option[String], propertyName: String, value: Value)
  sealed trait MethodTransform
  final case class MethodUpdate(oldContextName: Option[String], propertyName: String, newContextName: String, body: MethodBody) extends MethodTransform
  final case class MethodCall(contextName: Option[String], propertyName: String, arguments: Seq[String]) extends MethodTransform
  final case class MethodBodyWithCall(body: MethodBody, call: MethodCall)
  final case class Expression(innerExpr: Option[Expression], exprWith: ExpressionWith)
  final case class ObjectType(props: Seq[Property])
  final case class Sigma(properties: ObjectType, transforms: Seq[MethodTransform])

  val operation: P[String] = P(CharIn("+", "-", "*", "/").!)
  val intValue: P[String] = P(((CharIn('1' to '9') ~ CharIn('0' to '9').rep) | "0").!)
  val realValue: P[String] = P((intValue ~ "." ~ CharIn('0' to '9').rep(1)).!)
  val stringValue: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep(1).!)
  val parameter: P[Parameter] = P((realValue | intValue | stringValue) ~ methodCall.?).map {
    case (param, call) => Parameter(param, call)
  }
  val function: P[Function] = P(parameter ~ operation ~ parameter).map {
    case (param1, op, param2) => Function(param1, op, param2)
  }
  val typeName: P[String] = P(StringIn("Int", "Real", "Obj").!)
  val methodType: P[String] = P((typeName ~ ("->" ~ typeName).rep).!)
  val fieldType: P[String] = P(typeName)
  val lambdaName: P[String] = P(CharIn(('a' to 'z') :+ '_').rep(1).!)
  val contextName: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep(1).!)
  val propertyName: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep(1).!)
  val argument: P[String] = P(realValue | intValue | stringValue)
  val lambda: P[String] = P("\\" ~ lambdaName.!)
  val lambdaFunction: P[Lambda] = P((lambda ~ "=>").rep(1) ~ expr).map {
    case (params, exp) => Lambda(params, exp)
  }
  val context: P[String] = P("@" ~ contextName)
  val value: P[Value] = P(realValue | intValue | objectType | expr | stringValue).map(vv)
  val field: P[Field] = P(propertyName ~ ":" ~ fieldType ~ ":=" ~ value).map {
    case (name, typ, v) => Field(name, typ, v)
  }
  val fieldUpdate: P[FieldUpdate] = P(contextName.? ~ "." ~ propertyName ~ ":=" ~ value).map {
    case (cName, pName, v) => FieldUpdate(cName, pName, v)
  }
  val method: P[Method] = P(propertyName ~ ":" ~ methodType ~ "=" ~ context ~ "=>" ~ methodBody).map {
    case (prop, typ, ctx, body) => Method(prop, typ, ctx, body)
  }
  val property: P[Property] = P(method | field)
  val objectType: P[ObjectType] = P("[" ~ property ~ ("," ~ property).rep ~ "]").map {
    case (head, tail) => ObjectType(head +: tail)
  }
  val methodBodyWithCall: P[MethodBodyWithCall] = P("(" ~ methodBody ~ ")" ~ methodCall).map {
    case (body, call) => MethodBodyWithCall(body, call)
  }
  val exprWith: P[ExpressionWith] = P(fieldUpdate | methodUpdate | function | methodBodyWithCall | methodCall).map(rr)
  val expr: P[Expression] = P(("(" ~ expr ~ ")").? ~ exprWith).map {
    case (inner, exp) => Expression(inner, exp)
  }
  val methodBody: P[MethodBody] = P(lambdaFunction | objectType | expr).map(ff)
  val methodUpdate: P[MethodUpdate] = P(contextName.? ~ "." ~ propertyName ~ "<=" ~ context ~ "=>" ~ methodBody).map {
    case (cName, prop, ctx, body) => MethodUpdate(cName, prop, ctx, body)
  }
  val arguments: P[Seq[String]] = P("(" ~ argument ~ ("," ~ argument).rep ~ ")").map {
    case (head, tail) => head +: tail
  }
  val update: P[(String, MethodBody)] = P("<=" ~ context ~ "=>" ~ methodBody)
  val methodCall: P[MethodCall] = P(contextName.? ~ "." ~ propertyName ~ arguments.?).map {
    case (сName, prop, args) => MethodCall(сName, prop, args.toSeq.flatten)
  }
  val sigma = P(objectType ~ (methodCall | methodUpdate).rep).map {
    case (props, trans) => Sigma(props, trans)
  }

  println(function.parse("x + 2"))
  println(methodType.parse("Int -> Int -> []"))
  println(lambdaFunction.parse("""\x => \y => x + y"""))
  println(field.parse("move_x: Int := 5"))
  println(expr.parse("3 + 5"))
  println(fieldUpdate.parse("this.x := this.x + dx"))
  println(lambdaFunction.parse("""\dx => this.x := this.x + dx"""))
  println(method.parse("""move_x: Int -> Int = @this => \dx => this.x := this.x + dx"""))
  println(methodUpdate.parse("outer.move <= @this => [x: Int := 5]"))
  println(arguments.parse("(ass, 5)"))
  println(methodCall.parse(".someFunction(ass, 5, 3.2)"))
  println(objectType.parse("[ move_x: Real := 5, move_y: Int := 5 ]"))
  println(sigma.parse("[x: Int := 0, move: Int -> Obj = @this => \\dx => this.x := this.x + dx].move(5).x"))
  println(sigma.parse("""[arg: Real := 0.0, acc: Real := 0.0, clear: Obj = @this => ((this.arg := 0.0).acc := 0.0).equals <= @self => self.arg, enter: Real -> Obj = @this => \n => this.arg := n, add: Obj = @this => (this.acc := this.equals).equals <= @self => self.acc + self.arg, sub: Obj = @this => (this.acc := this.equals).equals <= @self => self.acc + self.arg, equals: Real = @this => this.arg].enter(5.0).add.equals"""))

}
