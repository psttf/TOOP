package typed

import fastparse.noApi._
import shapeless.{:+:, CNil, Coproduct}

object SigmaTypedParser extends App {
  import SigmaApi._

  type Expression = FieldUpdate :+: MethodUpdate :+: Function :+: MethodBodyWithCall :+: CNil

  implicit def rr(x: Product): Expression = x match {
    case a: FieldUpdate => Coproduct[Expression](a)
    case a: MethodUpdate => Coproduct[Expression](a)
    case a: MethodBodyWithCall => Coproduct[Expression](a)
    case a: Function => Coproduct[Expression](a)
  }

  type MethodBody = Lambda :+: ObjectType :+: Expression :+: CNil

  implicit def ff(x: Product): MethodBody = x match {
    case a: Lambda => Coproduct[MethodBody](a)
    case a: ObjectType => Coproduct[MethodBody](a)
    case a: Expression => Coproduct[MethodBody](a)
  }

  final case class Parameter(param: String, methodCall: Option[MethodCall])
  final case class Function(param1: Parameter, operator: String, param2: Parameter)
  final case class Lambda(params: Seq[String], func: Function)

  sealed trait Property
  final case class Field(name: String, typ: String, value: String) extends Property
  final case class Method(methodName: String, methodType: String, contextName: String, methodBody: MethodBody) extends Property
  final case class FieldUpdate(contextName: String, propertyName: String, value: String)
  sealed trait MethodTransform
  final case class MethodUpdate(propertyName: String, contextName: String, body: MethodBody) extends MethodTransform
  final case class MethodCall(propertyName: String, arguments: Seq[String]) extends MethodTransform
  final case class MethodBodyWithCall(body: MethodBody, call: MethodCall)
  final case class ObjectType(props: Seq[Property])
  final case class Sigma(properties: ObjectType, transforms: Seq[MethodTransform])

  val operation: P[String] = P(CharIn("+", "-", "*", "/").!)
  val intValue: P[String] = P(((CharIn('1' to '9') ~ CharIn('0' to '9').rep.?) | "0").!)
  val realValue: P[String] = P((intValue ~ "." ~ CharIn('0' to '9').rep).!)
  val stringValue: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep.!)
  val parameter: P[Parameter] = P((realValue | intValue | stringValue) ~ methodCall.?).map {
    case (param, call) => Parameter(param, call)
  }
  val function: P[Function] = P(parameter ~ operation ~ parameter).map {
    case (param1, op, param2) => Function(param1, op, param2)
  }
  val typeName: P[String] = P(StringIn("Int", "Real", "Obj").!)
  val methodType: P[String] = P((typeName ~ ("->" ~ typeName).rep.?).!)
  val fieldType: P[String] = P(typeName)
  val lambdaName: P[String] = P(CharIn(('a' to 'z') :+ '_').rep.!)
  val contextName: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep.!)
  val propertyName: P[String] = P(CharIn(('a' to 'z') ++ ('A' to 'Z') :+ '_').rep.!)
  val argument: P[String] = P(CharIn(('a' to 'z') :+ '_').rep.!)
  val lambda: P[String] = P("\\" ~ lambdaName.!)
  val lambdaFunction: P[Lambda] = P((lambda ~ "=>").rep ~ function).map {
    case (params, func) => Lambda(params, func)
  }
  val context: P[String] = P("@" ~ contextName)
  val value: P[String] = P(intValue | realValue)
  val field: P[Field] = P(propertyName ~ ":" ~ fieldType ~ ":=" ~ value).map {
    case (name, typ, v) => Field(name, typ, v)
  }
  val fieldUpdate: P[FieldUpdate] = P(context ~ "." ~ propertyName ~ ":=" ~ value).map {
    case (cName, pName, v) => FieldUpdate(cName, pName, v)
  }
  val method: P[Method] = P(propertyName ~ ":" ~ methodType ~ "=" ~ context ~ "=>" ~ methodBody).map {
    case (prop, typ, ctx, body) => Method(prop, typ, ctx, body)
  }
  val property: P[Property] = P(method | field)
  val objectType: P[ObjectType] = P("[" ~ property ~ ("," ~ property).rep.? ~ "]").map {
    case (head, tail) => ObjectType(head +: tail.toSeq.flatten)
  }
  val methodBodyWithCall: P[MethodBodyWithCall] = P("(" ~ methodBody ~ ")" ~ methodCall).map {
    case (body, call) => MethodBodyWithCall(body, call)
  }
  val expr: P[Expression] = P(fieldUpdate | methodUpdate | function | methodBodyWithCall).map(rr)
  val methodBody: P[MethodBody] = P(lambdaFunction | objectType | expr).map(ff)
  val methodUpdate: P[MethodUpdate] = P("." ~ propertyName ~ "<=" ~ context ~ "=>" ~ methodBody).map {
    case (prop, ctx, body) => MethodUpdate(prop, ctx, body)
  }
  val arguments: P[Seq[String]] = P("(" ~ argument ~ ("," ~ argument).rep.? ~ ")").map {
    case (head, tail) => head +: tail.toSeq.flatten
  }
  val update: P[(String, MethodBody)] = P("<=" ~ context ~ "=>" ~ methodBody)
  val methodCall: P[MethodCall] = P("." ~ propertyName ~ arguments.?).map {
    case (prop, args) => MethodCall(prop, args.toSeq.flatten)
  }
  val sigma = P(objectType ~ (methodCall | methodUpdate).rep).map {
    case (props, trans) => Sigma(props, trans)
  }

  println(function.parse("x + 2"))
  println(methodType.parse("Int -> Int -> []"))
  println(lambdaFunction.parse("""\x => \y => x + y"""))
  println(field.parse("move_x: Int := 5"))
  println(fieldUpdate.parse("@this.move_x := 3"))
  println(expr.parse("3 + 5"))
  println(methodBody.parse("(3 + 3.2).y"))
  println(method.parse("move_x: Real = @this => \\x => x - 1"))
  println(methodUpdate.parse(".move <= @this => [x: Int := 5]"))
  println(arguments.parse("(ass)"))
  println(methodCall.parse(".someFunction(ass)"))
  println(objectType.parse("[ move_x: Real := 5, move_y: Int := 5 ]"))
  println(sigma.parse("[x: Int := 0, y: Int := 0, move: Int -> Int = @this => \\d => d + 3].x"))

}
