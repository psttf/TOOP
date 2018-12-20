package typed

import SigmaTypedParser._
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TypedCalculator {

  type ReturnType = Int :+: Double :+: ObjectType :+: CNil
  implicit def resultType(x: Any): ReturnType = x match {
    case a: Int    => Coproduct[ReturnType](a)
    case a: Double    => Coproduct[ReturnType](a)
    case a: ObjectType => Coproduct[ReturnType](a)
  }

  type Numeric = Int :+: Double :+: CNil
  implicit def numericType(x: Any): Numeric = x match {
    case a: Int    => Coproduct[Numeric](a)
    case a: Double => Coproduct[Numeric](a)
  }

  type ContextType =
    String :+: ObjectType :+: String :+: Boolean :+: Int :+: Double :+: Call :+: CNil
  implicit def contextType(x: Any): ContextType = x match {
    case a: String     => Coproduct[ContextType](a)
    case a: Int        => Coproduct[ContextType](a)
    case a: Double     => Coproduct[ContextType](a)
    case a: ObjectType => Coproduct[ContextType](a)
    case a: Boolean    => Coproduct[ContextType](a)
    case a: Call       => Coproduct[ContextType](a)
  }

  val context: mutable.HashMap[String, ArrayBuffer[ContextType]] =
    mutable.HashMap.empty

  private def findMethod(
    objectType: ObjectType,
    name: String
  ): Option[Property] = {
    objectType.props.find(prop => prop.name == name)
  }

  private def setMethod(ctx: ObjectType, newProp: Property): ObjectType = {
    val updatedProps = ctx.props.map {
      case prop if prop.name == newProp.name => newProp
      case prop                              => prop
    }
    ObjectType(updatedProps)
  }

  private def getNewMethod(method: Method, ctx: ObjectType): Method = {
    method.methodBody match {
      case e: Expression =>
        val body = evalExprBodies(e.args)
        body match {
          case Inr(Inr(Inl(ob))) =>
            Method(method.name, method.methodType, method.contextName, ob)
          case _ => sys.error("ObjectType expected!")
        }
      case _: Lambda =>
        Method(method.name, method.methodType, ctx, method.methodBody)
      case _ => method
    }
  }

  private def numericFromReturn(value: ReturnType): Numeric = {
    value match {
      case Inl(head) => head
      case _         => 0
    }
  }

  private def inputValueFromReturn(body: ReturnType): InputValue = {
    body match {
      case Inl(int) => IntValue(int)
      case Inr(Inl(real)) => RealValue(real)
      case Inr(Inr(_)) => sys.error("Not implemented now")
    }
  }

  private def evalExpr(expression: Expression): ReturnType = {
    13
  }

  private def evalExprBodies(body: Seq[ExpressionBody]): ReturnType = {
    body.takeRight(1).headOption.fold(0)(body => evalExprBody(body).select.head)
  }

  private def evalExprBody(body: ExpressionBody): ReturnType = {
    23
  }

  private def evalParam(param: Parameter): ReturnType = {
    if (param.methodCall.isDefined) {
      return methodCall(param.methodCall.get)
    }
    val arr: ArrayBuffer[ContextType] = context.getOrElse(
      param.param.select.head.asInstanceOf[String],
      sys.error(s"No param ${param.param} in context")
    )
    arr.remove(arr.length - 1)
  }

  private def evalLambda(
    body: Lambda,
    args: Option[Vector[Argument]]
  ): ReturnType = {
    args.foreach(vec => {
      body.params.zipWithIndex.foreach {
        case (param, index) =>
          val arg: Option[Argument] = vec.lift(index)
          if (arg.isDefined) {
            addToContext(param.argName, arg.get)
          }
      }
    })
    evalExpr(body.body)
  }

  private def evalBody(
    body: Value,
    args: Option[Vector[Argument]]
  ): ReturnType = {
    body match {
      case l: Lambda      => evalLambda(l, args)
      case e: Expression  => evalExpr(e)
      case ot: ObjectType => ot
      case i: IntValue    => i.int
      case r: RealValue   => r.real
    }
  }

  private def methodCall(methodCall: Call): ReturnType = {
    3
  }

  private def evalSigma(
    obj: SigmaObject,
    parentCall: CutExpression
  ): ReturnType = {
    obj match {
      case o @ ObjectType(_) => addToContext("_", o)
      case Sigma(properties, sigmaCall) =>
        sigmaCall.map(sc => {
          val ctx = evalSigma(properties, sc)
          val key: String = context
            .get("_default")
            .fold("_")(vec => vec.remove(vec.length - 1).select.head)
          addToContext(key, ctx.select.head)
        })
    }
    resultFromCall(parentCall)
  }

  private def addToContext(key: String, o: ContextType) = {
    context += (key -> (context.getOrElse(key, ArrayBuffer.empty) += o))
  }

  private def resultFromCall(call: CutExpression): ReturnType = {
    call match {
      case x @ Call(_, _, _) => methodCall(x)
      case _                 => evalExprBody(call)
    }
  }

  def evalMain(sigma: Sigma): String = {
    val res = sigma.call.fold(sigma.toString) { sigmaCall =>
      {
        val ctx = evalSigma(sigma.properties, sigmaCall)
        context += ("_" -> ArrayBuffer(ctx.select.head))
        resultFromCall(sigmaCall).select.head.toString
      }
    }
    println(context)
    println(res)
    res
  }

}
