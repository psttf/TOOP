package typed

import SigmaTypedParser._
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TypedCalculator {

  type ReturnType = Int :+: Double :+: SigmaBody :+: CNil
  implicit def resultType(x: Any): ReturnType = x match {
    case a: Int        => Coproduct[ReturnType](a)
    case a: Double     => Coproduct[ReturnType](a)
    case a: SigmaBody => Coproduct[ReturnType](a)
  }

  type Numeric = Int :+: Double :+: CNil
  implicit def numericType(x: Any): Numeric = x match {
    case a: Int    => Coproduct[Numeric](a)
    case a: Double => Coproduct[Numeric](a)
  }

  type ContextType =
    String :+: SigmaBody :+: String :+: Boolean :+: Int :+: Double :+: IntValue :+: RealValue :+: Call :+: CNil
  implicit def contextType(x: Any): ContextType = x match {
    case a: String     => Coproduct[ContextType](a)
    case a: Int        => Coproduct[ContextType](a)
    case a: Double     => Coproduct[ContextType](a)
    case a: SigmaBody => Coproduct[ContextType](a)
    case a: Boolean    => Coproduct[ContextType](a)
    case a: IntValue   => Coproduct[ContextType](a)
    case a: RealValue  => Coproduct[ContextType](a)
    case a: Call       => Coproduct[ContextType](a)
  }

  val context: mutable.HashMap[String, ArrayBuffer[ContextType]] =
    mutable.HashMap.empty

  private def findProperty(
                            objectType: SigmaBody,
                            name: String
  ): Option[Property] = {
    objectType.props.find(prop => prop.name == name)
  }

  private def setProperty(ctx: SigmaBody, newProp: Property): SigmaBody = {
    val updatedProps = ctx.props.map {
      case prop if prop.name == newProp.name => newProp
      case prop                              => prop
    }
    SigmaBody(updatedProps)
  }

  private def getNewMethod(method: Method, ctx: SigmaBody): Method = {
    method.methodBody match {
      case e: Expression =>
        val body = evalExprBodies(e.args)
        body match {
          case Inr(Inr(Inl(ob))) =>
            Method(method.name, method.typ, method.context, ob)
          case _ => sys.error("ObjectType expected!")
        }
      case _: Lambda =>
        Method(method.name, method.typ, ctx, method.methodBody)
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
      case Inl(int)       => IntValue(int)
      case Inr(Inl(real)) => RealValue(real)
      case Inr(Inr(_))    => sys.error("Not implemented now")
    }
  }

  private def returnTypeToContextType(ret: ReturnType): ContextType =
    ret match {
      case Inl(int)          => Coproduct[ContextType](int)
      case Inr(Inl(d))       => Coproduct[ContextType](d)
      case Inr(Inr(Inl(ob))) => Coproduct[ContextType](ob)
    }

  private def evalExpr(expression: Expression): ReturnType = {
    val ctx = evalExprBodies(expression.args)
    val key = context
      .get("_default")
      .fold(Coproduct[ContextType]("_"))(vec => vec.last)
    addToContext(key.select[String].head, returnTypeToContextType(ctx))
    expression.innerExpr.fold(ctx)(inner => evalExpr(inner))
  }

  private def evalExprBodies(body: Seq[ExpressionBody]): ReturnType = {
    body
      .takeRight(1)
      .headOption
      .fold(Coproduct[ReturnType](0))(body => evalExprBody(body))
  }

  private def evalExprBody(body: ExpressionBody): ReturnType = {
    body match {
      case fu: FieldUpdate =>
        val ctx = context(fu.contextName.getOrElse("_")).last
        ctx match {
          case Inr(Inl(ob)) =>
            val method: Property = findProperty(ob, fu.propertyName)
              .getOrElse(sys.error(s"Field ${fu.propertyName} can't be found"))
            println(Type(Seq(Right("sss"))))
            println(fu.typ)
            if (method.typ.args != fu.typ.args)
              sys.error(s"Type ${method.typ.args} does not equal to ${fu.typ.args} type")
            val newValue = fu.value match {
              case e: Expression => inputValueFromReturn(evalExpr(e))
              case _             => IntValue(0)
            }
            val field = Field(method.name, method.typ, newValue)
            setProperty(ob, field)
          case _ => sys.error("Object type is required here")
        }
      case mu: MethodUpdate =>
        val ctx = context(mu.oldContextName.getOrElse("_")).last
        ctx match {
          case Inr(Inl(ob)) =>
            val method: Property = findProperty(ob, mu.propertyName)
              .getOrElse(sys.error(s"Method ${mu.propertyName} can't be found"))
            if (method.typ.args != mu.typ.args)
              sys.error(s"Type ${method.typ.args} does not equal to ${mu.typ.args} type")
            val newMethod = method match {
              case m: Method => getNewMethod(m, ob)
              case _         => sys.error("expected to find method")
            }
            setProperty(ob, newMethod)
          case _ => sys.error("Object type is required here")
        }
      case func: Function =>
        func.operator match {
          case Add() =>
            (
              numericFromReturn(evalParam(func.param1)),
              numericFromReturn(evalParam(func.param2))
            ) match {
              case (Inl(i1), Inl(i2)) => i1 + i2
              case (n1, n2)           => n1.asInstanceOf[Double] + n2.asInstanceOf[Double]
            }
          case _ => 0
        }
      case param: ParameterType => evalParam(param)
      case call: Call       => methodCall(call)
    }
  }

  private def evalParam(param: ParameterType): ReturnType = {
    ???
/*    if (param.methodCall.isDefined) {
      return methodCall(param.methodCall.get)
    }
    param.value match {
      case Inr(Inl(str)) =>
        val res = context(str).last
        res match {
          case Inr(Inl(ob))                               => return ob
          case Inr(Inr(Inr(Inr(Inl(int)))))               => return int
          case Inr(Inr(Inr(Inr(Inr(Inl(d))))))            => return d
          case Inr(Inr(Inr(Inr(Inr(Inr(Inl(iv)))))))      => return iv.int
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(rv)))))))) => return rv.real
          case _                                          => sys.error("Not a valid type")
        }
      case Inl(inp) =>
        inp match {
          case IntValue(i)  => return i
          case RealValue(r) => return r
        }
    }
    val arr: ArrayBuffer[ContextType] = context.getOrElse(
      param.value.select.head.asInstanceOf[String],
      sys.error(s"No param ${param.value} in context")
    )
    arr.remove(arr.length - 1)*/
  }

  private def evalLambda(
    body: Lambda,
    args: Option[Seq[Argument]]
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
    args: Option[Seq[Argument]] = None
  ): ReturnType = {
    body match {
      case l: Lambda      => evalLambda(l, args)
      case e: Expression  => evalExpr(e)
      case ot: SigmaBody => ot
      case i: IntValue    => i.int
      case r: RealValue   => r.real
    }
  }

  private def methodCall(methodCall: Call): ReturnType = {
    val args: Seq[Argument] = methodCall.arguments.getOrElse(Nil).map {
      case l: Lambda      => l
      case iv: InputValue => iv
      case e: Expression  => inputValueFromReturn(evalExpr(e))
    }
    val key: String = context
      .get("_default")
      .fold("_")(vec => vec.last.select.head)
    val arr = context.getOrElse(key, sys.error("Cannot find in context"))
    val ctx = arr.last
    ctx match {
      case Inr(Inl(ob)) =>
        val propToExecute: Option[Property] =
          findProperty(ob, methodCall.propertyName)
        val prop = propToExecute.getOrElse(
          sys.error(s"Property ${methodCall.propertyName} not found")
        )
        prop match {
          case m: Method =>
            m.context match {
              case Inr(Inl(str)) =>
                addToContext("_default", str)
                addToContext(str, ctx)
              case _ =>
            }
            evalBody(m.methodBody, Some(args))
          case f: Field => evalBody(f.value, Some(args))
        }
      case _ => sys.error("Object type is required here")
    }
  }

  private def evalSigma(
    obj: SigmaObject,
    parentCall: CutExpression
  ): ReturnType = {
    obj match {
      case o @ SigmaBody(_) => addToContext("_", o)
      case Sigma(properties, sigmaCall) =>
        sigmaCall.map(sc => {
          val ctx = evalSigma(properties, sc)
          val key = context
            .get("_default")
            .fold(Coproduct[ContextType]("_"))(vec => vec.last)
          addToContext(key.select[String].head, returnTypeToContextType(ctx))
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
