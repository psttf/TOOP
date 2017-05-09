package expressions

object Semantic {

  def FV(t:Term):Set[String] = t.FV

  /*t match {
     case Variable(name) =>
       Set(name)
     case Application(t1, t2) =>
       FV(t1) union FV(t2)
     case ObjectFormation(methods) =>
       methods.values.foldLeft[Set[String]](Set())((a,b) => a union FV(b))
     case MethodInvocation(obj, _) => FV(obj)
     case MethodUpdate(obj, _, method)  =>
       FV(obj) union FV(method)
     case quantifier : Quantifier[_] =>
       FV(quantifier.body) - quantifier.variable.name
     case t: Arithmetic => t.FV1
     case _ => Set()

   }*/

  def substitution(t1:Term, variable:Variable, t2:Term):Term =
    if (t2.FV(variable.name))
    t2 match {
    case Variable(name) if name == variable.name =>
      t1
    case Application(tA1, tA2) =>
      Application(substitution(t1, variable, tA1), substitution(t1, variable, tA2))
    case ObjectFormation(methods) =>
      ObjectFormation(methods.mapValues(v=>substitution(t1, variable, v)))
    case MethodInvocation(obj, label) =>
      MethodInvocation(substitution(t1, variable, obj), label)
    case MethodUpdate(obj, label, method) =>
      MethodUpdate(substitution(t1, variable, obj), label, substitution(t1, variable, method))
    case quantifier :Quantifier[_] if variable != quantifier.variable =>
      val fvT1 = FV(t1)
      val fv = FV(quantifier)
      if (!(fvT1 contains quantifier.variable.name))
        quantifier.create(quantifier.variable, substitution(t1, variable, quantifier.body))
      else
      {
        val newVariable = Variable(genName(quantifier.variable.name, fv union FV(quantifier)))
        quantifier.create(
          newVariable,
          substitution(t1, variable,
            substitution(newVariable, quantifier.variable, quantifier.body)
          )
        )
      }
    case Add(t21, t22) => Add(substitution(t1, variable, t21), substitution(t1, variable, t22))
    case Subtract(t21, t22) => Subtract(substitution(t1, variable, t21), substitution(t1, variable, t22))

    case t => t
  } else t2


  def genName(name:String, names:Set[String]):String = {
    def genName(name:String, names:Set[String], i:Int):String = {
      val newName = "$"+name+i
      if(!(names contains newName))
        newName
      else
        genName(name, names, i + 1)
    }
    genName(name, names, 0)
  }

  def eval(t:Term):Term = {
    def eval(t:Term, last:Term):Term = {
      val step = eval1(t)
      if (step eq last)
        last
      else
        eval(step, step)
    }
    eval(t, t)
  }

  def eval1(t:Term):Term = t match {
    case Application(t1, t2) =>
      val t1V = eval1(t1)
      if (t1V ne t1)
        Application(t1V, t2)
      else {
        val t2V = eval1(t2)
        if (t2V ne t2)
          Application(t1V, t2V)
        else
          t1V match {
            case Lambda(v, b) =>
              substitution(t2V, v, b)
            case _ => t//throw new IllegalStateException
        }
      }

    case MethodInvocation(o, l) =>
      //TOOD: Можно оптимизировать, и сразу начать новый цикл, а не возвращаться на уровень выше
      //println("invoke", o, l)
      val oV = eval1(o)
      //TODO поменять эквивалентость на сравнение ссылок. Если объект был изменён, то и ссылка на него изменится
      if (o eq oV)
      oV match {
        case obj @ ObjectFormation(methods) =>
          if (methods.contains(l))
            methods(l)  match {
              case Sigma(v, b) =>
                substitution(o, v, b)
              case _ => throw new IllegalStateException
            }
          else
            throw new IllegalAccessException

        case _ if oV == o =>
          println(o)
          println(oV)
          throw new IllegalStateException
      } else
        MethodInvocation(oV, l)

    case MethodUpdate(o, l, m) =>
      //println("update!", o , l , m)
      val oV = eval1(o)
      if (oV eq o) {
        val mV = m // eval1(m)
        if (mV ne m)
          MethodUpdate(oV, l, mV)
        else
          (oV, mV) match {
            case (ObjectFormation(methods), Sigma(_, _)) =>
              ObjectFormation(methods + ((l, m)))
            case (ObjectFormation(methods), field) =>
              ObjectFormation(methods + (
                (
                  l,
                  Sigma(Variable(Semantic.genName("", Semantic.FV(field))), field)
                )))
            case _ => throw new IllegalArgumentException
          }
      }
      else
        MethodUpdate(oV, l, m)

    case Add(t1, t2) =>
      val t1V = eval1(t1)
      //TODO: заменил == на eq
      if (t1 eq t1V)
      {
        val t2V = eval1(t2)
        //TODO заменил == на eq
        if (t2V eq t2)
        {
          (t1V, t2V) match {
            case (Number(n1), Number(n2)) =>
              Number(n1+n2)
            case _ => throw new IllegalArgumentException
          }
        }
        else
        Add(t1V, t2V)
      } else
      Add(t1V, t2)

    case Subtract(t1, t2) =>
      val t1V = eval1(t1)
      if (t1 == t1V)
      {
        val t2V = eval1(t2)
        if (t2V == t2)
        {
          (t1V, t2V) match {
            case (Number(n1), Number(n2)) =>
              Number(n1-n2)
            case _ => throw new IllegalArgumentException
          }
        }
        else
        Subtract(t1V, t2V)
      } else
      Subtract(t1V, t2)

    case _ => t
  }
}




//"""[
//                     |  numeral = @ top => [
//                     |    zero = @ numeral => [
//                     |      case = @ this => \ z => \ s => z,
//                     |      succ = @ this => (this.case := \ z => \ s => s this).val := this.val + 1,
//                     |      val  = @ this => 0
//                     |      pred = @ this => this.case (numeral.zero) (\ x => x)
//                     |    ]
//                     |  ],
//                     |
//                     |  zero = @ top => [
//                     |    case = @ this => \ z => \ s => z,
//                     |    succ = @ this => (this.case := \ z => \ s => s this).val := this.val + 1,
//                     |    val  = @ this => 0
//                     |  ],
//                     |  pred = @ this => this.case (top.zero) (\ x => x),
//                     |  main = @ top => (top.numeral.zero.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ).val
//                     |].main""".stripMargin
