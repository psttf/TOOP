package expressions

/**
  * Created by Qwex on 26.03.2017.
  */
//object SigmaDSL {
//  def v(name : String) : Variable = Variable(name)
//
//  object L {
//    def |(v : Variable) : L2 = new L2(Seq(v))
//  }
//
//  class L2(vs : Seq[Variable]) {
//    def |(v : Variable) : L2 = new L2(v+:vs)
//    def |>(body : Term) : Term =
//      vs.foldLeft(body)((b, v) => Lambda(v, b))
//
//    override def toString : String = s"L2($vs)"
//  }
//
//  def main(args : Array[String]) : Unit = {
//    println(L|v("a")|v("b")|>v("c"))
//  }
//}
//
//object Terms {
//  trait Term
//
//  case class Subst(v : String, t : Term)
//
//  case class Var(v : String) extends Term {
//    def apply(s : Subst) : Term = {
//      s match {
//        case Subst(v2, t) if v == v2 =>
//          t
//        case _ => this
//      }
//    }
//  }
//
//  case class App(t1 : Term, t2 : Term) {
//
//  }
//
//  case class Lambda(v : String, b : Term) {
//    def apply(t : Term) : Term = {
//      null
//    }
//  }
//
//  //case object <| {
//  //  def |()
//  //}
//}
//
//object Terms2 {
//  trait Term
//
//  class Obj(methods : Obj=>Term) extends Term {
//
//  }
//}
