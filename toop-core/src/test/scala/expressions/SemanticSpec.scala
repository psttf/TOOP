package expressions

import org.specs2.mutable._

class SemanticSpec extends Specification {

  "Semantic" should {
    "build simple term" in {
      Lambda(
        Variable("x"), Variable("y")).create(Variable("a"), Variable("c")
      ).toString must beEqualTo("(\\a=>c)")
    }
    "execute substitutions" in {
      Semantic.substitution(
        Variable("x"), Variable("y"),
        Lambda(Variable("x"),
          Application(
            Application(Variable("x"), Variable("$x0")), Variable("y")))
      ).toString must beEqualTo("(\\$x1=>(($x1 $x0) x))")
    }
  }

}
