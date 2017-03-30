package expressions

import org.specs2.mutable._

class SemanticParserSpec extends Specification {

  "Semantic Parser" should {

    "eval restorable" in {

      val restorable =
        """
        (((
          (
            [
             retrive = @s1=>s1,
             backup = @s2=>s2.retrive<=@s1=>s2,
             value = @s3=>10
            ].backup
          ).value<=(@s3=>15)
        ).backup).value<=(@s3=>25)).retrive.retrive.value"""//.retrive"""//.retrive<=@s1=>s2]"

      Semantic.eval(Parser parse restorable match {case Right(t) => t}).toString must
        beEqualTo("10")

    }

    "eval abstraction application" in {
      val o1 = "(\\x=>\\y=>\\z=>x (y z)) x y z"
      Semantic.eval(Parser parse o1 match {case Right(t) => t}).toString must
        beEqualTo("(x (y z))")
    }

  }

}
