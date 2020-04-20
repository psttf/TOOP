package expressions

import org.specs2.mutable._

import scala.util.Success

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
          ).value<=@s3=>15
        ).backup).value<=@s3=>25).retrive.retrive.value"""//.retrive"""//.retrive<=@s1=>s2]"

      Semantic.eval(Parser parse restorable match {case Success(t) => t}).term.map(_.toString) must
        beEqualTo(Right("10"))
    }

    "eval abstraction application" in {
      val o1 = "(\\x=>\\y=>\\z=>x (y z)) x y z"
      Semantic.eval(Parser parse o1 match {case Success(t) => t}).term.map(_.toString) must
        beEqualTo(Right("(x (y z))"))
    }

    "eval Fibonacci" in {
      val code =
        """(
          |[
          |zero = @ this1 => [
          |case = @ this => \ x => \ y => x,
          |val = @ this => 2,
          |succ = @ this => (this.case := \ x => \ y => y this).val := this.val + 1,
          |pred = @ this => this.case (this1.zero) (\ x => x),
          |one = @ this => this1.zero.succ,
          |iszero = @ this => this.case (this.true) (\ x => this.false),
          |fib = @ this => ((this.iszero.then:=1).else:=(this.pred.fib + ((this.pred.iszero.then:=1).else:=(this.pred.pred.fib)).val)).val,
          |true = @ this =>
          |[
          |then = @ x => x.then,
          |else = @ x => x.else,
          |val = @ x => x.then
          |],
          |false = @ this =>
          |[
          |then = @ x => x.then,
          |else = @ x => x.else,
          |val = @ x => x.else
          |]
          |],
          |main = @ this => this.zero.fib
          |].main
          |)
          |""".stripMargin
      val result = Semantic.eval(Parser parse code match {case Success(t) => t})
      result.term.map(_.toString) must beEqualTo(Right("1"))
    }

  }

}
