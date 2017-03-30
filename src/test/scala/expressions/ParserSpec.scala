package expressions

import org.specs2.mutable._

class ParserSpec extends Specification {

  "Parser" should {
    "parse x.l.b b (d e)" in {
      Parser.parse("x.l.b b (d e)").toString must
        beEqualTo("Right((({{x.l}.b} b) (d e)))")
    }
    "parse 10" in {
      Parser.parse("10").toString must
        beEqualTo("Right(10)")
    }
    "parse ([l = @x=>x].l:=[]).g:=a" in {
      Parser.parse("([l = @x=>x].l:=[]).g:=a").toString must
        beEqualTo("Right((([l = (@x=>x)].l<=(@$0=>[])).g<=(@$0=>a)))")
    }
  }

}
