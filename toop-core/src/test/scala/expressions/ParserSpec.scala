package expressions

import org.specs2.mutable._

class ParserSpec extends Specification {

  "Parser" should {
    "parse x.l.b b (d e)" in {
      Parser.parse("x.l.b b (d e)").toString must
        beEqualTo("Success((({{x.l}.b} b) (d e)))")
    }
    "parse 10" in {
      Parser.parse("10").toString must
        beEqualTo("Success(10)")
    }
    "parse ([l = @x=>x].l:=[]).g:=a" in {
      Parser.parse("([l = @x=>x].l:=[]).g:=a").toString must
        beEqualTo("Success((([l = (@x=>x)].l<=[]).g<=a))")
    }
  }

}
