import expressions._
import utils._

import scala.util.Success

/*val parsed = Parser parse "" +
  "[n = @ x => n, n2 = @ x => [k = @ l => l]] 10 [a = @ x => 10 ]"

val parsed2 = Parser parse "[" +
  "  zero = @ top => [" +
  "    case = @ this => \\ z => \\ s => z," +
  "    succ = @ this => (this.case := \\ z => \\ s => s this).val := this.val + 1,    " +
  "    val  = @ this => 0,    " +
  "    pred = @ this => this.case (top.zero) (\\ x => x)" +
  "    ]," +
  "  main = @ top => (top.zero.succ.succ.succ.succ.succ.pred).val" +
  "].main"

val s = parsed2 match {
  case Success(t) =>
    "\n"+Printer.print(t, "", false)
  case _ => ""
}

//println(s)

//parsed map {(t) =>print(Printer.print(t, "", false))}
"a\nnb".lastIndexOf("\n")
*/


val parserd3 = Parser parse "[" +
  "  zero = @ top => [" +
  "    case = @ this => \\ z => \\ s => z," +
  "    succ = @ this => (this.case := \\ z => \\ s => s this).val := this.val + 1,    " +
  "    val  = @ this => 0,    " +
  "    pred = @ this => this.case (top.zero) (\\ x => x)" +
  "    ]," +
  "  main = @ top => (top.zero).val" +
  "].main"



