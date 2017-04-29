import expressions._

val parsed =
  Parser parse """
    |[
      |numeral = @top => [
      |  zero = @ numeral => [
      |    case = @ zero => \ z => \ s => z,
      |    succ = @ zero => (zero.case :=  \ z => \ s => s zero).val := zero.val + 1,
      |    val  = @ zero => 0,
      |    pred = @ this => this.case (numeral.zero) (\ x => x)
      |  ]
      |],
      |main = @ top => (top.numeral.zero.succ.pred.succ.succ.succ.succ.pred.pred.pred.pred.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ.succ).val
    |].main
  """.stripMargin

val parsed2 = Parser parse "[a = @this => this].a"
//.,
//pred = @ this => this.case (top.zero) (\ x => x)

parsed map Semantic.eval