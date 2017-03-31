import expressions.{Parser, Semantic}

val parsed = Parser parse
 """([
   |  x=@o=>0,
   |  y=@o=>0,
   |  mv_x=@o1=>\dx=>o1.x:=(o1.x)+dx
   |].mv_x 1).x""".stripMargin
//val parsed = Parser parse "([arg=@o=>o,val=@o=>o.arg].arg<=(@o=>15)).val"
//val parsed = Parser parse "(10 + 10)"

val right = parsed.right map Semantic.eval

println(s"right = $right")
