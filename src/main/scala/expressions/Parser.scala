package expressions

import util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.{PackratParsers, ImplicitConversions}

object Parser extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  //import AST._

  //import CAM.Language._

  def parse(input: String): Either[String, Term] =
    phrase(expr)(new lexical.Scanner(input)) match {
      case Success(ast, _) => Right(ast)
      case e: NoSuccess => Left("phrase error: " + e.msg)
    }

  /*lexical.reserved ++= ("" split " ")*/
  lexical.delimiters ++= ("\\ => ( ) { } = . <= [ ] # := ," split ' ')

  type P[+T] = Parser[T]

  def expr : P[Term] = applications | operations//methodInvocation

  def applications:P[Term] = operations~rep1(operations) ^^ {case o1~o2 =>(o1/:o2)((t1:Term, t2:Term) => Application(t1, t2))}

  def operations:P[Term] = methodUpdate | fieldUpdate | methodInvocation | term

  def methodUpdate = (term<~".")~(ident<~"<=")~expr ^^ {case t~l~e => MethodUpdate(t,l,e)}

  def fieldUpdate = (term<~".")~(ident<~":=")~expr ^^
    {case t~l~f => MethodUpdate(t, l, Sigma(Variable(Semantic.genName("", Semantic.FV(f))), f))}

  def methodInvocation = term~rep1("."~>ident) ^^ {case t~l => (t/:l)((t, l) => MethodInvocation(t, l))}

  def term : P[Term] = sigma | lambda | const | variable | objectFormation | "("~>expr<~")"

  def lambda = ("\\"~>ident)~("=>"~>expr) ^^ {case v~b => Lambda(Variable(v), b)} //| application

  def objectFormation = "["~>repsep(methodDefinition, ",")<~"]" ^^ {case methods => ObjectFormation(methods.toMap)}

  def methodDefinition : P[(String, Term)] = ident~("="~>sigma) ^^ {case l~s => (l, s)}

  def const = numericLit ^^ {n => Number(n.toInt)}
  //def application = expr~rep1{expr}

  def sigma = ("#" ~> variable) ~ ("=>"~> expr ) ^^ Sigma

  def variable : P[Variable] = ident ^^  {case i => Variable(i)}
  //type P[+T] = PackratParser[T]

  //def program = rep1sep(expr, ";")

  /*lazy val expr: P[Term] = lambda | (application ^^ {case a => Application(a.head, a.tail.head)}) | methodInvocationPckrat | operations2

  lazy val methodInvocationPckrat : P[MethodInvocation] = (expr<~".")~ident ^^ {case e~i => MethodInvocation(e, i)}

  lazy val application : P[List[Term]] = repN(2, expr)// ^^ { case e1~e2 =>
    //e1::e2::Nil
    //if (e1.length == 1)  e1.head else Application(e1.head, e1.tail.head)
  //}
    //println(e1, e2);(e1 /: e2)((t1: Term, t2: Term) => Application(t1, t2))}

  def operations2 = ident ^^ {case i => Variable(i)}
  //|  simpleExpr
  lazy val lambda : P[Lambda] = ("\\" ~> variable) ~ ("=>" ~> expr) ^^ Lambda

  def sigma = ("#" ~> variable) ~ ("=>"~> expr ) ^^ Sigma

  def ObjectFormation = "["~>repsep(field, ",")<~"]" ^^ {case l => l.toMap}

  def field = (ident~>"=")~sigma ^^ {case l~e => (l, e)}

  def methodInvocation = simpleExpr~rep1("."~>ident) ^^ {
    case expr~labels =>
    (expr /: labels)((expr, label) => MethodInvocation(expr, label))
  }
  //def methodInvocation = (expr<~".")~ident ^^ //{case e~l => MethodInvocation(e,l)}

  def methodUpdate = simpleExpr~rep1(("."~>ident)~("<=>"~>expr)) ^^
    {
      case expr~updates=> (expr /: updates)((expr, update) => MethodUpdate(expr, update._1 ,update._2))
    }

  def variable = ident ^^ { case i => Variable(i)}

  /*def pat : P[Pattern] = (ident ^^ (new Variable(_))
    | ("("~>pat)~(","~>pat)<~")" ^^ PairPattern
    | (ident <~ "as") ~ pat ^^ {case i~p => LayeredPattern(new Var(i), p)}
    )*/

  //def ifExpr = ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ IfExpr

  //def letExpr = ("let" ~> pat) ~ ("=" ~> expr) ~ ("in" ~> expr) ^^ LetExpr |
  //              ("letrec" ~> pat) ~ ("=" ~> expr) ~ ("in" ~> expr) ^^ LetrecExpr

  def operations =
    methodUpdate |
    methodInvocation |
      lambda | sigma |
    simpleExpr ~ rep(simpleExpr) ^^ { case e1 ~ e2 => (e1 /: e2)((t1: Term, t2: Term) => Application(t1, t2))}

  def simpleExpr = ident ^^ { case i => Variable(i)} | "(" ~> expr <~ ")" */

  def main(args:Array[String]): Unit = {
    println(parse("x.l.b b (d e)"))
    //val p = parse("x.l.c.d")
    //println(p)
    //println(parse("x y z"))
    //println(parse("#x=>y"))
    println(parse("10"))
    println(parse("([l = #x=>x].l:=[]).g<=a"))

    val o1 = "(\\x=>\\y=>\\z=>x (y z)) x y z"
    val restorable =
      """
      (((
        (
          [
           retrive = #s1=>s1,
           backup = #s2=>s2.retrive<=#s1=>s2,
           value = #s3=>10
          ].backup
        ).value<=(#s3=>15)
      ).backup).value<=(#s3=>25)).retrive.retrive.value"""//.retrive"""//.retrive<=#s1=>s2]"

    //println(parse(restorable))

    println(
      Semantic.
        eval(
        parse(restorable) match {case Right(t) => t}))

    println(Semantic.eval(parse(o1) match {case Right(t) => t}))
    //println(parse("(\\x=>x) (\\y=>y)"))
  }
}


    /*| numericLit ^^ {case n => Num(n.toInt)}
    | ("true" ^^^ Bool(true) | "false" ^^^ Bool(false))
    | stringLit ^^ Str
    | ("("~>expr)~(","~>expr<~")") ^^ Pair
    | "(" ~> expr <~ ")"
    //| failure("Expression expected")*/

  /*def program = rep1sep(expr, ";") <~ opt(";") ^^ Sequence
  def expr: P[Expr] = lambda | ifExpr | assign | operations

  def lambda = ("\\" ~> repsep(ident, ",")) ~ ("=>" ~> expr) ^^ Lambda
  def ifExpr = ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ IfExpr
  def assign = ident ~ ("=" ~> expr) ^^ Assign
  def operations = infixOp

  def infixOp = equality
  def equality = sum * ("==" ^^^ Equal)
  def sum = product * ("+" ^^^ Add | "-" ^^^ Sub)
  def product = postfixOps * ("*" ^^^ Mul | "/" ^^^ Div)

  def postfixOps = application

  def application = simpleExpr ~ rep(argList) ^^ {case e~args => (e /: args)(Application)}

  def argList = "(" ~> repsep(expr, ",") <~ ")" | simpleExpr ^^ {List(_)}

  def simpleExpr = (ident ^^ Var
    | numericLit ^^ {x => Lit( x.toInt)}
    | stringLit ^^ Lit
    | "(" ~> expr <~ ")"
    | failure("Expression expected")
    )*/

/*  def main(args: Array[String]) {
    def p(s: lexical.Scanner): Unit =
      if (s.atEnd)
        Unit
      else {
        println(s.first)
        p(s.rest)
      }
    p(new lexical.Scanner("if addf Lambda sorting +"))
    //def par = "if" ~ ident ~ ident ~ ident ^^^ "Распарсил"
    //println(parse("\"s\" \"b\""))
    //println(parse("(\\ x => \"s\") (\\ x => x) \"b\""))
    //println(parse("(\\ x => \"s\") \"b\""))
    println(phrase(expr)(new lexical.Scanner("\\(x, y)=> x")))

    def l1:P[Expression] = l2

    def l2 =  ("\\"~>p1)~("=>"~ident) ^^ {case p1~("=>"~i) => Abs(p1, Ind(i))}
    def p1 = ("("~>ident)~(","~>ident<~")") ^^ {case i1~i2 => PairPattern(Ind(i1), Ind(i2))}
    println
    println(phrase(l1)(new lexical.Scanner("\\(x, y)=> x")))
    println
    println(phrase(expr)(new lexical.Scanner("(\"s\",\"b\")")))
    println
    println(parse("(\\(x,y)=>x) ((\\x => \\y=> x) (10,20) (\\z => z))"))
    println(parse("\\x as (y,z) => "))
    println(parse("let (x,y) = (\\x => (x, 20)) 10 in (\\z => z) x"))
    println(parse("letrec x = \\y => if y then x false else false in y true"))
    //println(phrase(par)(new lexical.Scanner("if addf else sorting")))
  }
} */

/*object AST {
  sealed abstract class Expr

  case class Sequence(l: List[Expr]) extends Expr {
    override def toString = l.mkString("\n")
  }

  case class Lambda(params: List[String], body: Expr) extends Expr {
    override def toString = "Lambda(Params("+params.mkString(", ")+"), Body("+body+"))"
  }

  case class IfExpr(e1: Expr, e2: Expr, e3: Expr) extends Expr
  case class Assign(name: String, expr: Expr) extends Expr

  case class Equal(e1: Expr, e2: Expr) extends Expr
  case class Add(e1: Expr, e2: Expr) extends Expr
  case class Sub(e1: Expr, e2:Expr) extends Expr
  case class Mul(e1: Expr, e2:Expr) extends Expr
  case class Div(e1: Expr, e2:Expr) extends Expr

  case class Application(func: Expr, args: List[Expr]) extends Expr {
    override def toString = "App(Fun("+func+"), Args("+args.mkString(", ")+"))"
  }

  case class Var(name: String) extends Expr

  case class Lit(v: Any) extends Expr {
    override def toString = if (v.isInstanceOf[String]) "\""+v+"\"" else v.toString
  }*/
//}