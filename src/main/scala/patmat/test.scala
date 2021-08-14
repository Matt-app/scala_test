package patmat

trait Expr
case class Num(n: Int) extends Expr
case class Var(name: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Pro(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match
  case Num(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
  case Pro(e1, e2) => eval(e1) * eval(e2)



def show(e: Expr): String = e match {
  case Num(n) => n.toString()
  case Var(name) => name
  case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
  case Pro(e1, e2) => s"${showP(e1)} * ${showP(e2)}"
}


def showP(e: Expr): String = e match {
  case e: Sum => s"(${show(e)})"
  case _ => show(e)
}


object Main extends App:
  val expr = Sum(Num(1), Num(1))
  val expr2 = Pro(expr, Num(3))
  val expr3 = Sum(expr2, Num(1))
  eval(expr)
  eval(expr2)
  show(expr)
  show(expr2)
  show(expr3)
  println(show(Pro(Num(2), Sum(Var("x"), Var("y")))))


