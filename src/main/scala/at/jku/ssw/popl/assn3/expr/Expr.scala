package at.jku.ssw.popl.assn3.expr

import at.jku.ssw.popl.assn3.field.Field

sealed abstract class Expr[T] {
  override def toString: String = Expr.show(this)
}
case class Lit[T](value: T) extends Expr[T]
case class Var[T](name: String) extends Expr[T]
abstract class UnyExpr[T](sub: Expr[T]) extends Expr[T]
case class Minus[T](sub: Expr[T]) extends UnyExpr[T](sub)
case class Recip[T](sub: Expr[T]) extends UnyExpr[T](sub)
abstract class BinExpr[T](left: Expr[T], right: Expr[T]) extends Expr[T]
case class Add[T](left: Expr[T], right: Expr[T]) extends BinExpr[T](left, right)
case class Mult[T](left: Expr[T], right: Expr[T]) extends BinExpr[T](left, right)

object Expr {

  def show[T](expr: Expr[T]): String = {
    expr match {
      case Lit(x) => x.toString
      case Var(n) => n
      case Add(left, right) => "(" + show(left) + " + " + show(right) + ")"
      case Minus(sub) => "(- " + show(sub) + ")"
      case Mult(left, right) => "(" + show(left) + " * " + show(right) + ")"
      case Recip(sub) => "(/ " + show(sub) + ")"
    }
  }

  def eval[T](expr: Expr[T], bds: Map[String, T])(using field: Field[T]) : T = {
    expr match {
      case Lit(x) => x
      case Var(n) => bds(n)
      case Mult(left, right) => field.times(eval(left, bds), eval(right, bds))
      case Add(left, right) => field.plus(eval(left, bds), eval(right, bds))
      case Minus(value) => field.neg(eval(value, bds))
      case Recip(value) => field.recip(eval(value, bds))
      case _ => ???
    }
  }

  def simplify[T](expr: Expr[T])(using field: Field[T]) : Expr[T] = {
    expr match {
      case m @ Minus(Minus(value)) => simplify(value) // (- (-a)) = a
      case r @ Recip(Recip(value)) => simplify(value) // (a^-1)^-1 = a
      case r @ Recip(value) => Recip(simplify(value)) // recursion if no match ?
      case a @ Add(left, Lit(field.zero)) => simplify(left) // a + 0 = a
      case a @ Add(Lit(field.zero), right) => simplify(right) // 0 + a = a
      case a @ Add(left, Minus(right)) if left.equals(right) => Lit(field.zero) // a + (-a) = 0
      case a @ Add(left, right) => Add(simplify(left), simplify(right)) // recursion if no match ?
      case m @ Mult(l, z @ Lit(field.zero)) => z // a * 0 = 0
      case m @ Mult(z @ Lit(field.zero), r) => z // 0 * a = 0
      case m @ Mult(z @ Lit(field.one), r) => simplify(r) // a * 1 = a
      case m @ Mult(l, z @ Lit(field.one)) => simplify(l) // 1 * a = a
      case m @ Mult(l, Recip(r)) if l.equals(r) => Lit(field.one) // a * (a^-1) = 1
      case m @ Mult(l, r) => Mult(simplify(l), simplify(r)) // recursion if no match ?
      case _ => expr
    }
  }
}