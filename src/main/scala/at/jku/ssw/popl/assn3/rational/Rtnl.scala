package at.jku.ssw.popl.assn3.rational

import at.jku.ssw.popl.assn3.expr.ExprInteractive
import at.jku.ssw.popl.assn3.field.Field

case class Rtnl(numer: Int, denom: Int) {
  override def toString: String = s"$numer/$denom"
}

object Rtnl {
  def apply(n: Int, d: Int) = {
    if (n == 0) then new Rtnl(0, 1)
    else {
      val g = gcd(n.abs, d.abs)
      if (n <= 0 && d < 0) then new Rtnl(-n / g, -d / g)
      else new Rtnl(n / g, d / g)
    }
  }
  def apply(n: Int) = new Rtnl(n, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  given rtnlField: Field[Rtnl] with
    def plus(x: Rtnl, y: Rtnl): Rtnl = {
      Rtnl.apply(x.numer*y.denom+x.denom*y.numer, x.denom*y.denom)
    }

    def times(x: Rtnl, y: Rtnl): Rtnl =
      Rtnl.apply(x.numer*y.numer,x.denom*y.denom)
    def neg(x: Rtnl):Rtnl =
      Rtnl.apply(-x.numer, x.denom)

    def recip(x: Rtnl): Rtnl =
      Rtnl.apply(x.denom, x.numer)

    val zero: Rtnl = Rtnl.apply(0,1)
    val one: Rtnl = Rtnl.apply(1,1)
}

// Application

object RtnlApp extends App {

  object RtnlInteractive extends ExprInteractive[Rtnl] {
    def value: Parser[Rtnl] =
      wholeNumber ~ "/" ~ wholeNumber ^^ { case n ~ _ ~ d => Rtnl(n.toInt, d.toInt) } |
        wholeNumber ^^ { case n => Rtnl(n.toInt) }

  }
  RtnlInteractive.interact(Map("x" -> Rtnl(1, 2), "y" -> Rtnl(2, 3))) (using Rtnl.rtnlField) // TODO: delete using

}
