package at.jku.ssw.popl.assn3.field

trait Field[F] {
  def plus(x: F, y: F): F
  def times(x: F, y: F): F
  def neg(x: F): F
  def recip(x: F): F
  val zero: F
  val one: F
}

object Field {
  // TODO: given for Field[Int]
  given intField: Field[Int] with
    def plus(x: Int, y: Int): Int = {
      x + y
    }

    def times(x: Int, y: Int): Int =
      x * y

    def neg(x: Int): Int =
      -x

    def recip(x: Int): Int =
      scala.math.pow(x.toDouble, -1.0).toInt

    val zero: Int = 0
    val one: Int = 1

  // TODO: given for Field[Double]
  given doubleField: Field[Double] with
    def plus(x: Double, y: Double): Double = {
      x + y
    }

    def times(x: Double, y: Double): Double =
      x * y

    def neg(x: Double): Double =
      -x

    def recip(x: Double): Double =
      scala.math.pow(x, -1.0)

    val zero: Double = 0.0
    val one: Double = 1.0
}