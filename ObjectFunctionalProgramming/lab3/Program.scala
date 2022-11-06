import scala.math._
import scala.language.implicitConversions

class QuadraticEquation[T](val a: T, val b: T, val c: T) (implicit solver: QuadraticEquationSolver[T]) {
  def solve()(implicit ops: QuadraticEquationSolverOps[T]): List[T] = solver.solve(a, b, c)
}

abstract class QuadraticEquationSolver[T] {
  def solve(a: T, b: T, c: T)(implicit ops: QuadraticEquationSolverOps[T]): List[T]
}

trait QuadraticEquationSolverOps[T] {
  def div(a: T, b: T): T
}

object QuadraticEquationSolverOps {
  implicit def float_ops[T](implicit frac: Fractional[T]): QuadraticEquationSolverOps[T] =
    new QuadraticEquationSolverOps[T] {
      def div(a: T, b: T): T = frac.div(a, b)
    }

  implicit def int_ops[T](implicit num: Integral[T]): QuadraticEquationSolverOps[T] =
    new QuadraticEquationSolverOps[T] {
      def div(a: T, b: T): T = {
        val x = num.abs(a)
        val y = num.abs(b)
        val aSign = num.sign(a)
        val bSign = num.sign(b)
        val resSign = num.times(aSign, bSign)
        if (num.gteq(x, y)) {
          var it = num.fromInt(0)
          var res = x
          while (num.gteq(res, y)) {
            it = num.plus(it, num.fromInt(1))
            res = num.minus(res, y)
          }
          num.times(resSign, it)
        } else {
          var it = num.fromInt(0)
          var res = y
          while (num.gteq(res, x)) {
            it = num.plus(it, num.fromInt(1))
            res = num.minus(res, x)
          }
          num.times(resSign, it)
        }
      }
    }

    implicit object complex extends QuadraticEquationSolverOps[Complex] {
      override def div(a: Complex, b: Complex): Complex = {
        a/b
      }
    }
}

object QuadraticEquationSolver {
  implicit object float extends QuadraticEquationSolver[Float] {
    override def solve(a: Float, b: Float, c: Float)(implicit ops: QuadraticEquationSolverOps[Float]): List[Float] = {
      val d = b*b - 4*a*c
      if (d == 0) {
        List[Float](-b/(2*a), -b/(2*a))
      } else if (d > 0) {
        var sqr: Float = 1
        var nx: Float = (sqr+d/sqr)/2
        while (abs(sqr-nx) > 0.00000001) {
          sqr = nx
          nx = (sqr+d/sqr)/2
        }
        List[Float]((-b+sqr)/(2*a), (-b-sqr)/(2*a))
      } else {
        List()
      }
    }
  }
  implicit object double extends QuadraticEquationSolver[Double] {
    override def solve(a: Double, b: Double, c: Double)(implicit ops: QuadraticEquationSolverOps[Double]): List[Double] = {
      val d = b*b - 4*a*c
      if (d == 0) {
        List[Double](-b/(2*a), -b/(2*a))
      } else if (d > 0) {
        List[Double]((-b+sqrt(d))/(2*a), (-b-sqrt(d))/(2*a))
      } else {
        List()
      }
    }
  }
  implicit def numSolver[T](implicit num: Integral[T]): QuadraticEquationSolver[T] = {
    new QuadraticEquationSolver[T] {
      def solve(a: T, b: T, c: T)(implicit ops: QuadraticEquationSolverOps[T]): List[T] = {
        val d = num.minus(num.times(b, b), num.times(num.times(num.fromInt(4), a), c))
        if (num.equiv(d, num.fromInt(0))) {
          val res = ops.div(num.negate(b), num.times(num.fromInt(2), a))
          if (num.equiv(num.times(res, num.times(num.fromInt(2), a)), num.negate(b))) {
            List(res, res)
          } else {
            List()
          }
        } else if (num.gt(d, num.fromInt(0))) {
          var it = num.fromInt(1)
          while (num.lt(num.times(it, it), d)) {
            it = num.plus(it, num.fromInt(1))
          }
          if (num.equiv(num.times(it, it), d)) {
            var res: List[T] = List()
            val x1 = ops.div(num.plus(num.negate(b), it), num.times(num.fromInt(2), a))
            if (num.equiv(num.plus(num.negate(b), it), num.times(x1, num.times(num.fromInt(2), a)))) {
              res = x1::res
            }
            val x2 = ops.div(num.minus(num.negate(b), it), num.times(num.fromInt(2), a))
            if (num.equiv(num.minus(num.negate(b), it), num.times(x2, num.times(num.fromInt(2), a)))) {
              res = x2::res
            }
            res
          } else {
            List()
          }
        } else {
          List()
        }
      }
    }
  }
  implicit def complexSolver(implicit complex: Complex): QuadraticEquationSolver[Complex] = {
    new QuadraticEquationSolver[Complex] {
      def solve(a: Complex, b: Complex, c: Complex)(implicit ops: QuadraticEquationSolverOps[Complex]): List[Complex] = {
        val d = b*b - Complex(4, 0)*a*c
        val newRe = sqrt((sqrt(d.re*d.re+d.im*d.im)+d.re)/2.0)
        val newIm = sqrt((sqrt(d.re*d.re+d.im*d.im)-d.re)/2.0)
        val disc = new Complex(newRe, newIm)
        List((-b+disc)/2.0, (-b-disc)/2.0)
      }
    }
  }
}

class Complex(val re: Double, val im: Double) {
  override def toString = re.toString() + (if (im < 0) "-" + (-im).toString() else "+" + im.toString()) + "*i"
  def unary_+ = this
  def unary_- = new Complex(-re, -im)
  def +(c: Complex) = new Complex(re + c.re, im + c.im)
  def +(d: Double) = new Complex(re + d, im)
  def -(c: Complex) = this + -c
  def *(c: Complex) =
    new Complex(re * c.re - im * c.im, im * c.re + re * c.im)
  def /(c: Complex) = {
    require(c.re != 0 || c.im != 0)
    val d = pow(c.re, 2) + pow(c.im, 2)
    new Complex((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
  }
  def /(d: Double) = {
    new Complex(re/d, im/d)
  }
  def this(re: Double) = this(re, 0)
  implicit def fromDouble(d: Double) = new Complex(d)
  implicit def fromFloat(f: Float) = new Complex(f)
  implicit def fromInt(i: Int) = new Complex(i)
}

object Complex {
  val i = new Complex(0, 1)
  def apply(re: Double, im: Double) = new Complex(re, im)
  def apply(re: Double) = new Complex(re)
  implicit def fromDouble(d: Double) = new Complex(d)
  implicit def fromFloat(f: Float) = new Complex(f)
  implicit def fromInt(i: Int) = new Complex(i)
  implicit val complex: Complex = new Complex(0, 0)
}

object Program {
  def main(args: Array[String]) = {
    import Complex._
    val qe = new QuadraticEquation[Complex](1, Complex(-7, 2), Complex(13, -13))
    // val qe = new QuadraticEquation[Int](1, 2, 1)
    val com = qe.solve()
    println(com)
  }
}