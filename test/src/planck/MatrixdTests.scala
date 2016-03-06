import org.scalatest.FunSpec
import planck.{SizeMismatchException, Vecd, Matrixd}

class MatrixdTests extends FunSpec {
  def v(e: Double*) = Vecd(e)
  def m(e: Vecd*) = Matrixd(e)

  val m1234 = m(v(1, 2), v(3, 4))
  val m2468 = m(v(2, 4), v(6, 8))
  val m135975 = m(v(1, 3), v(5, 9), v(7, 5))
  val m126790 = m(v(1, 2, 6), v(7, 9, 0))

  describe("*") {
    describe("matrix-matrix multiplication") {
      it("multiplies matrices") {
        assert(m1234 * m2468 == m(v(14, 20), v(30, 44)))
      }

      it("multiplies matrices of different sizes") {
        assert(m1234 * m135975 == m(v(10, 14), v(32, 46), v(22, 34)))
        assert(m135975 * m126790 == m(v(53, 51), v(52, 102)))
      }

      it("multiplies matrices of size 0") {
        assert(m() * m() == m())
      }

      it("throws an exception on an invalid sizes") {
        intercept[SizeMismatchException] {
          m135975 * m1234
        }
      }
    }

    describe("matrix-vector multiplication") {
      it("multiplies matrices and vectors") {
        assert(m1234 * v(5, 3) == v(14, 22))
        assert(m135975 * v(3, -1, 2) == v(12, 10))
      }

      it("throws an exception on an invalid sizes") {
        intercept[SizeMismatchException] {
          m135975 * v(1, 2, 3, 4)
        }
      }
    }
  }
}
