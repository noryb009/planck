import org.scalatest.FunSpec
import planck.{SizeMismatchException, Vecd}

class VecdTests extends FunSpec {
  def v(e: Double*) = Vecd(e)

  describe("+") {
    it("adds vectors") {
      val v1 = v(1) + v(4)
      assert(v1(0) == 5)
      val v2 = v(2, 6, 3) + v(4, -1, 8)
      assert(v2.entries == Seq(6, 5, 11))
    }

    it("adds vectors of size 0") {
      val v0 = v() + v()
      assert(v0.entries.isEmpty)
    }
  }

  describe("dot") {
    it("finds the dot product of vectors") {
      assert((v(1, 6) dot v(5, 2)) == 5 + 12)
      assert((v(1, 6, 8, 2) dot v(2, -2, 1, 3)) == 2 - 12 + 8 + 6)
    }

    it("finds the dot product of vectors of size 0") {
      assert((v() dot v()) == 0)
    }
  }

  describe("cross") {
    it("finds the cross product of vectors.") {
      assert((v(1, 0, 0) cross v(0, 1, 0)) == v(0, 0, 1))
      assert((v(0, 1, 0) cross v(1, 0, 0)) == v(0, 0, -1))
      assert((v(1, 2, 3) cross v(4, -1, 0)) == v(3, 12, -9))
    }

    it("throws an exception when at least one vector is not size 3") {
      intercept[SizeMismatchException] {
        v(1, 2, 3) cross v(4, 5)
      }

      intercept[SizeMismatchException] {
        v(1, 2) cross v(3, 4, 5)
      }
    }
  }

  describe("normalize") {
    it("normalizes a vector") {
      val n = v(3, 4, 0).normalize
      assert(n.lengthSq == 1.0)
      assert(n == v(3.0/5, 4.0/5, 0))

      val n2 = v(0, 3, 4).normalize
      assert(n2.lengthSq == 1.0)
      assert(n2 == v(0, 3.0/5, 4.0/5))
    }

    it("normalizes a vector of length 0") {
      assert(v(0, 0, 0).normalize == v(0, 0, 0))
    }

    it("normalizes a vector of size 0") {
      assert(v().normalize == v())
    }
  }
}
