import org.scalatest.FunSpec
import planck._

class QuaterniondTests extends FunSpec {
  def q(e: Double*) = Quaterniond(e)
  def v(e: Double*) = Vecd(e)
  def m(e: Vecd*) = Matrixd(e)

  it("throws an error if not given 4 components") {
    intercept[SizeMismatchException] {
      q(1, 2, 3)
    }
  }

  describe("*") {
    it("multiplies quaternions") {
      val r = q(1,2,3,4) * q(2,3,4,5)
      assert(r == q(-36,6,12,12))
      val r2 = q(0,6,1,-7) * q(0,8,3,2)
      assert(r2 == q(-37,23,-68,10))
    }
  }

  describe("fromYPR") {
    it("converts yaw, pitch and roll to a quaternion") {
      assert(Quaterniond.fromYPR(1,1.5,2) ~= q(0.072,0.717,0.693,0.028))
    }
  }

  describe("toRotationMatrix") {
    it("converts the quaternion to a rotation matrix") {
      val ret = q(0.072,0.717,0.693,0.028).toRotationMatrix
      val exp = m(
        v(0.038, 0.998, -0.060, 0),
        v(0.990, -0.030, 0.142, 0),
        v(0.140, -0.064, -0.990, 0),
        v(0,0,0,1)
      )
      assert(ret ~= exp)
    }
  }
}
