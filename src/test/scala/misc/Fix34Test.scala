package misc

class Fix34Test extends UnitSpec {
  "A Fix34 function" when {
    "given an empty array" should {
      "return an empty array" in {
        Fix34.fix34Shift(Seq.empty) shouldBe Seq.empty[Int]
      }
    }
    "given an array with only one 3 and one 4" should {
      "return [3, 4]" in {
        Fix34.fix34Shift(Seq(3,4)) shouldBe Seq(3,4)
        Fix34.fix34Shift(Seq(4,3)) shouldBe Seq(3,4)
      }
    }
    "given an array with one single element 3 or 4" should {
      "throw IllegalArgumentException" in {
        assertThrows[IllegalArgumentException] {
          Fix34.fix34Shift(Seq(3))
        }
        assertThrows[IllegalArgumentException] {
          Fix34.fix34Shift(Seq(4))
        }
      }
    }
    "given an array with no 3 and 4" should {
      "return it as is" in {
        Fix34.fix34Shift(Seq(1, 2)) shouldBe Seq(1, 2)
      }
    }
    "given an array with one matching pair of 3 and 4" should {
      "return it with 3 before 4" in {
        Fix34.fix34Shift(Seq(1, 3, 1, 4)) shouldBe Seq(1, 3, 4, 1)
        Fix34.fix34Shift(Seq(4, 3, 2, 2)) shouldBe Seq(3, 4, 2, 2)
      }
    }
  }

  "A Fix34 (Swap) function" when {
    "given an empty array" should {
      "return an empty array" in {
        Fix34.fix34Swap(Seq.empty) shouldBe Seq.empty[Int]
      }
    }
    "given an array with only one 3 and one 4" should {
      "return [3, 4]" in {
        Fix34.fix34Swap(Seq(3,4)) shouldBe Seq(3,4)
      }
    }
    "given an array with one single element 3 or 4" should {
      "throw IllegalArgumentException" in {
        assertThrows[IllegalArgumentException] {
          Fix34.fix34Swap(Seq(3))
        }
        assertThrows[IllegalArgumentException] {
          Fix34.fix34Swap(Seq(4))
        }
      }
    }
    "given an array with no 3 and 4" should {
      "return it as is" in {
        Fix34.fix34Swap(Seq(1, 2)) shouldBe Seq(1, 2)
      }
    }
    "given an array with one matching pair of 3 and 4" should {
      "return it with 3 before 4" in {
        Fix34.fix34Swap(Seq(1, 3, 1, 4)) shouldBe Seq(1, 3, 4, 1)
        Fix34.fix34Swap(Seq(3, 2, 2, 2, 4)) shouldBe Seq(3, 4, 2, 2, 2)
      }
    }
    "given an array with multiple matching pair of 3 and 4" should {
      "return it with each 3 before 4" in {
        Fix34.fix34Swap(Seq(1, 3, 1, 4, 3, 2, 5, 1, 4, 1)) shouldBe Seq(1, 3, 4, 1, 3, 4, 5, 1, 2, 1)
      }
    }
    "given an array where some 4s are before the matching 3s" should {
      "return it with each 3 before 4" in {
        Fix34.fix34Swap(Seq(1, 3, 1, 4, 4, 3, 2, 5, 1, 1)) shouldBe Seq(1, 3, 4, 1, 2, 3, 4, 5, 1, 1)
      }
    }
    "given an array where there are multiple 3s before 4s" should {
      "return it with each 3 before 4" in {
        Fix34.fix34Swap(Seq(3, 1, 3, 2, 3, 5, 4, 4, 4)) shouldBe Seq(3, 4, 3, 4, 3, 4, 1, 2, 5)
      }
    }
    "given an array where a 4 is immediately after a 3 and they are at the same level of nesting" should {
      "return it with each 3 before 4" in {
                Fix34.fix34Swap(Seq(3, 4, 2, 3, 4, 2)) shouldBe Seq(3, 4, 2, 3, 4, 2)
      }
    }
    "given an array where a 4 is immediately after a 3 but they are at different levels of nesting" should {
      "fail with IllegalArgumentException" in {
        assertThrows[IllegalArgumentException] {
          Fix34.fix34Swap(Seq(3, 4, 4, 3, 4, 3, 2))
        }
      }
    }
  }
}
