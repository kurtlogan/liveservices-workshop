package liveserviceworkshop

import org.scalatest.{FreeSpec, MustMatchers, WordSpec}

class ListTests extends WordSpec with MustMatchers {

  import liveserviceworkshop.List._

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  "::: prepends" should {

    "passed 2 lists" in {

      list1 ::: list2 mustBe 4 :: 5 :: 6 :: 1 :: 2 :: 3 :: Nil
      list1 ++: list2 mustBe 4 :: 5 :: 6 :: 1 :: 2 :: 3 :: Nil
    }
  }

  "++ appends" should {

    "passed 2 lists" in {

      list1 ++ list2 mustBe List(1, 2, 3, 4, 5, 6)
    }
  }

  "take" should {
    "take 3 elements" in {

      List(1, 2, 3, 4, 5, 6).take(3) mustBe List(1, 2, 3)
    }
  }

  "drop" should {
    "drop 3 elements" in {

      List(1, 2, 3, 4, 5, 6).drop(3) mustBe List(4, 5, 6)
    }
  }
}