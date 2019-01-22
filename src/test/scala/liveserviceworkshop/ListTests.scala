package liveserviceworkshop

import org.scalatest.{MustMatchers, WordSpec}

class ListTests extends WordSpec with MustMatchers {

  val list1 = 1 :: 2 :: 3 :: Nil
  val list2 = 4 :: 5 :: 6 :: Nil

  ":::" should {
    "prepend list" in {

      List(1, 2, 3) ::: List(4, 5, 6)
      list1 ++ list2 mustBe 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
    }
  }
}