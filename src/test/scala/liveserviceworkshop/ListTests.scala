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

  "slice" should {

    "skip 2 element and take 2 element" in {

      List(1, 2, 3, 4, 5, 6).slice(2, 4) mustBe List(3, 4, 5)
    }
  }

  "reverse" should {

    "reverse a list" in {

      List(1, 2, 3, 4, 5, 6).reverse mustBe List(6, 5, 4, 3, 2, 1)
    }
  }

  "takeRight" should {

    "take 2 element from right" in {

      List(1, 2, 3, 4, 5, 6).takeRight(2) mustBe List(5, 6)
    }
  }

  "dropRight" should {

    "drop 2 element from right" in {

      List(1, 2, 3, 4, 5, 6).dropRight(2) mustBe List(1, 2, 3, 4)
    }
  }

  "splitAt" should {

    "split at 0" in {

      List(1, 2, 3).splitAt(0) mustBe (Nil, List(1, 2, 3))
    }

    "split at 3" in {

      List(1, 2, 3, 4, 5, 6).splitAt(3) mustBe (List(1, 2, 3), List(4, 5, 6))
    }
  }

  "map" should {

    "+ 1" in {

      List(1, 2, 3, 4).map(_ + 1) mustBe List(2, 3, 4, 5)
    }
  }

  "flatMap" should {

    "return new list" in {

      List(1, 2, 3).flatMap(_ => List(4, 5)) mustBe List(4, 5, 4, 5, 4, 5)
    }
  }

  "collect" should {

    "increment even numbers" in {

      List(1, 2, 3, 4).collect {

        case 2 => 4

        case i if i % 2 == 0 => i + 1


      } mustBe List(3, 5)
    }
  }
}