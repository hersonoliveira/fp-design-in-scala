package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    oneOf(const(empty),
      for {
        k <- arbitrary[A]
        v <- oneOf(const(empty), genHeap)
      } yield insert(k, v)
    )
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    if (a > b) findMin(h) == b
    else findMin(h) == a
  }

  property("deleteMin") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sorted") = forAll { h: H =>
    def loop(heap: H, res: List[Int]): List[Int] = {
      if (heap == empty) res
      else loop(deleteMin(heap), findMin(heap) :: res)
    }

    val x = loop(h, Nil)
    x == x.sorted
  }

  property("min of melding") = forAll { (a: H, b: H) =>
    val min1 = findMin(a)
    val min2 = findMin(b)
    val h = meld(a, b)
    val min = findMin(h)
    min == min1 || min == min2
//    val min = if (min1 > min2) min2 else min1
//    findMin(h) == min
  }


}
