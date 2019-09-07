package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      k <- arbitrary[A]
      v <- oneOf(const(empty), genHeap)
    } yield insert(k, v)
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
      else loop(deleteMin(heap), res ::: List(findMin(heap)))
    }

    val x = loop(h, Nil)
    x == x.sorted
  }

  property("min of melding") = forAll { (a: H, b: H) =>
    val min = findMin(meld(a, b))
    min == Math.min(findMin(a), findMin(b))
  }

  property("3 integers") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(a, insert(b, insert(c, empty)))
    val max = Math.max(a, Math.max(b ,c))
    val h1 = deleteMin(deleteMin(h))
    findMin(h1) == max
  }

}
