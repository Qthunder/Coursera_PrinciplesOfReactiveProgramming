package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("heapsort") = forAll {h:H =>
      val list = toList(h)
      list == list.sorted
  }

  property("MeldMadness") = forAll{ a :(H,H) =>
    val (left,right) = a
    val list1 = toList(left)
    val list2 = toList(right)
    (list1 ++ list2).sorted == toList(meld(left,right))
  }

  def toList(h:H) :List[A] = if (isEmpty(h)) List() else findMin(h) :: toList(deleteMin(h))

  lazy val genHeap: Gen[H] = for (
    k <- arbitrary[Int];
    m <- oneOf(const(empty),genHeap)
  ) yield insert(k,m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
