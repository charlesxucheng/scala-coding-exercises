package misc

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.language.postfixOps

/**
 * The Fix34 problem: Return an array that contains exactly the numbers as the given array, but rearranged so that every
 * 3 is immediately followed by a 4. The array contains the same number of 3's and 4's, every 3 has a number after it
 * that is not a 3, and a 3 appears in the array before any 4.
 *
 * Variations:
 * (1) Shifting 4's to behind every 3, and pushing back the rest of the elements. The implementation is fix34Shift.
 * (2) Swapping 4's with the element behind every 3. In this variation the position of 3's don't change.
 * However, under certain conditions there is no way to keep the position of 3's as you can see in the test case.
 * The implementation is fix34Swap.
 */
object Fix34 {

  case class StateShift(processed: Seq[Int], outstandingFourCount: Int, remaining: Seq[Int])

  def fix34Shift(input: Seq[Int]): Seq[Int] = fix34RecShift(StateShift(Vector.empty, 0, input)).processed

  def fix34Swap(input: Seq[Int]): Seq[Int] = fix34RecSwap(StateSwap(Vector.empty, Balanced(), input)).processed

  @tailrec
  def fix34RecShift(inState: StateShift): StateShift = {
    inState.remaining match {
      case Nil =>
        if (inState.outstandingFourCount != 0)
          throw new IllegalArgumentException("Number of 3s and number of 4s do not match")
        else inState
      case head::tail =>
        val newState = {
          if (head == 4) StateShift(
            processed = inState.processed,
            outstandingFourCount = inState.outstandingFourCount + 1,
            remaining = tail)
          else if (head == 3) StateShift(
            processed = inState.processed :+ 3 :+ 4,
            outstandingFourCount = inState.outstandingFourCount - 1,
            remaining = tail)
          else StateShift(
            processed = inState.processed :+ head,
            inState.outstandingFourCount,
            remaining = tail
          )
        }
        fix34RecShift(newState)
    }
  }

  trait State

  /**
   * @param staging the list of items that are being accumulated while the state is:
   *                (a) having one or more outstanding 3s and waiting for matching 4s, or
   *                (b) having one or more outstanding 4s and waiting for matching 3s
   * @param stagingQ The queue that holds the outstanding segments of lists. When a match is found
   */
  sealed case class MoreThrees(staging: Seq[Int], stagingQ: Queue[Seq[Int]]) extends State
  sealed case class MoreFours(staging: Seq[Int], stagingQ: Queue[Seq[Int]]) extends State
  sealed case class Balanced() extends State

  /**
   * Recursive implementation of the actual logic of Fix34 (Swap).
   * The logic using recursion is long because we need to accumulate segments of the inputs to match the other symbol.
   * @param processed Elements whose position will not change further
   * @param state What state it is currently in
   * @param remaining The remaining list of items to be processed
   */
  case class StateSwap(processed: Seq[Int], state: State, remaining: Seq[Int])

  @tailrec
  def fix34RecSwap(inState: StateSwap): StateSwap = {
    inState.remaining match {
      case Nil =>
        inState.state match {
          case Balanced() => inState
          case _ => throw new IllegalArgumentException("Number of 3s and number of 4s do not match")
        }
      case head :: tail =>
        val newState = inState.state match {
          case Balanced() =>
              if (head == 3) {
                StateSwap(
                  processed = inState.processed,
                  state = MoreThrees(Vector(head), Queue.empty),
                  remaining = tail)
              } else if (head == 4) {
                  StateSwap(
                    processed = inState.processed,
                    state = MoreFours(Vector(head), Queue.empty),
                    remaining = tail)

              } else StateSwap(
                processed = inState.processed :+ head,
                state = Balanced(),
                remaining = tail
              )
          case MoreThrees(s, sq) =>
            if (head == 3)
              StateSwap(
                processed = inState.processed,
                state = MoreThrees(Vector(head), sq :+ s),
                remaining = tail)
            else if (head == 4) {
              if (sq.isEmpty) { // The last segment in staging, so this brings the state back to Normal
                val valueToAppend =
                  if (s.size == 1) Vector(s.head, head)
                  else Vector(s.head, head) :++ s.tail.tail :+ s.tail.head
                StateSwap(
                  processed = inState.processed :++ valueToAppend,
                  state = Balanced(),
                  remaining = tail)
              }
              else { // Take first item in the Q and match it with the 4
                val (segment, remainingQ) = sq.dequeue
                assert(segment.size>1, "The segment being matched by a 4 has less than 2 elements. Actual:" + segment.size)
                assert(segment.head == 3, "The segment being matched by a 4 does not start with 3. Actual:" + segment.head)
                StateSwap(
                  processed = inState.processed :++
                    Vector(segment.head, head) :++ segment.tail.tail,
                  state = MoreThrees(s :+ segment.tail.head, remainingQ),
                  remaining = tail)
              }
            }
            else StateSwap(
              processed = inState.processed,
              state = MoreThrees(s :+ head, sq),
              remaining = tail)
          case MoreFours(s, sq) =>
            if (head == 4)
              StateSwap(
                processed = inState.processed,
                state = MoreFours(Vector(head), sq :+ s),
                remaining = tail)
            else if (head == 3) {
              assert(tail.size > 1)
              if (sq.isEmpty) { // The last segment in staging, so this brings the state back to Normal
                StateSwap(
                  processed = inState.processed :+
                    tail.head :++ s.tail :+ head :+ s.head,
                  state = Balanced(),
                  remaining = tail.tail)
              }
              else { // Take first item in the Q and match it with the 4
                val (segment, remainingQ) = sq.dequeue
                assert(segment.head == 4, "The segment being matched by a 3 does not start with 4. Actual:" + segment.head)
                StateSwap(
                  processed = inState.processed :+
                    tail.head :++ s.tail,
                  state = MoreFours(s :+ head :+ segment.head, remainingQ),
                  remaining = tail.tail)
              }
            } else StateSwap(
              processed = inState.processed,
              state = MoreFours(s :+ head, sq),
              remaining = tail
            )
        }
        fix34RecSwap(newState)
    }
  }
}
