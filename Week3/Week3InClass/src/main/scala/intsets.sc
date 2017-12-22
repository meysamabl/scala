abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def incl(x: Int) = new NonEmpty(x, Empty, Empty)

  def contains(x: Int) = false

  override def toString = "."

  override def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int) =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def contains(x: Int) =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  override def toString: String =
    "{" + left + elem + right + "}"

  override def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem
}

val s1 = new NonEmpty(3, Empty, Empty)
val s2 = s1 incl 4
val s3 = s2 incl 6
