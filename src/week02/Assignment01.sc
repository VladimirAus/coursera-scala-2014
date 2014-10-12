package week02

object Assignment01 {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: week02.Assignment01.Set, elem: Int)Boolean

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x: Int) => x == elem
                                                  //> singletonSet: (elem: Int)week02.Assignment01.Set
  
  def union(s: Set, t: Set): Set = (x: Int) => contains(s, x) || contains(t, x)
                                                  //> union: (s: week02.Assignment01.Set, t: week02.Assignment01.Set)week02.Assign
                                                  //| ment01.Set
  def intersect(s: Set, t: Set): Set = (x: Int) => contains(s, x) && contains(t, x)
                                                  //> intersect: (s: week02.Assignment01.Set, t: week02.Assignment01.Set)week02.As
                                                  //| signment01.Set

  def diff(s: Set, t: Set): Set = (x: Int) => contains(s, x) && !contains(t, x)
                                                  //> diff: (s: week02.Assignment01.Set, t: week02.Assignment01.Set)week02.Assignm
                                                  //| ent01.Set

  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => p(x) && s(x)
                                                  //> filter: (s: week02.Assignment01.Set, p: Int => Boolean)week02.Assignment01.S
                                                  //| et
  val s1 = singletonSet(1)                        //> s1  : week02.Assignment01.Set = <function1>
  val s2 = singletonSet(2)                        //> s2  : week02.Assignment01.Set = <function1>
  val s3 = singletonSet(3)                        //> s3  : week02.Assignment01.Set = <function1>
  val s4 = singletonSet(4)                        //> s4  : week02.Assignment01.Set = <function1>
  val s5 = singletonSet(5)                        //> s5  : week02.Assignment01.Set = <function1>
  val s = union(s1, s2)                           //> s  : week02.Assignment01.Set = <function1>
  contains(s, 1)                                  //> res0: Boolean = true
  contains(s, 3)                                  //> res1: Boolean = false
  
  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000                                //> bound  : Int = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if ((a > bound)) true
      else if (s(a)) p(a) && iter(a+1)
      else iter(a+1)
    }
    iter(-bound)
  }                                               //> forall: (s: week02.Assignment01.Set, p: Int => Boolean)Boolean

  def map(s: Set, f: Int => Int): Set =
    filter((x: Int) => ((-bound <= x) && (x <= bound)),
    (y: Int) => (s(y) && (-bound <= f(y)) && (f(y) <= bound) ))
                                                  //> map: (s: week02.Assignment01.Set, f: Int => Int)week02.Assignment01.Set
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }                                               //> toString: (s: week02.Assignment01.Set)String

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }                                               //> printSet: (s: week02.Assignment01.Set)Unit

	val s01 = union(union(s1, s3), s5)        //> s01  : week02.Assignment01.Set = <function1>
  printSet(s01)                                   //> {1,3,5}
  printSet(map(s01, x=>x*2))                      //> {1,3,5}
}