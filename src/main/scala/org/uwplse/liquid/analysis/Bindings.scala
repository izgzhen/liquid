package org.uwplse.liquid.analysis

/* Created at 2/15/20 by zhen */
sealed abstract class Bindings extends Product with Serializable with Iterable[Binding] {
  def sum(bs2: Bindings): Bindings
  def prod(bs2: Bindings): Bindings
  def getKeys: Set[String]
  def extend(b: Binding): Bindings
}

object Bindings {
  final case class Zero() extends Bindings {
    override def sum(bs2: Bindings): Bindings = bs2
    override def prod(bs2: Bindings): Bindings = this
    override def iterator: Iterator[Binding] = Iterator.empty
    override def getKeys: Set[String] = throw new RuntimeException("Getting keys of " + this)
    override def extend(b: Binding): Bindings = this
  }

  final case class NonEmpty(override val head: Binding, override val tail: List[Binding]) extends Bindings {
    override def sum(bs2: Bindings): Bindings = {
      bs2 match {
        case Zero() => this
        case NonEmpty(h2, t2) =>
          assert(head.m.keySet == h2.m.keySet)
          NonEmpty(head, tail ++ List(h2) ++ t2)
      }
    }

    override def prod(bs2: Bindings): Bindings = {
      val matches = this.iterator.flatMap(m1 => bs2.iterator.flatMap(m2 => {
        m1.prod(m2)
      })).toList
      if (matches.nonEmpty) {
        NonEmpty(matches.head, matches.tail)
      } else {
        Zero()
      }
    }

    override def iterator: Iterator[Binding] = Iterator.single(head) ++ tail.iterator

    override def getKeys: Set[String] = head.m.keySet

    override def extend(b: Binding): Bindings = {
      val extended = (head::tail).map(_.prod(b)).filter(_.isDefined).map(_.get)
      extended match {
        case h::t => NonEmpty(h, t)
        case _ => Zero()
      }
    }
  }

  def one(): Bindings = NonEmpty(Binding.one(), List())
  def from(bs: Iterable[Binding]): Bindings = {
    bs.map(x => Bindings.NonEmpty(x, List())).fold(Zero()){ case (x, y) => x.sum(y) }
  }
}