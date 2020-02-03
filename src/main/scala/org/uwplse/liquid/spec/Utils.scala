package org.uwplse.liquid.spec

import org.uwplse.liquid.Analysis

object Utils {
  type Binding = Map[String, SemanticVal]
  type OptBinding = Option[Map[String, SemanticVal]]
  type Bindings = List[Map[String, SemanticVal]]
  type OptBindings = Option[List[Map[String, SemanticVal]]]

  // TODO: reason about this (https://github.com/izgzhen/liquid/issues/8)
  //   compute "and" with an empty list of constraint results in an empty list
  def optBinding(b: Boolean) : OptBinding = {
    if (b) Some(Map()) else None
  }

  def optBindings(b: Boolean) : OptBindings = {
    if (b) Some(List(Map())) else None
  }

  def choose[T](l: List[T], k: Int): Iterable[List[T]] = {
    if (k == 0) {
      List()
    } else if (k == 1) {
      l.map(List(_))
    } else {
      choose(l, k-1).flatMap(chosen => l.map(x => x::chosen))
    }
  }

  def prod[A](xss: List[List[A]]): List[List[A]] = {
    xss match {
      case xs::xss2 => xs.flatMap(x => prod(xss2).map(xs2 => x::xs2))
      case _ => List(List())
    }
  }

  def chooseZipMerge[X, Y, Z](xs: List[X], ys: List[Y], f: (X, Y) => Option[Z],
                              zero: Option[Z], merge: (Option[Z], Option[Z]) => Option[Z]): List[Z] =
    choose(xs, ys.size).flatMap(chosen => {
      chosen.zip(ys).map({ case (x, y) => f(x, y) }).fold(zero)(merge)
    }).toList

  /**
   * Faster version of [[chooseZipMerge]]
   * @param xs
   * @param ys
   * @param f
   * @param zero
   * @param merge
   * @tparam X
   * @tparam Y
   * @tparam Z
   * @return
   */
  def chooseZipMerge2[X, Y, Z](xs: List[X], ys: List[Y], f: (X, Y) => Option[Z],
                               zero: Option[Z], merge: (Option[Z], Option[Z]) => Option[Z]): List[Z] = {
    val zs: List[List[Z]] = ys.map(y => xs.flatMap(x => f(x, y)))
    prod(zs).flatMap(zs2 => zs2.map(z => Some(z)).fold(zero)(merge))
  }

  def extend[K, V](bs: List[Map[K, V]], b: Map[K, V]) : List[Map[K, V]] = {
    bs.map(b1 => b1 ++ b)
  }

  def mergeBinding(m1: Binding, m2: Binding): Option[Binding] = {
    val common = m1.keySet intersect m2.keySet
    if (common.nonEmpty && !common.forall(k => Analysis.equalValue(m1(k), m2(k)))) {
      None
    } else {
      Some(m1 ++ m2)
    }
  }

  def mergeOptBinding(m1: OptBinding, m2: OptBinding): OptBinding = {
    (m1, m2) match {
      case (Some(m1_), Some(m2_)) => mergeBinding(m1_, m2_)
      case _ => None
    }
  }

  def mergeOptBindings(b1: OptBindings, b2: OptBindings) : OptBindings = {
    (b1, b2) match {
      case (Some(bs1), Some(bs2)) => {
        val matches = bs1.flatMap(m1 => bs2.flatMap(m2 => {
          mergeBinding(m1, m2)
        }))
        if (matches.nonEmpty) {
          Some(matches)
        } else {
          None
        }
      }
      case _ => None
    }
  }
}
