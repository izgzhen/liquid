package org.uwplse.liquid.spec

import org.uwplse.liquid.Analysis

object Utils {
  type Binding = Map[String, SemanticVal]
  type OptBinding = Option[Map[String, SemanticVal]]
  type ScoredBinding = (Map[String, SemanticVal], Double)
  type Bindings = List[Map[String, SemanticVal]]
  type OptBindings = Option[List[Map[String, SemanticVal]]]
  type ScoredBindings = (List[Map[String, SemanticVal]], Double)

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

  type R[X] = (X, Double)

  def chooseZipMergeR[X, Y, Z](xs: List[X], ys: List[Y], f: (X, Y) => R[Z],
                               zero: R[Z], merge: (R[Z], R[Z]) => R[Z]): List[Z] =
    choose(xs, ys.size).map(chosen => {
      chosen.zip(ys).map({ case (x, y) => f(x, y) }).fold(zero)(merge)
    }).toList.sortBy(_._2).map(_._1)

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

  def similarityFromBoolean(x: Boolean): Double = if (x) { 1.0 } else { 0.0 }

  def scoredBindingFrom(y: Double): ScoredBinding = (Map(), y)
  def scoredBindingsFrom(y: Double): ScoredBindings = (List(Map()), y)

  def scoredBindingFalse(): ScoredBinding = scoredBindingFrom(0.0)
  def scoredBindingTrue(): ScoredBinding = scoredBindingFrom(1.0)

  def scoredBindingsFalse(): ScoredBindings = scoredBindingsFrom(0.0)
  def scoredBindingsTrue(): ScoredBindings = scoredBindingsFrom(1.0)

  def scoreProd(d1: Double, d2: Double): Double = d1 * d2

  def mergeScoredBinding(b1: ScoredBinding, b2: ScoredBinding): ScoredBinding = {
    val (b1_, s1) = b1
    val (b2_, s2) = b2
    mergeBinding(b1_, b2_) match {
      case Some(b) => (b, s1 * s2)
      case _ => scoredBindingFalse()
    }
  }

  def extendScoredBindings(b1: ScoredBindings, b2: ScoredBindings): ScoredBindings = {
    val (b1_, s1) = b1
    val (b2_, s2) = b2
    (b1_ ++ b2_, s1 * s2)
  }

  def scoreOptBinding(b: OptBinding): ScoredBinding = b match {
    case Some(b_) => (b_, 1.0)
    case None => scoredBindingFalse()
  }

  def scoredBindingProd(b: ScoredBinding, y: Double): ScoredBinding = (b._1, b._2 * y)


  def mergeScoredBindings(b1: ScoredBindings, b2: ScoredBindings) : ScoredBindings = {
    val (bs1, s1) = b1
    val (bs2, s2) = b2
    val matches = bs1.flatMap(m1 => bs2.flatMap(m2 => { mergeBinding(m1, m2) }))
    if (matches.nonEmpty) {
      (matches, s1 * s2)
    } else {
      scoredBindingsFalse()
    }
  }
}
