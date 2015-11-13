package com.rdio.thor

/**
  * Created by achan on 11/13/15.
  */
object BaseUtils {
  def mergeOpt[A](a: Option[A], b: Option[A]): Option[A] = (a,b) match {
    case (None, None) => None
    case (None, Some(bv)) => Some(bv)
    case (Some(av), Some(bv)) => Some(bv)
    case (Some(av), None) => Some(av)
  }
}
