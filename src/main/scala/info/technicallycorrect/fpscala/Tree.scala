package info.technicallycorrect.fpscala

/**
 * Chapter 3 Tree p. 86
 */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeUtils {

  def size[A](root: Tree[A]): Int = root match {
    case Leaf(value) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(root: Tree[Int]): Int = root match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](root: Tree[A]): Int = root match {
    case Leaf(value) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](root: Tree[A])(fn: A => B): Tree[B] = root match {
    case Leaf(value) => Leaf(fn(value))
    case Branch(left, right) => Branch(map(left)(fn), map(right)(fn))
  }

  def fold[A, B](root: Tree[A])(init: B)(fn: (B, A) => B): B = root match {
    case Leaf(value) => fn(init, value)
    case Branch(left, right) => fold(right)(fold(left)(init)(fn))(fn)
  }

}
