package wolfendale.scalacheck.regexp.data

sealed abstract class SetGroup[A] {

  type C

  def compliment: C

  def intersect(that: SetGroup[A]): SetGroup[A]

  def ++(that: SetGroup[A]): SetGroup[A]

  def --(that: SetGroup[A]): SetGroup[A]
}

object SetGroup {

  final case class Inclusion[A](values: Set[A]) extends SetGroup[A] {

    type C = Exclusion[A]

    override lazy val compliment: Exclusion[A] = Exclusion(values)

    override def intersect(that: SetGroup[A]): SetGroup[A] =
      that match {
        case Inclusion(other) => Inclusion(values.intersect(other))
        case Exclusion(other) => Inclusion(values -- other)
      }

    override def ++(that: SetGroup[A]): SetGroup[A] =
      that match {
        case Inclusion(other) => Inclusion(values ++ other)
        case Exclusion(other) => Exclusion(other -- values)
      }

    override def --(that: SetGroup[A]): SetGroup[A] =
      that match {
        case Inclusion(other) => Inclusion(values -- other)
        case Exclusion(other) => Inclusion(values.intersect(other))
      }
  }

  final case class Exclusion[A](values: Set[A]) extends SetGroup[A] {

    type C = Inclusion[A]

    override lazy val compliment: Inclusion[A] = Inclusion(values)

    override def intersect(that: SetGroup[A]): SetGroup[A] =
      that match {
        case Inclusion(other) => Inclusion(values -- other)
        case Exclusion(other) => Exclusion(values ++ other)
      }

    override def ++(that: SetGroup[A]): SetGroup[A] =
      that match {
        case Inclusion(other) => Exclusion(values -- other)
        case Exclusion(other) => Exclusion(values.intersect(other))
      }

    override def --(that: SetGroup[A]): SetGroup[A] =
      that match {
        case Exclusion(other) => Exclusion(other -- values)
        case Inclusion(other) => Exclusion(values ++ other)
      }
  }
}