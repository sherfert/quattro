package quattro

sealed abstract class Color(val str: String) {
  def other: Color = this match {
    case White => Black
    case Black => White
  }
}
case object White extends Color("W")
case object Black extends Color("B")

sealed abstract class Size(val str: String)
case object Small extends Size("S")
case object Large extends Size("L")

sealed abstract class Shape(val str: String)
case object Round extends Shape("O")
case object Rectangle extends Shape("□")

sealed abstract class Hole(val str: String)
case object WithHole extends Hole("o")
case object NoHole extends Hole("_")

final case class Figure(color: Color, size: Size, shape: Shape, hole: Hole) {
  override def toString: String = {
    s"${color.str}-${size.str}-${shape.str}-${hole.str}"
  }
}

object Figure {
  def all: Set[Figure] = {
    Set(
      Figure(White, Small, Round, WithHole),
      Figure(White, Small, Round, NoHole),
      Figure(White, Small, Rectangle, WithHole),
      Figure(White, Small, Rectangle, NoHole),
      Figure(White, Large, Round, WithHole),
      Figure(White, Large, Round, NoHole),
      Figure(White, Large, Rectangle, WithHole),
      Figure(White, Large, Rectangle, NoHole),
      Figure(Black, Small, Round, WithHole),
      Figure(Black, Small, Round, NoHole),
      Figure(Black, Small, Rectangle, WithHole),
      Figure(Black, Small, Rectangle, NoHole),
      Figure(Black, Large, Round, WithHole),
      Figure(Black, Large, Round, NoHole),
      Figure(Black, Large, Rectangle, WithHole),
      Figure(Black, Large, Rectangle, NoHole)
    )
  }

  def characteristics: Seq[Figure => Any] = Seq(
    _.color,
    _.size,
    _.shape,
    _.hole
  )
}
