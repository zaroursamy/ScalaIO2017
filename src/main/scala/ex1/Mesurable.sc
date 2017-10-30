import ex1._

//Piece = {Pile, Face}
sealed trait Piece
case object Pile extends Piece
case object Face extends Piece
case object Empty extends Piece
case object PileFace extends Piece

val mes1: Mesurable[Piece] = Mesurable.fromF({
  case Pile => 50
  case Face => 20
})

val mes2: Mesurable[Piece] = Mesurable.fromF({
  case Pile => 0.5
  case Face => 0
})

s"mes1(Face) = ${mes1.mes(Face)}, mes1(Pile) = ${mes1.mes(Pile)}"
//=> mes1(Face) = 20.0, mes1(Pile) = 50.0
s"mes2(Face) = ${mes2.mes(Face)}, mes2(Pile) = ${mes2.mes(Pile)}"
//=> mes2(Face) = 0.0, mes2(Pile) = 0.5

def union(a:Piece, b:Piece)= (a,b) match {
  case (Pile, Face) => PileFace
  case (Pile,Empty) => Pile
  case (Face, Empty) => Face
  case (Empty, PileFace) => PileFace

}
def complementaire(a:Piece) = a match {
  case Pile => Face
  case Face => Pile
  case PileFace => Empty
  case Empty => PileFace
}

union(Pile, Face)