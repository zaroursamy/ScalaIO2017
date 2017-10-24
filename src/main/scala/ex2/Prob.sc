import ex2.Prob
import ex2.Prob.{choose, or}
import scala.util.Random.nextDouble

//De = {DeValue(1), ..., DeValue(6)}
sealed trait De
case class DeValue(i:Int) extends De {
  require(i>0 && i<7)
  def value: Int = i
}

//Piece = {Pile, Face}
sealed trait Piece
case object Pile extends Piece
case object Face extends Piece

val probDe: Prob[De] = choose((1 to 6).map(DeValue.apply))

val probPiece: Prob[Piece] = Prob.fromGet(() => if(nextDouble()<0.5) Pile else Face)

def gainPiece(p:Piece): Int = p match {
  case Pile => 1
  case Face => 0
}

def f(de:De): Prob[Int] = Prob.fromGet(() => de match {
  case DeValue(imp) if imp%2==1 => probPiece.map(gainPiece)
  case DeValue(pair) => probPiece.map(p => gainPiece(p) + pair)
}).flatMap(identity)


val gains: Prob[Int] = probDe.flatMap(f)
val prob1 = gains.prob(_==1)

println(prob1)

