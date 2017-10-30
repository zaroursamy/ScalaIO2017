import ex2.Prob
import ex2.Prob.choose
import scala.util.Random.nextDouble

//De = {DeValue(1), ..., DeValue(6)}
sealed trait De
case class DeValue(i:Int) extends De {
  require(i>0 && i<7)
}

//Piece = {Pile, Face}
sealed trait Piece
case object Pile extends Piece
case object Face extends Piece

val probDe: Prob[De] = choose((1 to 6).map(DeValue.apply))

val badD = DeValue(8)

probDe.samples(100).foreach(println)

val probPiece: Prob[Piece] = Prob.fromGet(() => if(nextDouble()<0.5) Pile else Face)

def gainPiece(p:Piece): Int = p match {
  case Pile => 1
  case Face => 0
}

def f(de:De): Prob[Int] = Prob.fromGet(() => de match {
  case DeValue(imp) if imp%2==1 => probPiece.map(gainPiece)
  case DeValue(pair) => probPiece.map(p => gainPiece(p) + pair)
}).flatMap(identity)


val X: Prob[Int] = probDe.flatMap(f)
val densityX: Map[Int, Double] = X.density()
//=> Map(0 -> 0.250588, 5 -> 0.083194, 1 -> 0.249508, 6 -> 0.082922, 2 -> 0.083565, 7 -> 0.083708, 3 -> 0.082821, 4 -> 0.083694)

densityX.map(_._2).sum
//=> 0.9999999999999999

////Piece = {Pile, Face}
//sealed trait Piece
//case object Pile extends Piece
//case object Face extends Piece
//
//val probPiece: Prob[Piece] = Prob.fromGet(() => if(nextDouble()<0.5) Pile else Face)
//
//val densityPiece: Map[Piece, Double] = probPiece.density()
////=> Map(Face -> 0.498954, Pile -> 0.501046)
//
//def gainPiece(p:Piece): Int = p match {
//  case Pile => 1
//  case Face => 0
//}
//val probGain: Prob[Int] = probPiece.map(gainPiece)
//val prob1euro: Double = probGain.prob(_==1)
////=> 0.499237
//
//val distribNormal: Map[Double, Double] = Prob.generateNormal(30, 5).density((d:Double) => math.round(d))
//distribNormal.filter(x => x._1>=25 && x._1<=35).values.toSeq.sum
////=> 0.7282919999999999
//
//
//def variableRandom[Omega, E](prob:Prob[Omega],  X: Omega => E):Prob[E] = prob.map(X)
