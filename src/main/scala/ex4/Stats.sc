import ex2.Prob

//import ex2.Prob
//
//import scala.util.Random.nextDouble
//
//sealed trait Piece
//case object Pile extends Piece
//case object Face extends Piece
//
//val pieceSimple: Prob[Piece] = Prob.fromGet(() => if (nextDouble() < 0.5) Pile else Face)
//
//val gain: Prob[Double] = pieceSimple.map({
//  case Pile => 0.5
//  case _ => 0
//})
//
//s"mediane = ${gain.median}"
////=>mediane = 0.5
//s"esp = ${gain.esp}"
////=>esp = 0.25023924976075024
//s"kurtosis = ${gain.kurtosis}"
////=>kurtosis = 1.0000175238266984
//s"asym = ${gain.asym}"
////=>asym = 0.00120599742258425
//s"variance = ${gain.variance}"
////=>variance = 0.06250006838873595
//s"std = ${gain.std}"
////=>std = 0.25000003473970833
//
//val normalAge = Prob.generateNormal(30,5)
//s"mediane = ${normalAge.median}"
////=>mediane = 30.002552912237398
//s"esp = ${normalAge.esp}"
////=>esp = 29.99329357110289
//s"kurtosis = ${normalAge.kurtosis}"
////=>kurtosis = 3.016174522762754
//s"asym = ${normalAge.asym}"
////=>asym = 0.006522548889749082
//s"variance = ${normalAge.variance}"
////=>variance = 25.012177563579556
//s"std = ${normalAge.std}"
////=>std = 5.000275776151668
//
//Piece = {Pile, Face}
sealed trait Piece
case object Pile extends Piece
case object Face extends Piece

val probPiece: Prob[Piece] = Prob.choose(Seq(Pile,Face))

val densityPiece: Map[Piece, Double] = probPiece.density()
//=> Map(Face -> 0.498954, Pile -> 0.501046)

def gainPiece(p:Piece): Int = p match {
  case Pile => 1
  case Face => 0
}
val probGain: Prob[Int] = probPiece.map(gainPiece)
val prob1euro: Double = probGain.prob(_==1)
//=> 0.499237

