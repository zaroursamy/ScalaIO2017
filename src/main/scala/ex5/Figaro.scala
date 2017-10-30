package ex5

import com.cra.figaro.algorithm.Values
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.sampling.MetropolisHastings
import com.cra.figaro.language._
import com.cra.figaro.library.compound.{If, ^^}
import ex2.Prob

sealed trait Piece
case object Pile extends Piece
case object Face extends Piece


object Metropolis extends App{





}
object Figaro extends App {



  val piece: Element[Piece] = Flip(0.5).map(b => if (b) Pile else Face)

  val de: Element[Int] = Select(1.0 -> 1, 1.0 -> 2, 1.0 -> 3, 1.0 -> 4, 1.0 -> 5, 1.0 -> 6)

  val gain = for {
    p <- piece
    d <- de
  } yield {
    (p, d % 2 == 0) match {
      case (Pile, true) => d +1
      case (Face, true) => d
      case (Pile, _) => 1
      case (Face, _) => 0
    }
  }

  val alg = BeliefPropagation(10, gain)
  alg.start()
  val density: Stream[(Double, Int)] = alg.computeDistribution(gain)
  val prob: Double = alg.probability(gain, (g:Int) => g > 5)
  alg.kill()

  println(density.toList)
  //List((0.08333333333333333,3), (0.25000000000000006,1), (0.08333333333333333,4), (0.08333333333333333,7), (0.08333333333333333,5), (0.08333333333333333,2), (0.25000000000000006,0), (0.08333333333333333,6))
  println(s"P(X>5) = $prob")
  //P(X>5) = 0.16666666666666666

}

object Conditionnel{

  def main(args: Array[String]): Unit = {

    def resTest(b:Boolean) = if(b) 'positif else 'negatif

    // 99% de chance d'être sain
    val probMalade: Element[Boolean] = Flip(0.001)

    // malade => 90% de chance d'etre positif. Sain => 3% de chance d'etre positif
    val probTest: If[Boolean] = If(probMalade, Flip(0.9), Flip(0.03))

    /**
      * probMalade.flatMap{ m =>
      if(m == 'malade) Flip(0.9).map(resTest) else Flip(0.03).map(resTest)
    }
      */

    val algMalade = BeliefPropagation(10, probMalade)
    val algTest = BeliefPropagation(10, probTest)

    algMalade.start()
    algTest.start()

    println(s"P(malade) = ${algMalade.probability(probMalade, true)}")

    probTest.observe(true)
    println("P(malade | positif) = " + algTest.probability(probMalade,true))

    algMalade.kill()
    algTest.kill()
  }
}
