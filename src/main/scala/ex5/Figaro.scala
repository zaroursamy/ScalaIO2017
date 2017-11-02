package ex5

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.sampling.MetropolisHastings
import com.cra.figaro.language._
import com.cra.figaro.library.compound.{CPD, CPD2}

sealed trait Piece
case object Pile extends Piece
case object Face extends Piece

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


object Burglary {
  Universe.createNew()

  private val burglary = Flip(0.01)

  private val earthquake = Flip(0.0001)

  private val alarm: CPD2[Boolean, Boolean, Boolean] = CPD(burglary, earthquake,
    (false, false) -> Flip(0.001),
    (false, true) -> Flip(0.1),
    (true, false) -> Flip(0.9),
    (true, true) -> Flip(0.99))

  private val johnCalls = CPD(alarm,
    false -> Flip(0.01),
    true -> Flip(0.7))

  def main(args: Array[String]) {
    val alg1 = VariableElimination(burglary, earthquake)
    alg1.start()
    println("Probability of burglary: " + alg1.probability(burglary, true))
    //=> Probability of burglary: 0.01

    alg1.kill

    johnCalls.observe(true)
    val alg = VariableElimination(burglary, earthquake)
    alg.start()
    println("Probability of burglary | john calls: " + alg.probability(burglary, true))
    //=> Probability of burglary | john calls: 0.3733781172643905
    alg.kill

    println(MetropolisHastings.probability(burglary, true))
    //=>0.009514
    johnCalls.observe(true)
    println(MetropolisHastings.probability(burglary, true))
    //=>0.3644864673490166
  }
}