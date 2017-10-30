import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.learning.EMWithBP
import com.cra.figaro.algorithm.sampling.MetropolisHastings
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.{AtomicBeta, Beta, Normal}
import com.cra.figaro.library.compound.If
import com.cra.figaro.patterns.learning.{ModelParameters, ParameterCollection}

/**
  * Created by zarour on 03/10/2017.
  */

object ExampleFunctor {
  def main(args: Array[String]): Unit = {

    def exampleFunctor() = {

      val pileFace = Select(0.499 -> 'pile, 0.499 -> 'face, 0.002->'tranche)

      def myFonctor(mu:Element[Symbol]): Element[Int] = mu.map{ result =>
        if(result == 'pile) 1 else 0
      }

      println(VariableElimination.probability(pileFace, 'pile))
      println(1 - VariableElimination.probability(pileFace, 'pile))

      println(VariableElimination.probability(myFonctor(pileFace), 1))
      println(VariableElimination.probability(myFonctor(pileFace), 0))

    }

    def exampleMonad() = {

      val p1 = Select(0.5 -> 'pile, 0.5 -> 'face)
      val p2 = Select(1.0 -> 'pile, 2.0->'face)

      val p3 = Select(1.0 -> p1, 3.0 -> p2)
    }

    def exampleConcret() = {
      val probOrage = Flip(0.05)


      val temps = Select(0.5 -> "pluie", 0.4 -> "beau", 0.1 -> "vent")

      val tauxHumidite = Normal(0.6, 0.01*0.01)

      val ejectionOrage: Element[Boolean] = If(probOrage, true, false)

      val ejectionHumidite: Element[Boolean] = tauxHumidite.flatMap(t => if(t>0.8) Flip(0.9) else Flip(0.001))

      val ejectionSure = Chain(temps,
        tauxHumidite,
        (tps:String, hum:Double) => if(tps=="pluie") ejectionOrage else {
          if (hum > 0.5) ejectionHumidite else ejectionOrage
        })




      println("P(ejection) = " + VariableElimination.probability(ejectionSure, true))


      temps.observe("beau")
      println("P(ejection | temps=beau) = " + VariableElimination.probability(ejectionSure, true))

      println("****")
      temps.unobserve()
      temps.observe("vent")
      println("P(ejection | temps=vent) = " + VariableElimination.probability(ejectionSure, true))


      temps.observe("vent")
      println("P(ejection | temps=vent deux fois) = " + VariableElimination.probability(ejectionSure, true))


      println("****")
      temps.unobserve()
      temps.observe("pluie")
      println("P(ejection | temps=pluie) = " + VariableElimination.probability(ejectionSure, true))
      temps.observe("pluie")
      println("P(ejection | temps=pluie deux fois) = " + VariableElimination.probability(ejectionSure, true))

    }


    exampleFunctor()
  }
}

object FairCoinTest extends App{


  class Trials(val parameters: ParameterCollection) {
    val trials: Seq[Flip] = for (f <- 0 until 86) yield {
      Flip(parameters.get("fairness"))
    }
  }

  def test() = {
    Universe.createNew()

    /*
   * This is an easy way of representing 86 coin flips.
   * An 'H' represents 'heads' and 'T' represents tails.
   * There are 62 'H's and 24 'T's.
   */
    val data = Seq(
      'H', 'H', 'H', 'T', 'H', 'H', 'T', 'H', 'T', 'H', 'H', 'T', 'H',
      'T', 'H', 'H', 'T', 'H', 'T', 'T', 'H', 'T', 'H', 'H', 'T', 'H', 'H', 'H', 'T',
      'T', 'H', 'H', 'T', 'H', 'T', 'H', 'T', 'T', 'H', 'T', 'H', 'H', 'H', 'H', 'H',
      'H', 'H', 'H', 'H', 'T', 'H', 'T', 'H', 'H', 'T', 'H', 'H', 'H', 'H', 'H',
      'H', 'T', 'H', 'H', 'H', 'T', 'H', 'H', 'H', 'H', 'H', 'H', 'H',
      'H', 'H', 'H', 'H', 'H', 'H', 'H', 'T', 'H', 'T',
      'H', 'H', 'H')

    val params: ModelParameters = ModelParameters()
    val fairness: AtomicBeta = Beta(2.0, 2.0)("fairness", params)
    val model: Trials = new Trials(params.priorParameters)

   println("Probabilité que le paramètre soit entre 0.4 et 0.6 = " + MetropolisHastings.probability(fairness, (d:Double) => d>0.4 && d<0.6))


    data zip model.trials foreach {
      (datum: (Char, Flip)) => if (datum._1 == 'H') datum._2.observe(true) else datum._2.observe(false)
    }

    val numberOfEMIterations = 10
    val numberOfBPIterations = 10
    val learningAlgorithm = EMWithBP(10, 10, params)
    learningAlgorithm.start
    learningAlgorithm.stop
    learningAlgorithm.kill
    /*
     * This will create a flip having a probability of 'true' learned from the input data.
     */
    println("The probability of a coin with this fairness showing 'heads' is: ")
    println(params.posteriorParameters.get("fairness"))


    System.exit(0)

    val t1 = Flip(params.posteriorParameters.get("fairness"))
    val t2 = Flip(params.posteriorParameters.get("fairness"))

    val equal = t1 === t2

    val alg = VariableElimination(equal)
    alg.start()
    println("The probability of two coins which exhibit this fairness showing the same side is: " + alg.probability(equal, true))

//    println(alg.probability(equal, true)  == (0.60 +- (0.01)))

    alg.kill()

  }

  test
}


