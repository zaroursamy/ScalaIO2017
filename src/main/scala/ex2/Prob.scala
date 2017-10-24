package ex2

import ex1.Mesurable
import scala.util.Random

// Définition d'un espace probabilisé
trait Prob[T] extends Mesurable[T] {
  self =>

  def get: T

  def samples(n: Int): Seq[T] = Stream.fill(n)(get)

  def flatMap[U](f: T => Prob[U]): Prob[U] = Prob.fromGet(() => f(self.get).get)

  def map[U](f: T => U): Prob[U] = flatMap(f.andThen(Prob.pure))

  def density(factor: T => T = identity, nb: Int = 1000000): Map[T, Double] = {
    samples(nb).groupBy(factor).mapValues(_.size.toDouble / nb)
  }

  def prob(pred: T => Boolean): Double = {
    val d: Map[Boolean, Double] = map(pred).density()
    d(true)
  }

  override def mes(a: T): Double = prob(_ == a)

}

object Prob {
  def range(start: Int, end: Int): Prob[Int] = choose(start to end)

  def pure[T](e: T): Prob[T] = fromGet(() => e)

  def fromGet[T](_get: () => T): Prob[T] = {
    new Prob[T] {
      override def get: T = _get()
    }
  }

  implicit class DoubleProp(prob: Prob[Double]) {
    val size = 1000001

    def esp: Double = prob.samples(size).sum / size

    def variance: Double = {
      val e = esp
      prob.samples(size).map(_ - e).map(x => x * x).sum / size
    }

    def std: Double = math.sqrt(variance)

    def median: Double = {
      val sortedProb: Seq[Double] = prob.samples(size).sorted
      val medSize: Int = size / 2
      if(size % 2 == 1) {
        sortedProb(medSize + 1)
      } else {
        (sortedProb(medSize + 1)  + sortedProb(medSize)) / 2
      }
    }

    def asym: Double = {
      val (e, s) = (esp, std)
      prob.samples(size).map(x => math.pow((x - e) / s, 3)).sum / size
    }
  }


  def choose[T](xs: Seq[T]): Prob[T] = Prob.fromGet(() => xs(Random.nextInt(xs.size)))
  def or[T](prods: Prob[T]*): Prob[T] = choose(prods).flatMap(identity)

  def generateNormal(mean: Double, std: Double): Prob[Double] = Prob.fromGet(() => util.Random.nextGaussian() * std + mean)
  def generateUniform(min: Double, max: Double): Prob[Double] = Prob.fromGet(() => util.Random.nextDouble() * (max - min) + min)
  //cela serait pas mieux de faire l'inverse ?
  val normal: Prob[Double] = generateNormal(0, 1)
  val uniform: Prob[Double] = generateUniform(0, 1)

}