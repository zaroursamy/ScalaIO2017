import ex1.Mesurable
import ex2.Prob
import ex2.Prob._

trait Bucketizer[T] {
  def toBucket(t: T): T
}

object Bucketizer {
  def idBucketizer[T]: Bucketizer[T] = new Bucketizer[T] {
    override def toBucket(t: T): T = t
  }
}




sealed trait OmegaDe

case class OmegaDe2(i: Int) extends OmegaDe {
  require(i > 0 && i < 7)
}


object Test {

  import scala.util.Random


  val piece: Prob[Symbol] = Prob.fromGet[Symbol](() => {
    if (scala.util.Random.nextDouble() > 0.5) 'pile else 'face
  })

  val pieceHard = new Prob[Symbol] {
    override def get: Symbol = {
      val unif3 = Random.nextDouble()

      if (unif3 < 0.998) {
        piece.get
      } else {
        'tranche
      }

    }
  }

  println("P(tranche) = " + pieceHard.prob(_ == 'tranche))




  val de: Prob[OmegaDe2] = choose((1 to 6).map(OmegaDe2.apply))
  val dePipe: Prob[OmegaDe2] = or(Prob.pure(OmegaDe2(6)), de)


  def main(args: Array[String]): Unit = {

//
//    println("Uniform")
//    println(Prob.uniform.esp)
//    println(Prob.uniform.variance)
//    println(Prob.uniform.std)
//    println(Prob.uniform.asym)
//    println(Prob.uniform.median)
//
//    println()
//    println("Normal")
//    println(Prob.normal.esp)
//    println(Prob.normal.variance)
//    println(Prob.normal.std)
//    println(Prob.normal.asym)
//    println(Prob.normal.median)

//    System.exit(0)
//
//    import breeze.plot._
//
//    import breeze.linalg._
//
//    val f = Figure()
//    val p = f.subplot(0)
//    val x = linspace(0.0, 1.0)
//    p += plot(x, x :^ 2.0)
//    p += plot(x, x :^ 3.0, '.')
//    p.xlabel = "x axis"
//    p.ylabel = "y axis"
//    f.saveas("lines.png")
//
//
//    dePipe.samples(100).foreach(println)
//
//
//    Thread.sleep(1000000)
//    System.exit(0)


    //    piece.sample(100).foreach(println)
    //
    //
    //    println(piece.map(s => if(s=='pile) 1 else 0).sample(100).sum)

//    println(piece.prob(_ == 'pile))

//    val fonctorExample = pieceHard.map(p => if (p == 'pile) 1.0 else 0)
//    println("P(gain=1euro)" + fonctorExample.prob(_ == 1) + ", P(gain=0) = " + fonctorExample.prob(_ != 1))
//
//
//    val monadExample = piece.flatMap(s => if (s == 'pile) de else dePipe)

//    monadExample.samples(10000).groupBy(_.toString).map(x => (x._1, x._2.size.toDouble / 10000)).foreach(println)



    println("exemple concret")
    val nbVisiteur = generateUniform(50, 60)


    val dist1 = generateNormal(10, 1).density(factor = (d:Double) => math.round(d))
    val dist2 = generateNormal(10, 1).density(factor= (d:Double) => d)

    dist2.foreach(println)
    println("******")
    dist1.foreach(println)
//    val dist2 = generateNormal(19, 0.5)
//
//    val res = dist1.samples(generateUniform(20, 30).get.toInt) ++ dist2.samples(generateUniform(50, 70).get.toInt)
//
//    res.foreach(println)
  }
}
