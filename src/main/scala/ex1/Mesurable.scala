package ex1

//Définition d'un espace mesurable et d'une mesure
trait Mesurable[T] {
  def mes(a: T): Double
  def mesSeq(seq: T*): Double = seq.distinct.map(mes).sum

}

object Mesurable {
  //helper
  def fromF[T](f: T => Double): Mesurable[T] = new Mesurable[T] {
    override def mes(a: T): Double = f(a)
  }
}