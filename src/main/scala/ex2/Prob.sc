

sealed trait OmegaDe

case class OmegaDe2(i: Int) extends OmegaDe {
  require(i > 0 && i < 7)
}


val de: Prob[OmegaDe2] = choose((1 to 6).map(OmegaDe2.apply))
val dePipe: Prob[OmegaDe2] = or(Prob.pure(OmegaDe2(6)), de)