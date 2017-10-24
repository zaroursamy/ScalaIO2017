import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.language.Flip
import com.cra.figaro.library.atomic.continuous.{AtomicBeta, Beta}

/**
  * Created by zarour on 23/10/2017.
  */

sealed trait Result

case object Victoire extends Result

case object Defaite extends Result

sealed trait Climate

case object Humide extends Climate

case object Sec extends Climate


case class Data(win: Result, experience: Boolean, climate: Climate)

object Data extends App {

  val data: Seq[Data] = Seq(
    Data(Victoire, true, Humide),
    Data(Victoire, false, Humide),
    Data(Victoire, true, Humide),
    Data(Victoire, false, Sec),
    Data(Victoire, true, Sec),
    Data(Victoire, true, Sec),
    Data(Defaite, false, Sec),
    Data(Victoire, false, Sec),
    Data(Defaite, false, Sec),
    Data(Defaite, false, Humide)
  )


  object Rallye {


    val N = data.size

    val victoirePriori = Beta(1,1)



  }

}
