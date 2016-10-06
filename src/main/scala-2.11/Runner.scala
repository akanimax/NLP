import NLP.MarkovModels.{Emission, HMM, Transition}

/**
  * Created by botman on 6/10/16.
  */
object Runner extends App{

  val model_1 = HMM(Transition(Array("Hot", "Cold")),
    Emission(List("1", "2", "3"), 2))

    println(model_1.likelihood(List("3", "1", "3")))
}
