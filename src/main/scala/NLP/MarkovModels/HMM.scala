package NLP.MarkovModels

/**
  * Api for hidden markov models
  * Created by botman on 4/10/16.
  */


/**
  * The states and their transitions for the Markov model
  */
case class Transition (st_labels: Array[State], mat: Array[Array[Double]]) {
  require(mat forall (x => x forall (y => y <= 1 && y >= 0))) // condition of probability
}

/**
  * The Emission store for the HMM.
  */
case class Emission (obs_vocab: Array[Observation], mat: Array[Array[Double]]) {
  require(mat forall (x => x forall (y => y <= 1 && y >= 0))) // condition of probability
}

/**
  * HMM has three methods.
  */
case class HMM(A: Transition, B: Emission) {
  // TODO implement the likelihood function for the HMM
  def likelihood(obs_seq: List[Observation]): Double = ???

  // TODO implement the decode function for the HMM
  def decode(obs_seq: List[Observation]): List[State] = ???

  // TODO implement the train function for the HMM
  def train(obs_seq: List[Observation]): Unit = ???

}
