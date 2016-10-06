package NLP.MarkovModels

import scala.util.Random

/**
  * Api for hidden markov models
  * Created by botman on 4/10/16.
  */


/**
  * The states and their transitions for the Markov model
  */
case class Transition (st_labels: States, mat: Array[Array[Double]]) {
  require(mat forall (x => x forall (y => y <= 1 && y >= 0)), "Value greater than 1 or -ve") // condition of probability
  require(mat forall (x => x.sum <= 1), "Condition of trasnistion") // condition of the Transition probabilities


  // parametrised constructor of the class (Randomly initialize the probability matrix)
  def this(st_labels: States) = this(st_labels,
    Array.fill(st_labels.length + 1, st_labels.length) {
      Random.nextDouble()
    } map(x => x map(y => y / x.sum))
  )
}

// Override the companion object
object Transition {
  def apply(st_labels: States): Transition = new Transition(st_labels)
}

/**
  * The Emission store for the HMM.
  */
case class Emission (obs_vocab: Observations, noSta: Int, mat: Array[Array[Double]]) {
  require(mat forall (x => x forall (y => y <= 1 && y >= 0)), "Value greater than 1 or -ve") // condition of probability
  require(mat forall (x => (1 - x.sum) <= 0.001), "Condition of Emission")

  // parametrised constructor of the class (Randomly initialize the probability matrix)
  def this(obs_vocab: Observations, noSta: Int) = this(obs_vocab, noSta,
    Array.fill(noSta, obs_vocab.length) {
      Random.nextDouble()
    } map (x => x map(y => y / x.sum))
  )
}

// Override the companion object
object Emission {
  def apply(obs_vocab: Observations, noSta: Int): Emission = new Emission(obs_vocab, noSta)
}


/**
  * HMM has three methods.
  */
case class HMM(A: Transition, B: Emission) {

  /**
    * @param obs_seq := A list of obeservations from the obs_vocab
    * @return := The probability of the given observation sequence
    */

  def likelihood(obs_seq: Observations): Double = {

    // funciton to perform the forward computations
    def forward(prev: Array[Double], curr: Observations): Double = curr match {
      case List() => prev.sum
      case x :: xs => forward(Array.range(0, prev.length)
          .map(y => transform(prev, y, B.obs_vocab.indexOf(x))), xs)
    }

    def transform(arr: Array[Double], s_ind: Int, obs_ind: Int): Double = {
      (for (i <- arr.indices)
        yield arr(i) * A.mat(i + 1)(s_ind) * B.mat(s_ind)(obs_ind)).sum
    }

    // call the forward function to get the answer
    val initial = (for {
      i <- A.mat(0).indices
    } yield A.mat(0)(i) * B.mat(i)(B.obs_vocab.indexOf(obs_seq.head))).toArray

    forward(initial, obs_seq.tail)
  }

  /**
    * @return := String representation of the A matrix
    */
  def printA: String = "\n" + ((A.mat map (x => x mkString " ")) mkString "\n")

  /**
    * @return := String representation of the B matrix
    */
  def printB: String = "\n" + ((B.mat map (x => x mkString " ")) mkString "\n")


  /**
    * @param obs_seq := sequence of observations
    * @return := sequence of most likely hidden states
    */
  def decode(obs_seq: Observations): List[String] = {
    // funciton to perform the forward computations
    def viterbi(prev: Array[Double], acc: States, curr: Observations): States = curr match {
      case List() => acc ++ Array(maxState(prev))
      case x :: xs => viterbi(Array.range(0, prev.length)
        .map(y => transform(prev, y, B.obs_vocab.indexOf(x))), acc ++ List(maxState(prev)), xs)
    }

    def maxState(xs: Array[Double]): String = A.st_labels(xs.zipWithIndex.maxBy(_._1)._2)

    def transform(arr: Array[Double], s_ind: Int, obs_ind: Int): Double= {
      (for (i <- arr.indices)
        yield arr(i) * A.mat(i + 1)(s_ind) * B.mat(s_ind)(obs_ind)).max
    }

    // call the forward function to get the answer
    val initial = (for {
      i <- A.mat(0).indices
    } yield A.mat(0)(i) * B.mat(i)(B.obs_vocab.indexOf(obs_seq.head))).toArray

    viterbi(initial, Array(), obs_seq.tail).toList
  }

  // TODO implement the train function for the HMM
  def train(obs_seq: Observations): Unit = ???

}
