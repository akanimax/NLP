import NLP.MarkovModels.{Emission, HMM, Transition}

/*
 * Tutorial for using Random HMM
 */

// Hidden States: Weather("Hot", "Cold")
// Observation vocab: No_of_ice_creams_eaten(1, 2, 3)
val model_1 = HMM(Transition(Array("Hot", "Cold")),
  Emission(List("1", "2", "3"), 2))

// Compute the likelihood of a sequence
model_1.likelihood(List("1", "2", "3", "2", "1"))


/*
 * Tutorial for creating an HMM with predefined values of probabs
 */

val model_2 = HMM(
  Transition(Array("Hot", "Cold"), // states
    Array(  // transition matrix
      Array(0.8, 0.2),
      Array(0.7, 0.3),
      Array(0.4, 0.6)
    )
  ),

  Emission(List("1", "2", "3"), 2, // Obs_vocab and no. of. states
    Array(  // Emission Matrix
      Array(0.2, 0.4, 0.4),
      Array(0.5, 0.4, 0.1)
    )
  )
)

// calculate the liklihood of 3 1 3
model_2.likelihood(List("3", "1", "3"))