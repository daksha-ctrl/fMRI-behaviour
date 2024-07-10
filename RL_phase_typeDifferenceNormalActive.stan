// Phase 3: self is known
// There are two quite different ways you could learn. Here first, we'll do something similar to Lisa's figure 4:\
// once people have done the first active trial in a phase, they can compute the feedback difference and thus other and control. 
// before that this model cannot have a belief about control and other

data {
  int ntr_phase;            // maximum trial number per phase (or block)
  int nrow;                 // number of rows of whole data set (equal to ntr * nsub)
  // Participants' behaviour
  real RateSelf[nrow];  // in phase 3, we assume that no learning happens here the value used here could therefore either be their rating or it could be the true value which they were shown in phase 2
  real RateOther[nrow];   
  real RateControl[nrow]; // new in phase 3
  int active_trial[nrow];
  // features of trial, shown to participant
  real feedback[nrow];
  int phase_trial[nrow];    // trial number (when first trial new game, want to reset beliefs) - assuming it's counted as 1 to 11, then starting again at 1 for next mini game 
  int active_trial_hasHappenedAlreadyThisPhase[nrow]; // set to 1 for every trial AFTER the first active trial in a phase
  int first_feedback_normalTrial_thisPhase[nrow]; // this should have 4 unique values that get repeated, one for each game
  int first_feedback_activeTrial_thisPhase[nrow]; // this should have 4 unique values that get repeated, one for each game

}

// The parameters accepted by the model
parameters {
  real<lower=0> rating_noise; 
  real<lower=0,upper=1>learning_rate;
  
 // real<lower=0,upper=100>prior_self;
  //real<lower=0,upper=100>prior_other;
}

model {
  // Define temporary variables
  real belief_other[nrow];
  real belief_control[nrow];
  real exp_feedback_normalTrials[nrow];
  real exp_feedback_activeTrials[nrow];
  real PE_feedback_normalTrials[nrow];
  real PE_feedback_activeTrials[nrow];

  // Priors for parameters that we will fit
  rating_noise ~ cauchy(0,5); //mu, sd 
  
  // Learning model: how we update beliefs based on outcomes
  for (itr in 1:nrow){
    // Is it the first trial for this game, then we set the starting belief to their prior, and the perceived control to the true one
    if (phase_trial[itr] == 1){
      exp_feedback_normalTrials[itr] = first_feedback_normalTrial_thisPhase[itr];
      exp_feedback_activeTrials[itr] = first_feedback_activeTrial_thisPhase[itr];
    } 
    // Keep track of your believes about normal feedabck and active feedback
    if (active_trial[itr]!=1){ // trials where you don't make a mistake on purpose
      PE_normalTrials[itr] = feedback[itr]-exp_feedback_normalTrials[itr];
      exp_feedback_normalTrials[itr+1] = exp_feedback_normalTrials[itr] + learning_rate*PE_normalTrials[itr];
      exp_feedback_activeTrials[itr+1] = exp_feedback_activeTrials[itr];
    } else {
      PE_activeTrials[itr] = feedback[itr]-exp_feedback_activeTrials[itr];
      exp_feedback_activeTrials[itr+1] = exp_feedback_activeTrials[itr] + learning_rate*PE_activeTrials[itr];
      exp_feedback_normalTrials[itr+1] = exp_feedback_normalTrials[itr];
    }
    
    // you have estimates of control and other only when you have at least once seen both types of trials
    if (active_trial_hasHappenedAlreadyThisPhase[itr]==1){
      belief_control[itr] = (exp_feedback_normalTrials[itr]-exp_feedback_activeTrials[itr])/RateSelf[itr];
      // something like this for other
      
       RateControl[itr] ~ normal(belief_control[itr],rating_noise);
       RateOther[itr] ~ normal(belief_other[itr],rating_noise);
    }
   
  }


}
