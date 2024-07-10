// Phase 1: control is known
// With control bias a multiplication

data {
 // int ntr;                  // number of trials
  int ntr_phase;            // maximum trial number per phase (or block)
  int nrow;                 // number of rows of whole data set (equal to ntr * nsub)

  int active_trial[nrow];
  // features of trial, shown to participant
  real control_level[nrow]; // known true control level
  real feedback[nrow];
  int phase_trial[nrow];    // trial number (when first trial new game, want to reset beliefs) - assuming it's counted as 1 to 11, then starting again at 1 for next mini game 


// The parameters are now given to the model as data
  real<lower=0> rating_noise; 
  real<lower=0,upper=1>learning_rate;
  real<lower=0,upper=100>prior_self;
  real<lower=0,upper=100>prior_other;
}

generated quantities {
  real RateSelf[nrow];    
  real RateOther[nrow];   
  
  { // to prevent creating huge output files, everything we dont need, is put inside this bracket
  // Define temporary variables
  real belief_self[nrow];
  real belief_other[nrow];
  real belief_total[nrow];
  real PE_total[nrow];
  real PE_self[nrow];
  real PE_other[nrow];

 
  // Learning model: how we update beliefs based on outcomes
  for (itr in 1:nrow){
    // Is it the first trial for this game, then we set the starting belief to their prior, and the perceived control to the true one
    if (phase_trial[itr] == 1){
      belief_self[itr]       = prior_self;
      belief_other[itr]      = prior_other;
    } 
    // Predict trial outcome
    if (active_trial[itr]!=1){ // trials where you don't make a mistake on purpose
      belief_total[itr] = control_level[itr] * belief_self[itr] + (1-control_level[itr])*belief_other[itr]; /// why 
    } else {
      belief_total[itr] = control_level[itr] * 0 + (1-control_level[itr])*belief_other[itr];
    }
    PE_total[itr]       = feedback[itr] - belief_total[itr];
  
    // split up total PE according to perceived_control
    PE_self[itr]      = PE_total[itr] *  control_level[itr]; // rmv for now perceived_control[itr];
    PE_other[itr]     = PE_total[itr] - PE_self[itr];
 
    // Update beliefs for next trial
    if (phase_trial[itr] < ntr_phase){
      belief_self[itr+1]  = belief_self[itr]  + learning_rate*PE_self[itr];
      belief_other[itr+1] = belief_other[itr] + learning_rate*PE_other[itr];
    }
  }
  
  // Decision model: mapping beliefs to ratings
  // more efficient code\
  RateSelf = normal_rng(belief_self,rating_noise);
  RateOther = normal_rng(belief_other,rating_noise);
}
}
