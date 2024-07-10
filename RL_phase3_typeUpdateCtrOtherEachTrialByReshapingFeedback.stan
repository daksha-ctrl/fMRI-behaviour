// Phase 1: control is known
// With control bias a multiplication

data {
 // int ntr;                  // number of trials
  int ntr_phase;            // maximum trial number per phase (or block)
  int nrow;                 // number of rows of whole data set (equal to ntr * nsub)
  // Participants' behaviour
  real RateSelf[nrow];  // alternative could be the true value from phase 2  
  real RateOther[nrow];   
  real RateControl[nrow];
  int active_trial[nrow];
  // features of trial, shown to participant
  real control_level[nrow]; // known true control level
  real feedback[nrow];
  int phase_trial[nrow];    // trial number (when first trial new game, want to reset beliefs) - assuming it's counted as 1 to 11, then starting again at 1 for next mini game 
}

// The parameters accepted by the model
parameters {
  real<lower=0> rating_noise; 
  real<lower=0,upper=1>learning_rate;
  real<lower=0,upper=100>prior_control;
  real<lower=0,upper=100>prior_other;
// remove for first model  real<lower=0,upper=1>control_bias;        // bias term to bias true control /// 1 control bias term per subject?
}

model {
  // Define temporary variables
  real belief_control[nrow];
  real belief_other[nrow];
  real feedback_control[nrow];
  // same for other
  real PE_self[nrow];
  real PE_control[nrow];
 //remove for first model real perceived_control[nrow];   // perceived control = true control altered by control bias /// don't we want to fit one perceived control per game per subject?
  
  // Priors for parameters that we will fit
  rating_noise ~ cauchy(0,5); //mu, sd 
  
  // Learning model: how we update beliefs based on outcomes
  for (itr in 1:nrow){
    // Is it the first trial for this game, then we set the starting belief to their prior, and the perceived control to the true one
    if (phase_trial[itr] == 1){
      belief_control[itr]       = prior_control;
      belief_other[itr]      = prior_other;
    } 
    
    
    
    
    
    // Predict trial outcome
    if (active_trial[itr]!=1){ // trials where you don't make a mistake on purpose
      feedback_control[itr] = (feedback[itr]-belief_other[itr])/(RateSelf[itr]-belief_other[itr]); // equation to be checked
    
     // belief_total[itr] = control_level[itr] * belief_self[itr] + (1-control_level[itr])*belief_other[itr]; /// why 
    } else {
      //belief_total[itr] = control_level[itr] * 0 + (1-control_level[itr])*belief_other[itr];
      feedback_control[itr] = (feedback[itr]-belief_other[itr])/(0-belief_other[itr]); // equation to be checked
    
    }
   // PE_total[itr]       = feedback[itr] - belief_total[itr];
   PE_control[itr] = feedback_control[itr] - belief_control[itr];
  
   // equivalent code for updating beliefs about other
    
    
    // Update beliefs for next trial
    if (phase_trial[itr] < ntr_phase){
      belief_control[itr+1] =  belief_control[itr] + learning_rate*PE_control[itr];
      belief_other[itr+1] = belief_other[itr] + learning_rate*PE_other[itr];
    }
  }

  // more efficient code\
  // change to control RateSelf ~ normal(belief_self,rating_noise);
  RateOther ~ normal(belief_other,rating_noise);
}
