// Phase 1: control is known
// With control bias a multiplication

data {
 // int ntr;                  // number of trials
  int ntr_phase;            // maximum trial number per phase (or block)
  int nrow;                 // number of rows of whole data set (equal to ntr * nsub)
 // int nsub;                 // total number of participants 
 // int subID[nrow];          // which subject (schedule) which data point belongs to - not needed: code now runs a single participant
  // Participants' behaviour
  real RateSelf[nrow];    
  real RateOther[nrow];   
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
  real<lower=0,upper=100>prior_self;
  real<lower=0,upper=100>prior_other;
// remove for first model  real<lower=0,upper=1>control_bias;        // bias term to bias true control /// 1 control bias term per subject?
}

model {
  // Define temporary variables
  real belief_self[nrow];
  real belief_other[nrow];
  real belief_total[nrow];
  real PE_total[nrow];
  real PE_self[nrow];
  real PE_other[nrow];
 //remove for first model real perceived_control[nrow];   // perceived control = true control altered by control bias /// don't we want to fit one perceived control per game per subject?
  
  // Priors for parameters that we will fit
  rating_noise ~ cauchy(0,5); //mu, sd 
  
  // Learning model: how we update beliefs based on outcomes
  for (itr in 1:nrow){
    // Is it the first trial for this game, then we set the starting belief to their prior, and the perceived control to the true one
    if (phase_trial[itr] == 1){
      belief_self[itr]       = prior_self;
      belief_other[itr]      = prior_other;
    //rmv for 1st model  perceived_control[itr] = control_level[itr];
    } 
    // Predict trial outcome
    if (active_trial[itr]!=1){ // trials where you don't make a mistake on purpose
      belief_total[itr] = control_level[itr] * belief_self[itr] + (1-control_level[itr])*belief_other[itr]; /// why 
    } else {
      belief_total[itr] = control_level[itr] * 0 + (1-control_level[itr])*belief_other[itr];
    }
    PE_total[itr]       = feedback[itr] - belief_total[itr];
    
    // Biased model - people have a biased control perception depending on a positive or negative PE
   // get rid of this for now
  //  if (phase_trial[itr] != 1) {
//      if (PE_total[itr] >= 0){
  //      perceived_control[itr] = 1 * control_bias + (1 - control_bias) * control_level[itr];
    //  } else {
    //    perceived_control[itr] = 0 * control_bias + (1 - control_bias) * control_level[itr];
    //  }
  //  }
  
  
    // split up total PE according to perceived_control
    PE_self[itr]      = PE_total[itr] *  control_level[itr]; // rmv for now perceived_control[itr];
    PE_other[itr]     = PE_total[itr] - PE_self[itr];
    
    /*if (subID[itr]==1) {
      print("subID ", subID[itr]);
      print("trial ", phase_trial[itr]);
      print("perceived control ", perceived_control[itr]);
      print("true control ", control_level[itr]);
    }*/
    // Update beliefs for next trial
    if (phase_trial[itr] < ntr_phase){
      belief_self[itr+1]  = belief_self[itr]  + learning_rate*PE_self[itr];
      belief_other[itr+1] = belief_other[itr] + learning_rate*PE_other[itr];
    }
  }
  
  // Decision model: mapping beliefs to ratings
  //for (itr in 1:nrow){
  //  RateSelf[itr] ~ normal(belief_self[itr],rating_noise);
  //  RateOther[itr] ~ normal(belief_other[itr],rating_noise);
  //}
  // more efficient code\
  RateSelf ~ normal(belief_self,rating_noise);
  RateOther ~ normal(belief_other,rating_noise);
}
