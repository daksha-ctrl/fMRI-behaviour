fit_behaviour = function(df,my.model,fitting.procedure,data.type){
  # inputs: df - data to be fitted (for one single subject)
  # my.model - stan model, already compiled
  # fitting.procedure - either sampling or VBA (much faster, but less good)
  # data.type: FMRI or iPad
  
  # outputs: singleSessionParas - parameter estimate for the person that was fitted
  
  # This code takes schedule and behaviour for one participant (real or simulated) and fits behavioural model specified in 'my.model' 
  # Initial settings for fitting
  my.rhat=5 #just to make while loop run
  count_divergent=10 # just to make while loop run
  my.adapt_delta=0.8 # setting for first iteration
  my.iter=4000 # setting for first iteration
  try.count=1
  max.tries=3 
  #max.tries.vb=10 # max tries before changing to sampling
  #my.tol_rel_obj =0.01
  done=0
  
  # make df into list
  df = as.list(df)
  # run model
  df$nrow=length(df$RateSelf)
  # fix size
  # df$last_repeat_safe=as.vector(df$last_repeat_safe)
  # df$loss_scale=as.vector(df$loss_scale)
  # df$Left_EV =as.vector(df$Left_EV)
  # df$Right_EV=as.vector(df$Right_EV)
  # df$Left_Variance = as.vector(df$Left_Variance)
  # df$Right_Variance = as.vector(df$Right_Variance)
  # df$Left_Skew = as.vector(df$Left_Skew)
  # df$Right_Skew = as.vector(df$Right_Skew) 
 # if(length(df$L_prob)<=20 | data.type=='FMRI' | data.type=='ModelComparison'){ # then there was a problem with how the session was recorded (e.g. a participant did a session at 1am and another at 7am and it's unclear whether it's the same day or a different one...)
    # Keep fitting the data until fit indices are appropriate (Rhat <1.1 and no divergent samples)
    # If the fit is not good, tune the parameters (number of iterations and adapt_delta and try again)
    
    suppressWarnings(rm(out)) # rm output if it exists, and suppress warning if it doesn't exist yet
    
    while (done==0){
      # Run the model
      if (fitting.procedure=='sampling'){
        print(paste('iterations:', my.iter,', my.adapt_delta:',my.adapt_delta, ', try count:',try.count))
        out=sampling(my.model,data=df,chains=4,iter=my.iter,control=list(adapt_delta=my.adapt_delta,max_treedepth = 15),verbose=FALSE,refresh=0)
        # check divergences, rhat and rerun if necessary
        np=nuts_params(out)
        count_divergent=sum(subset(np,Parameter=="divergent__")$Value)
        #my.rhat=max(head(rhat(out),-1)) #brms::rhat??
        my.rhat= max(as.data.frame(print(summary(out)$summary))$Rhat)
        # check whether done and increase number of iterations, adapt delta and the counter for attempts at fitting
        if(count_divergent ==0 && my.rhat<1.1){
          done=1
        } else{
          #  if (data.type=='ModelComparison' && my.rhat<1.05){ # for the exponential models, pretty difficult to get the divergent samples to go away
          #  done=1 
          #} else{
            my.iter=my.iter*1.5
            my.adapt_delta= my.adapt_delta + (1-my.adapt_delta)*0.5
            try.count=try.count+1
            print(paste('Rhat:',my.rhat,', Divergent samples:',count_divergent))
            #}
        }
        
        
      } 
      # else if (fitting.procedure=='vb'){
      #   print(paste('VARIATIONAL INFERENCE, iterations:', my.iter,', tol_rel_obj:',my.tol_rel_obj, ', try count:',try.count))
      #   # rarely with vb, error occur that terminate code - let's 'trycatch' them
      #   
      #   # (Note: tryCatch doesnt actually work here..)
      #   out = tryCatch(vb(my.model,data=df,refresh=0,iter=my.iter,tol_rel_obj=my.tol_rel_obj), 
      #                  error = function(e) paste("ErrorOccured"))
      #   
      #   
      # 
      #   if (typeof(out)=='S4'){
      #     # different checks for vb:
      #     t=summary(out,pars='lp__')$summary
      #     kvalue=t[,'khat']
      #   } else{
      #     kvalue=1
      #   }
      #   
      #   # Check whether done and Increase number of iterations and decrease tol_rel_obj
      #   if (kvalue<=0.7){
      #     done=1
      #   } else{
      #     my.iter=my.iter*1.5
      #     my.tol_rel_obj = my.tol_rel_obj*0.8
      #     try.count=try.count+1
      #     
      #     if (try.count>max.tries.vb){
      #       print('setting to sampling')
      #       try.count=0
      #       fitting.procedure='sampling'
      #       my.iter=10000
      #       try.count=1
      #       browser()
      #     }
      #   }
      #   
      # }
      # if (try.count>max.tries){
      #   break
      # }
      # 
    }
    print(paste('final try count',try.count))
    # Note down whether fitting was successful and extract parameters if successful
    par.names= names(out) # extract parameter names so that the code can be used for each model
    par.names= par.names[!par.names %in% 'lp__']
    
    if (try.count<=max.tries){
      fitted.ok.flag=1
      singleSessionParasT=rstan::extract(out,pars=par.names) # ,pars=c("invTemp","lossW","b_loss_scale","b_last_repeat_safe"))
      singleSessionParas= as.data.frame(lapply(singleSessionParasT,mean))
    } else{
      fitted.ok.flag = 0

      singleSessionParas = data.frame(matrix(NA,ncol = length(par.names), nrow = 1))
      colnames(singleSessionParas) <- par.names
    }
    
    out=list()
    out$singleSessionParas=singleSessionParas
    out$fitted.ok.flag=fitted.ok.flag
    return(out)
#  } 
}