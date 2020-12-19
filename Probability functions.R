


###Default probability function #####
###Inputs ###
#data= CDS spreads
#LGD=Loss Given Default
#end=Last date to extrapolate to
#x=time

Default_prob<-function(data,LGD,end,x)
{
  maturity_CDS<-data[[1]]
  spread<-data[[2]]/10000
  
  last_obs<-maturity_CDS[length(maturity_CDS)]
  
  lambda<-spread/LGD
  maturity_CDS<-append(maturity_CDS,11:end)
  lambda<-append(lambda,rep(lambda[length(lambda)],(end-last_obs)))
  
  Prob<-1-exp(-lambda*maturity_CDS)
  
  Prob_f<-splinefun(maturity_CDS,Prob)
  return(Prob_f(x))
}

###Survival probability function #####
###Inputs ###
#data= CDS spreads
#LGD=Loss Given Default
#end=Last date to extrapolate to
#x=time

survival_prob<-function(data,LGD,end,x)
{
  maturity_CDS<-data[[1]]
  spread<-data[[2]]/10000
  
  last_obs<-maturity_CDS[length(maturity_CDS)]
  
  lambda<-spread/LGD
  
  maturity_CDS<-append(maturity_CDS,11:end)
  lambda<-append(lambda,rep(lambda[length(lambda)],(end-last_obs)))
  
  Prob<-exp(-lambda*maturity_CDS)
  
  Prob_f<-splinefun(maturity_CDS,Prob) 

  return(Prob_f(x))
}
