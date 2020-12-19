          
                                                 ###### Functions to calculate CVA ######
                                                 
    
 ###### Function to calculate unilateral CVA #####
####inputs ####
# t=time
# end= maturity
# dt=timestep
# LGD=Loss given default
# EDE= Expected discounted exposure(vector)
# PD=probability of default(vector)
UCVA<-function(t,end,dt,LGD,EDE,PD)
{
  steps<-seq(t,end,dt)
  sum<-0
  Prob<-rep(NA,(length(steps)))
  Prob[1]<-0
  for(i in 2:(length(steps)))
  {
      Prob[i]<-PD(steps[i])-PD(steps[i-1])
      sum<-sum+LGD*EDE[i]*Prob[i]
  }
  return(sum)
}

###### Function to calculate unilateral DVA #####
####inputs ####
# t=time
# end= maturity
# dt=timestep
# LGD=Loss given default
# EDNE= Expected discounted negative exposure(vector)
# PD=probability of default(vector)



UDVA<-function(t,end,dt,LGD,EDNE,PD)
{
  steps<-seq(t,end,dt)
  sum<-0
  Prob<-rep(NA,(length(steps)))
  Prob[1]<-0
  for(i in 2:(length(steps)))
  {
    Prob[i]<-PD(steps[i])-PD(steps[i-1])
    sum<-sum+LGD*EDNE[i]*Prob[i]
  }
  return(sum)
}

###### Function to calculate bilateral CVA #####
####inputs ####
# t=time
# end= maturity
# dt=timestep
# LGD=Loss given default
# EDE= Expected discounted exposure(vector)
# PD=probability of default(vector)
# S=Survival probability(vector)

CVA_bi<-function(t,end,dt,LGD,EDE,PD,S)
{
  steps<-seq(t,end,dt)
  sum<-0
  Prob<-rep(NA,(length(steps)))
  Prob[1]<-0
  for(i in 2:(length(steps)))
  {
    Prob[i]<-PD(steps[i])-PD(steps[i-1])
    sum<-sum+LGD*EDE[i]*Prob[i]*S(steps[i])
  }
  return(sum)
}
###### Function to calculate bilateral DVA #####
####inputs ####
# t=time
# end= maturity
# dt=timestep
# LGD=Loss given default
# EDNE= Expected discounted negative exposure(vector)
# PD=probability of default(vector)
# S=Survival probability(vector)


DVA_bi<-function(t,end,dt,LGD,EDNE,PD,S)
{
  steps<-seq(t,end,dt)
  sum<-0
  Prob<-rep(NA,(length(steps)))
  Prob[1]<-0
  for(i in 2:(length(steps)))
  {
    Prob[i]<-PD(steps[i])-PD(steps[i-1])
    
    sum<-sum+LGD*EDNE[i]*Prob[i]*S(steps[i])
  }
  return(sum)
}

###### Function to calculate bilateral CVA+DVA #####
####inputs ####
# t=time
# end= maturity
# dt=timestep
# LGD=Loss given default
# EDE= Expected discounted exposure(vector)
# EDNE= Expected discounted negative exposure(vector)
# PD1=own probability of default(vector)
# PD2=counterparty probability of default(vector)
# S1= own Survival probability(vector)
# S1= counterparty Survival probability(vector)

BCVA<-function(t,end,dt,LGD,EDE,EDNE,PD1,PD2,S1,S2)
{
  steps<-seq(t,end,dt)
  CVA<-0
  DVA<-0
  Prob1<-rep(NA,(length(steps)))
  Prob2<-rep(NA,(length(steps)))
  Survival1<-rep(NA,(length(steps)))
  Survival2<-rep(NA,(length(steps)))
  
  
  for(i in 2:(length(steps)))
  {
    Prob1[i]<-PD1(steps[i])-PD1(steps[i-1])
    Prob2[i]<-PD2(steps[i])-PD2(steps[i-1])
    CVA<-CVA+LGD*EDE[i]*Prob2[i]*S2(steps[i])
    DVA<-DVA+LGD*EDNE[i]*Prob1[i]*S1(steps[i])
  }
  return(CVA+DVA)
}