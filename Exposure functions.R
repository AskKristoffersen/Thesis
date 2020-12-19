
###### Expected Positive exposure ######
#Value=Interest rate swaps values(matrix)
EPE<-function(Value)
{
  EAD<-matrix(NA,dim(Value)[1],dim(Value)[2]) ##Exposure at default matrix
  EPE<-rep(NA,dim(Value)[1])  ##Expected postive exposure(EPE) matrix
  
  ###Calculates Exposure at Default ###
  for(i in 1:dim(Value)[1])
  {
    for(j in 1:dim(Value)[2])
    {
      EAD[i,j]<-max(Value[i,j],0)
    }
  }
  
  ### calculates EPE ###
  for(i in 1:dim(Value)[1])
  {
    EPE[i]<-1/dim(Value)[2]*sum((EAD[i,]))
  }
  
  return(EPE)
}

##### Expected Negative exposure ######
#Value=Interest rate swaps values(matrix)

ENE<-function(Value)
{
  NEAD<-matrix(NA,dim(Value)[1],dim(Value)[2]) ##Exposure at default matrix
  ENE<-rep(NA,dim(Value)[1])  ##Expected postive exposure(EPE) matrix
  
  ###Calculates negative Exposure at Default ###
  for(i in 1:dim(Value)[1])
  {
    for(j in 1:dim(Value)[2])
    {
      NEAD[i,j]<-min(Value[i,j],0)
    }
  }
  
  ### calculates ENE ###
  for(i in 1:dim(Value)[1])
  {
    ENE[i]<-1/dim(Value)[2]*sum((EAD[i,]))
  }
  
  return(ENE)
}


##### Expected discounted exposure #####
####Inputs ###
#Value=Interest rate swap values(matrix)
#ZCB=Discount curve
#dt=timestep
#a,sigma=Hull-White parameters
#r=interest rate at time zero
#eps= small time step

EDE<-function(Value, ZCB,dt, a, sigma, r, eps)
{
  EAD<-matrix(NA,dim(Value)[1],dim(Value)[2]) ##Exposure at default matrix
  EDE<-rep(NA,dim(Value)[1]) 
  ###Calculates Exposure at Default ###
  for(i in 1:dim(Value)[1])
  {
    for(j in 1:dim(Value)[2])
    {
      EAD[i,j]<-HUllWhiteZCBprices(0,(i-1)*dt, a, sigma, r[1,1], ZCB, eps)*max(Value[i,j],0)
    }
  }
  
  ### calculates EPE ###
  for(i in 1:dim(Value)[1])
  {
    EDE[i]<-1/dim(Value)[2]*sum((EAD[i,]))
  }
  
  return(EDE)
  
}

####Expected Discounted negative exposure #####
#Value=Interest rate swap values(matrix)
#ZCB=Discount curve
#dt=timestep
#a,sigma=Hull-White parameters
#r=interest rate at time zero
#eps= small time step


EDNE<-function(Value, ZCB,dt, a, sigma, r, eps)
{
  EAD<-matrix(NA,dim(Value)[1],dim(Value)[2]) ##Exposure at default matrix
  EDE<-rep(NA,dim(Value)[1]) 
  ###Calculates Exposure at Default ###
  for(i in 1:dim(Value)[1])
  {
    for(j in 1:dim(Value)[2])
    {
      EAD[i,j]<-HUllWhiteZCBprices(0,(i-1)*dt, a, sigma, r[1,1], ZCB, eps)*min(Value[i,j],0)
    }
  }
  
  ### calculates EPE ###
  for(i in 1:dim(Value)[1])
  {
    EDE[i]<-1/dim(Value)[2]*sum((EAD[i,]))
  }
  
  return(EDE)
  
}

#### Potential future exposure #####
#Value=Interest rate swap values(matrix)

PFE<-function(Value)
{
  EAD<-matrix(NA,dim(Value)[1],dim(Value)[2]) ##Exposure at default matrix
  
  ###Calculates Exposure at Default ###
  for(i in 1:dim(Value)[1])
  {
    for(j in 1:dim(Value)[2])
    {
      EAD[i,j]<-max(Value[i,j],0)
    }
  }
  
  ### calculates PFE ###
  PFE<-apply(EAD,1, quantile,probs=0.975)

  return(PFE)
}


#### Potential future negative exposure #####
PFNE<-function(Value)
{
  NEAD<-matrix(NA,dim(Value)[1],dim(Value)[2])
  
  for(i in 1:dim(Value)[1])
  {
    for(j in 1:dim(Value)[2])
    {
      NEAD[i,j]<-min(Value[i,j],0)
    }
  }
  
  ### calculates PFNE ###
  PFNE<-apply(NEAD,1, quantile,probs=0.025)
  
  return(PFNE)
}

