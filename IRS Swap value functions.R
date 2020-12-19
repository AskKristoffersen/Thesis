

##### function to Hull White discount curve #####
### Inputs ###
#t=time
#Tn=maturity
#a,sigma= parameters in hull-white
#r=interest rate at time t
# ZCB= Discount curve
# eps=small time step

HUllWhiteZCBprices<-function(t,Tn,a,sigma,r,ZCB,eps)
{
  
  forward<-function(x)
  {
    return( -(log(ZCB(x+eps))-log(ZCB(x)))/eps)
  }
  B<-function(t,Tn,a){1/a*(1-exp(-a*(Tn-t)))}
  ZCB<-(ZCB(Tn)/ZCB(t))*exp(B(t,Tn,a)*forward(t)-(((sigma^2)/(4*a))*(B(t,Tn,a))^2*(1-exp(-2*a*t)))-(B(t,Tn,a)*r))
  return(ZCB)
}

###### Function to get swap values of payer swap #####
### Inputs ###
#start= start of swap agreement in years
#end=maturity of swap agreement
# tenor= time between payments
#steps= number of timesteps
#n=number of simulations
#dt=timestep size
#N=Notional
#a,sigma=hull-white parameters
#fixed=fixed rate
#r=simulated rates(matrix)
# ZCB=discount curve
#eps = small timestep


PV_IRS_Payer<-function(start, end, tenor, steps, n, dt,  N, a, sigma, fixed, r, ZCB, eps)
{
  
PV_Payer<-matrix(NA,steps+1,n) #Matrix with IRS Values

Payments<-seq(start,end,tenor)
l_pay<-length(Payments)
 for(k in 1:n)
 {
   
   for(t in 1:(steps+1))
   {
    ###fixed ###
     sum<-0
     for(i in Payments[Payments>(t-1)*dt])
     {
       sum<-sum+(fixed*tenor*HUllWhiteZCBprices((t-1)*dt, i, a, sigma, r[t,k], ZCB, eps))
     }
    fixed_leg<-N*sum
    ####Floating ###
    floating_leg<-N*(HUllWhiteZCBprices((t-1)*dt, min(Payments[Payments>=(t-1)*dt]), a, sigma, r[t,k], ZCB, eps)-
                 HUllWhiteZCBprices((t-1)*dt, end, a, sigma, r[t,k], ZCB, eps))
    
    PV_Payer[t,k]<-floating_leg-fixed_leg
    
   }
}
return(PV_Payer)
}

###### Function to get swap values of Reciever swap #####
### Inputs ###
#start= start of swap agreement in years
#end=maturity of swap agreement
# tenor= time between payments
#steps= number of timesteps
#n=number of simulations
#dt=timestep size
#N=Notional
#a,sigma=hull-white parameters
#fixed=fixed rate
#r=simulated rates(matrix)
# ZCB=discount curve
#eps = small timestep


PV_IRS_Reciever<-function(start, end, tenor, steps, n, dt,  N, a, sigma, fixed, r, ZCB, eps)
{
  
  PV_Reciever<-matrix(NA,steps+1,n) #Matrix with IRS Values
  
  Payments<-seq(start,end,tenor)
  l_pay<-length(Payments)
  for(k in 1:n)
  {
    
    for(t in 1:(steps+1))
    {
      ###fixed ###
      sum<-0
      for(i in Payments[Payments>(t-1)*dt])
      {
        sum<-sum+(fixed*tenor*HUllWhiteZCBprices((t-1)*dt, i, a, sigma, r[t,k], ZCB, eps))
      }
      fixed_leg<-N*sum
      ####Floating ###
      floating_leg<-N*(HUllWhiteZCBprices((t-1)*dt, min(Payments[Payments>=(t-1)*dt]), a, sigma, r[t,k], ZCB, eps)-
                         HUllWhiteZCBprices((t-1)*dt, end, a, sigma, r[t,k], ZCB, eps))
      
      PV_Reciever[t,k]<-fixed_leg-floating_leg
      
    }
  }
  return(PV_Reciever)
}
