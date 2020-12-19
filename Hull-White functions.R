


                                           ######Hull-White Functions ######



                                           ####### Bootstrap Zero coupon prices from swap rates ######
                                           

#### Inputs ####
#data=swap rates
#x= time 
                                        
ZCB<-function(data,x)
{
  swap<-data[[2]]/100
  maturity<-data[[1]]
  
  spline_swap<-spline(maturity,swap,maturity[length(maturity)])$y
  spline_maturity<-spline(maturity,swap,maturity[length(maturity)])$x
  l_swap<-length(spline_swap)
  
  
  SSE<-function(x)
  {
    R_theo<-rep(0,l_swap)
    for(i in 1:l_swap)
    {
      R_theo[i]<-(1-x[i])/sum(x[1:i])
    }
    SSE<-sum((spline_swap-R_theo)^2)
    return(SSE)
  }
  
  ZCB<-nlm(SSE, rep(1,l_swap))$estimate
  cubic_ZCB<-splinefun(spline_maturity,ZCB, method = "natural")
  return(cubic_ZCB)
}

                                #####Function to get the forward curve #####
###Inputs ###
#ZCB=Discount curve
#x=time


forward_f<-function(ZCB,x)
{
  return( -(log(ZCB(x+eps))-log(ZCB(x)))/eps)*100
}

                            ##### function to get the Hull-White calibrated theta ######
### Inputs ###
#eps= small step to get derivative
#a,sigma= Hull white parameters
#ZCB= discount curve
#x=time

theta_f<-function(eps,a,sigma,ZCB,x)
{
  forward<-function(ZCB,x)
  {
    return( -(log(ZCB(x+eps))-log(ZCB(x)))/eps)*100
  }
  theta<-((forward(ZCB,x+eps)-forward(ZCB,x))/(eps))+(a*forward(ZCB,x))+((sigma^2)/(2*a))*(1-exp(-2*a*x))
  return(theta)
}

                                ###### Function to get interest rate structure  #######
##### inputs #####
# eps= small difference used for getting forward rate
# a, sigma= Hull-white parameters
#r0= start rate
#n= number of simulations
# Tn= end time
#steps= number of steps from 0 to Tn
# ZCB= discount curve

Hull_White_rente<-function(eps , a , sigma , r0 , n , Tn , steps, ZCB)
{
  dt<-Tn/steps
  r <- matrix(0,steps+1,n)  # matrix to hold short rate paths
  r[1,] <- r0     #start interest rate
  
  forward<-function(x)
  {
    return( -(log(ZCB(x+eps))-log(ZCB(x)))/eps)
  }
  
  theta<-function(x,eps,a,sigma)
  {
    theta<-((forward(x+eps)-forward(x))/(eps))+(a*forward(x))+((sigma^2)/(2*a))*(1-exp(-2*a*x))
    return(theta)
  }
  
  for(j in 1:n){
    for(i in 2:(steps+1)){
      dr <-(theta(i*dt,eps,a,sigma)-a*r[i-1,j])*dt + sigma*sqrt(dt)*rnorm(1,0,1)
      r[i,j] <- r[i-1,j] + dr
    }
  } 
  
  return(r)
}

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

                                               ###### function to get Swap par rate #####
#### inputs ####
#start=start of swap in years from today
# end= maturity of swap from today in years
#tenor=time between payments
#r0= rate at time zero
#ZCB= discount curve
#eps=small time step
#a,sigma= Hull-White parameters

swap_rate_f<-function( start , end , tenor,r0, ZCB, eps, a, sigma)
{
  Payments<-seq(start,end,tenor)
  mention<-0
  counting<-HUllWhiteZCBprices(t=0,Tn=start,a,sigma,r=r0, ZCB, eps)-HUllWhiteZCBprices(t=0,Tn=end,a,sigma,r=r0, ZCB, eps)
  for(i in Payments)
  {
    current<-tenor*HUllWhiteZCBprices(t=0,Tn=i,a,sigma,r=r0, ZCB, eps)
    mention<-mention+current
  }
  return(counting/mention)
}