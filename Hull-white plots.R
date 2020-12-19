###imports libraries for plotting ###
library(reshape2)
library(ggplot2)

###Import functions##

source("Hull-White functions.R")

###Imports zero rates and ZCB prices ###

zero_rates<-read.csv2('Interest rate data 2019.csv', header = T)

#Parameters

eps<-0.0001#small timestep
a<-0.1 #Hull-white parameter
sigma<-0.01 #volatility of interest rate
r0<- -0.00470352 #start rate

#simulations

n<-10000 #number of simulations
Tn<-30 #end of simulations
steps<-12*30 #timesteps
dt<-Tn/steps #size of timestep


ZCB<-splinefun(zero_rates[[1]],zero_rates[[3]]) #discount curve

###simualtes interest rate

r<-Hull_White_rente(eps,a, sigma, r0, n, Tn, steps, ZCB)

##Plots interest rate structure

mean_r<-rep(NA,steps+1)
mean_r[1]<-r0
for(i in 1:steps+1)
{
  mean_r[i]<-mean(r[i,])
}

quantiles<-apply(r,1, quantile,probs= c(0.025,0.975))
lines<-t(rbind(mean_r,quantiles))

time_vector<-seq(0,Tn,by=Tn/steps)
data_plot<- data.frame("time"=time_vector, "paths" = r[,1:20])
data_plot<- melt(data_plot,  id = c('time'))

plot_rate <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+
  ggtitle("Hull-White interest rate paths ")+xlab("Time in years")+ylab("Interest rate")+
  theme(plot.title = element_text(hjust = 0.5))
plot_rate
data_quantiles<-data.frame("time"=time_vector, "quantiles" =lines)
plot_rate+geom_line(data=data_quantiles, aes(x=time, y=quantiles.mean_r))+
  geom_line(data=data_quantiles, aes(x=time, y=quantiles.2.5.))+
  geom_line(data=data_quantiles, aes(x=time, y=quantiles.97.5.))

####Mean reversion parameter

theta<-function(x){theta_f(eps,a,sigma,ZCB,x)/a}
plot(theta,xlim=c(0,30)
     ,col="blue"
     ,main="Mean reversion parameter",
     ylab=expression(theta/a)
     ,xlab="Time in years")
grid()
#### Hull-white ZCB ###


ZCB_HW<-rep(NA,Tn)
# ZCB_Vasicek<-rep(NA,Tn)

for(i in 1:Tn)
{
  ZCB_HW[i]<-HUllWhiteZCBprices(0,i,a,sigma,r0,ZCB,eps)
  # ZCB_Vasicek[i]<-VasicekZCBprice(b,a,sigma,i,r0,0)
}
ZCB_HW_f<-splinefun(1:30,ZCB_HW)

####Plots discount curves

plot(ZCB,
     xlim=c(0,30),
     ylim=c(-1,1.5),
     col='blue',
     xlab="Time in Years",
     ylab="ZCB prices",
     main="Discount curves")
plot(ZCB_HW_f,
     add=T,
     xlim=c(0,30),
     ylim=c(-1,1.5),
     col='red')
legend("topleft",
       c("ZCB Market","ZCB Hull-White"),
       fill=c("blue","red")
)
grid()


