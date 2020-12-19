
####Libraries used for plotting ####
library(ggplot2)
library(reshape2)

#### imports functions needed from other R files #####
source("Hull-White functions.R")
source("IRS Swap value functions.R")
source("Exposure functions.R")
source("Probability functions.R")
source("CVA functions.R")

###Import data from excel files ###

zero_rates<-read.csv2('Interest rate data 2007.csv', header = T)
CDS_Crosss<-read.csv2("ITRAXX Crossover 2007.csv", header=T)
CDS_Europe<-read.csv2("ITRAXX Europe 2007.csv", header=T)

#### Parameters ####

eps<-0.00001 ###small value for computing derivatives
a<-0.1  ##Hul-White parameter
sigma<-0.01  #volatility of interest rate
r0<- zero_rates[1,2]/100  #Rate at start
R<-0.4  #recovery rate
LGD<-1-R  # Loss Given Default

###Simulations ####

n<-100 #Number of simulations
Tn<-31 #end of simulations in year
steps<-31*12 #Number of steps
dt<-Tn/steps # size of steps

### Swap contract 1 ###

start1<-1+9/12 #Start of swap contract 1
end1<-30+9/12 #end of swap contract 2
steps1<-end1*12 #number of steps in swap 1
tenor<-3/12 #size between payments
fixed1<-0.057 #fixed rate 1
N1<-9734000 # Notional 1

### Swap contract 2 ###

start2<-9/12 # start of swap agreement 2
end2<-30+3/12 #end of swap contract 2
steps2<-end2*12 #number of steps in swap 2
tenor<-3/12 #size between payments
fixed2<-0.0555 #fixed rate 2
N2<-17413000 #Notional 2


###Get calibrated curves ###

ZCB<-splinefun(zero_rates[[1]],zero_rates[[3]]) #Makes Discount curve

PD_Cross<-function(x){Default_prob(CDS_Crosss,LGD, end1, x)} #Default probability for Crossover
S_Cross<-function(x){survival_prob(CDS_Crosss,LGD, end1,x)} #Survival probability for Crossover

PD_Europe<-function(x){Default_prob(CDS_Europe,LGD, end1, x)} #Default probability for Europe
S_Europe<-function(x){survival_prob(CDS_Europe,LGD, end1, x)} #Survival probability for Europe

swap_rate_f(start1,end1,tenor,r0,ZCB,eps,a,sigma) #Par swap rate swap 1
swap_rate_f(start2,end2,tenor,r0,ZCB,eps,a,sigma) #Par swap rate swap 2

###simulate interest rate  ####

r<-Hull_White_rente(eps,a,sigma,r0,n,Tn,steps, ZCB)


### plot interest rates ###
mean_r<-rep(NA,steps+1)
mean_r[1]<-r0
for(i in 1:steps+1)
{
  mean_r[i]<-mean(r[i,])
}
quantiles<-apply(r,1, quantile,probs= c(0.025,0.975))
lines<-t(rbind(mean_r,quantiles))

time_vector<-seq(0,Tn,by=Tn/steps)
data_plot<- data.frame("time"=time_vector, "paths" = r[,1:100])
data_plot<- melt(data_plot,  id = c('time'))

plot_rate <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable),show.legend = FALSE) +
  theme(legend.position = "none")+
  ggtitle("Hull-White interest rate paths 2007 ")+xlab("Time in years")+ylab("Interest rate ")+
  theme(plot.title = element_text(hjust = 0.5))
data_quantiles<-data.frame("time"=time_vector, "quantiles" =lines)
plot_rate+geom_line(data=data_quantiles, aes(x=time, y=quantiles.mean_r))+
  geom_line(data=data_quantiles, aes(x=time, y=quantiles.2.5.))+
  geom_line(data=data_quantiles, aes(x=time, y=quantiles.97.5.))+
  geom_hline(aes(yintercept = 0.057), color="red")+
  geom_hline(aes(yintercept = 0.0555), color="blue")+
  scale_linetype_manual(name="Fixed rate", values =2, guide=guide_legend(overide.aes=list(color="red")))+
  theme(legend.position = "right")




### calculate IRS Values ###

Value1<-PV_IRS_Payer(start1,end1, tenor, steps1,n, dt, N1, a, sigma, fixed1,  r, ZCB,  eps)
Value2<-PV_IRS_Payer(start2,end2, tenor, steps2,n, dt, N2, a, sigma, fixed2,  r, ZCB,  eps)

base_value1<-Value1[1,1]
base_value2<-Value2[1,1]

###Calculates netting value

Netting<-matrix(NA, steps1+1,n)

for(i in 1:steps1+1)
{
  for(j in 1:n)
  {
    if(i<(steps2+1))
    {
      Netting[i,j]<-Value1[i,j]+Value2[i,j]
    }
    else{Netting[i,j]<-Value1[i,j]}
  }
}
Netting[1,]<-Value1[1,1]+Value2[1,1]

netted_base_value<- Netting[1,1]
##All base values printed ##
base_value1
base_value2
netted_base_value


###Plot of Interest rate swap values separate ####
# mean_IRS<-rep(NA,steps1+1)
# mean_IRS<-Value1[1,1]
# for(i in 1:steps1+1)
# {
#   mean_IRS[i]<-mean(Value1[i,])
# }
# 
# quantiles_IRS<-apply(Value1,1, quantile,probs= c(0.025,0.975))
# lines_IRS<-t(rbind(mean_IRS,quantiles_IRS))
# 
# time_vector<-seq(0,end1,by=dt)
# data_plot<- data.frame("time"=time_vector, "paths" = Value1[,1:100])
# data_plot<- melt(data_plot,  id = c('time'))
# 
# plot_IRS_Values <- ggplot(data_plot, aes(time, value)) +
#   geom_line(aes(colour = variable)) +
#   theme(legend.position = "none")+
#   ggtitle("Interest rate swap value 1")+xlab("Time in years")+
#   scale_y_continuous(name="IRS value", labels = scales::comma)+
#   theme(plot.title = element_text(hjust = 0.5))
# 
# data_quantiles_IRS<-data.frame("time"=time_vector, "quantiles" =lines_IRS)
# 
# plot_IRS_Values+geom_line(data=data_quantiles_IRS, aes(x=time, y=quantiles.mean_IRS,linetype="dashed"))+
#   geom_line(data=data_quantiles_IRS, aes(x=time, y=quantiles.2.5.))+
#   geom_line(data=data_quantiles_IRS, aes(x=time, y=quantiles.97.5.))
# 
# mean_IRS2<-rep(NA,steps2+1)
# mean_IRS2<-Value2[1,1]
# for(i in 1:steps2+1)
# {
#   mean_IRS2[i]<-mean(Value2[i,])
# }
# 
# quantiles_IRS2<-apply(Value2,1, quantile,probs= c(0.025,0.975))
# lines_IRS2<-t(rbind(mean_IRS2,quantiles_IRS2))
# 
# time_vector2<-seq(0,end2,by=dt)
# data_plot2<- data.frame("time"=time_vector2, "paths" = Value2[,1:100])
# data_plot2<- melt(data_plot2,  id = c('time'))
# 
# plot_IRS_Values2 <- ggplot(data_plot2, aes(time, value)) +
#   geom_line(aes(colour = variable)) +
#   theme(legend.position = "none")+
#   ggtitle("Interest rate swap value 2")+xlab("Time in years")+
#   scale_y_continuous(name="IRS value", labels = scales::comma)+
#   theme(plot.title = element_text(hjust = 0.5))
# 
# data_quantiles_IRS2<-data.frame("time"=time_vector2, "quantiles" =lines_IRS2)
# 
# plot_IRS_Values2+geom_line(data=data_quantiles_IRS2, aes(x=time, y=quantiles.mean_IRS2,linetype="dashed"))+
#   geom_line(data=data_quantiles_IRS2, aes(x=time, y=quantiles.2.5.))+
#   geom_line(data=data_quantiles_IRS2, aes(x=time, y=quantiles.97.5.))


####### Plot of interest rate swap Netting value ######

mean_netting<-rep(NA,steps1+1)
mean_netting<-Netting[1,1]
for(i in 1:steps1+1)
{
  mean_netting[i]<-mean(Netting[i,])
}
length(mean_netting)
quantiles_netting<-apply(Netting,1, quantile,probs= c(0.025,0.975))
lines_netting<-t(rbind(mean_netting,quantiles_netting))

time_vector_netting<-seq(0,end1,by=dt)
data_plot_netting<- data.frame("time"=time_vector_netting, "paths" = Netting[,1:100])
data_plot_netting<- melt(data_plot_netting,  id = c('time'))

plot_IRS_Values_netting <- ggplot(data_plot_netting, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+
  ggtitle("Interest Rate Swap Portfolio value 2007")+xlab("Time in years")+
  scale_y_continuous(name="IRS value", labels = scales::comma)+
  theme(plot.title = element_text(hjust = 0.5))

data_quantiles_IRS_netting<-data.frame("time"=time_vector_netting, "quantiles" =lines_netting)

plot_IRS_Values_netting+geom_line(data=data_quantiles_IRS_netting, aes(x=time, y=quantiles.mean_netting,linetype="dashed"))+
  geom_line(data=data_quantiles_IRS_netting, aes(x=time, y=quantiles.2.5.))+
  geom_line(data=data_quantiles_IRS_netting, aes(x=time, y=quantiles.97.5.))




#### Calculates exposures ####

####Netted exposure ####

EDE_netted<-EDE(Netting,ZCB,dt,a,sigma,r,eps)
EDNE_netted<-EDNE(Netting,ZCB,dt,a,sigma,r,eps)
PFE_netted<-PFE(Netting)
PFNE_netted<-PFNE(Netting)

### Without netting ####

EAD1<-matrix(NA,dim(Value1)[1],dim(Value1)[2])
EAD2<-matrix(NA,dim(Value2)[1],dim(Value2)[2])
NEAD1<-matrix(NA,dim(Value1)[1],dim(Value1)[2])
NEAD2<-matrix(NA,dim(Value2)[1],dim(Value2)[2])

for(i in 1:dim(Value1)[1])
{
  for(j in 1:dim(Value1)[2])
  {
    EAD1[i,j]<-max(Value1[i,j],0)
    NEAD1[i,j]<-min(Value1[i,j],0)
  }
}
for(i in 1:dim(Value2)[1])
{
  for(j in 1:dim(Value2)[2])
  {
    EAD2[i,j]<-max(Value2[i,j],0)
    NEAD2[i,j]<-min(Value2[i,j],0)
  }
}
EAD_total<-matrix(NA,dim(Value1)[1],dim(Value1)[2])
NEAD_total<-matrix(NA,dim(Value1)[1],dim(Value1)[2])

for(i in 1:dim(Value1)[1])
{
  for(j in 1:dim(Value1)[2])
  {
    if(i<steps2)
    {
      EAD_total[i,j]<-HUllWhiteZCBprices(0,(i-1)*dt, a, sigma, r[1,1], ZCB, eps)*(EAD1[i,j]+EAD2[i,j])
      NEAD_total[i,j]<-HUllWhiteZCBprices(0,(i-1)*dt, a, sigma, r[1,1], ZCB, eps)*(NEAD1[i,j]+NEAD2[i,j])
    }
    else{
      EAD_total[i,j]<-HUllWhiteZCBprices(0,(i-1)*dt, a, sigma, r[1,1], ZCB, eps)*EAD1[i,j]
      NEAD_total[i,j]<-HUllWhiteZCBprices(0,(i-1)*dt, a, sigma, r[1,1], ZCB, eps)*NEAD1[i,j]}
  }
}



EDE_without<-rep(NA,dim(Value1)[1])
EDNE_without<-rep(NA,dim(Value1)[1])
for(i in 1:dim(Value1)[1])
{
  EDE_without[i]<-1/dim(Value1)[2]*sum((EAD_total[i,]))
  EDNE_without[i]<-1/dim(Value1)[2]*sum((NEAD_total[i,]))
}

### Calculates difference between netted and not netted portfolio ###
difference<-EDE_without-EDE_netted
negative_diff<-EDNE_without-EDNE_netted
length(EDE_without)

time_vector<-seq(0,end1,by=dt)

plot(time_vector,difference, type="l",
     ylab = "Difference in kr",
     xlab = "Time in years",
     main = "Difference in exposure of Nettet and Not Nettet Portfolio")

plot(time_vector,negative_diff, type="l",
     ylab = "Difference in kr",
     xlab = "Time in years",
     main = "Difference in negative exposure Between Nettet and Not Nettet Portfolio")


#### plot of exposure ####
time_vector<-seq(0,end1,by=dt)


data_exposure<-data.frame("time"=time_vector, "EDE" =EDE_netted, "PFE"=PFE_netted)
data_exposure<-melt(data_exposure,  id = c('time'))

ggplot(data_exposure,aes(time,value))+
  geom_line(aes(colour = variable))+
  scale_y_continuous(name="Exposure", labels = scales::comma)+
  scale_color_hue(name = "Exposure metrics",labels = c("EDE","PFE"))+
  xlab("Time in years")+
  ggtitle("Exposure 2007 ")+
  theme(plot.title = element_text(hjust = 0.5))

data_negative_exposure<-data.frame("time"=time_vector, "EDNE" =EDNE_netted, "PFNE"=PFNE_netted)
data_negative_exposure<-melt(data_negative_exposure,  id = c('time'))

ggplot(data_negative_exposure,aes(time,value))+
  geom_line(aes(colour = variable))+
  scale_y_continuous(name="Exposure", labels = scales::comma)+
  scale_color_hue(name = "Exposure metrics",labels = c("EDNE", "PFNE"))+
  xlab("Time in years")+
  ggtitle("Negative Exposure 2007")+
  theme(plot.title = element_text(hjust = 0.5))


#### CVA calculations ###

UCVA_v<-rep(0,dim(Value1)[1])
UDVA_v<-rep(0,dim(Value1)[1])
CVA_bi_v<-rep(0,dim(Value1)[1])
DVA_bi_v<-rep(0,dim(Value1)[1])
BCVA_v<-rep(0,dim(Value1)[1])

for(i in 1:(dim(Value1)[1]))
{
  if(i<(dim(Value1)[1])-2)
  {
    UCVA_v[i]<-UCVA(i*dt,end1,dt,LGD,EDE_netted,PD_Europe)
    UDVA_v[i]<-UDVA(i*dt,end1,dt,LGD,EDNE_netted,PD_Cross)
    CVA_bi_v[i]<-CVA_bi(i*dt,end1,dt,LGD,EDE_netted,PD_Europe,S_Cross)
    DVA_bi_v[i]<-DVA_bi(i*dt,end1,dt,LGD,EDNE_netted,PD_Cross,S_Europe)
    BCVA_v[i]<-BCVA(i*dt,end1,dt,LGD,EDE_netted,EDNE_netted,PD_Cross,PD_Europe,S_Europe,S_Cross)
  }
  else
  {
    UCVA_v[i]<-0
    UDVA_v[i]<-0
    CVA_bi_v[i]<-0
    DVA_bi_v[i]<-0
    BCVA_v[i]<-0
  }
}
# Values at time zero
UCVA_v[1]
UDVA_v[1]
CVA_bi_v[1]
DVA_bi_v[1]

####Plots of CVA ####

#### CVA comparison ###
time<-seq(0,end1,dt)


data_CVA<-data.frame(time,UCVA_v, CVA_bi_v)
data_CVA<-melt(data_CVA,  id = c('time'))

ggplot(data_CVA,aes(time,value))+
  geom_line(aes(colour = variable))+
  scale_y_continuous(name="Adjustment in kr", labels = scales::comma)+
  scale_color_hue(name = "Model",labels = c("UCVA","BCVA"))+
  xlab("Time in years")+
  ggtitle("CVA 2007")+
  theme(plot.title = element_text(hjust = 0.5))


##### DVA comparision ####

data_DVA<-data.frame(time,UDVA_v,DVA_bi_v)
data_DVA<-melt(data_DVA,  id = c('time'))

ggplot(data_DVA,aes(time,value))+
  geom_line(aes(colour = variable))+
  scale_y_continuous(name="Adjustment in kr", labels = scales::comma)+
  scale_color_hue(name = "Model",labels = c("UDVA","BDVA"))+
  xlab("Time in years")+
  ggtitle("DVA 2007")+
  theme(plot.title = element_text(hjust = 0.5))

#### Total adjustments ####

Uni_total<-UCVA_v+UDVA_v
data_total<-data.frame(time,Uni_total,BCVA_v)
data_total<-melt(data_total,  id = c('time'))

ggplot(data_total,aes(time,value))+
  geom_line(aes(colour = variable))+
  scale_y_continuous(name="Adjustment in kr", labels = scales::comma)+
  scale_color_hue(name = "Model",labels = c("Unilateral","Bilateral"))+
  xlab("Time in years")+
  ggtitle("Total Adjustment 2007")+
  theme(plot.title = element_text(hjust = 0.5))

