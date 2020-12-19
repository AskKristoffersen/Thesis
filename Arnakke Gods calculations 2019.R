
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

zero_rates<-read.csv2('Interest rate data 2019.csv', header = T)
CDS_Crosss<-read.csv2("ITRAXX Crossover 2019.csv", header=T)
CDS_Europe<-read.csv2("ITRAXX Europe 2019.csv", header=T)

#### Parameters ####

eps<-0.00001 ###small value for computing derivatives
a<-0.1  ##Hul-White parameter
sigma<-0.01  #volatility of interest rate
r0<- zero_rates[1,2]/100  #Rate at start
R<-0.4  #recovery rate
LGD<-1-R  # Loss Given Default


###Simulations ####

n<-100 #Number of simulations
Tn<-19 #end of simulations in year
steps<-19*12  #Number of steps
dt<-Tn/steps  # size of steps

### Swap contract ###

start<-0 #Start of swap contract
end<-19  #end of swap contract
tenor<-3/12 #size between payments
fixed<-0.043 #fixed rate
N<-66000000 #Notional


###Get calibrated curves ###

ZCB<-splinefun(zero_rates[[1]],zero_rates[[3]]) #Makes Discount curve

PD_Cross<-function(x){Default_prob(CDS_Crosss,LGD, end, x)} #Default probability for Crossover
S_Cross<-function(x){survival_prob(CDS_Crosss,LGD, end,x)} #Survival probability for Crossover

PD_Europe<-function(x){Default_prob(CDS_Europe,LGD, end, x)} #Default probability for Europe
S_Europe<-function(x){survival_prob(CDS_Europe,LGD, end, x)} #Survival probability for Europe

par_swap_rate<-swap_rate_f(start,end, tenor, r0, ZCB, eps, a, sigma) #gets par swap rate
par_swap_rate

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
  ggtitle("Hull-White interest rate paths 2019 ")+xlab("Time in years")+ylab("Interest rate ")+
  theme(plot.title = element_text(hjust = 0.5))
data_quantiles<-data.frame("time"=time_vector, "quantiles" =lines)
plot_rate+geom_line(data=data_quantiles, aes(x=time, y=quantiles.mean_r))+
  geom_line(data=data_quantiles, aes(x=time, y=quantiles.2.5.))+
  geom_line(data=data_quantiles, aes(x=time, y=quantiles.97.5.))+
  geom_hline(aes(yintercept = 0.043), color="red")+
  scale_linetype_manual(name="Fixed rate", values =2, guide=guide_legend(overide.aes=list(color="red")))+
  theme(legend.position = "right")




### calculate IRS Values ###

Value<-PV_IRS_Payer(start,end, tenor, steps,n, dt, N, a, sigma, fixed,  r, ZCB,  eps)

base_value<-Value[1,1] #gets base value at time zero

###Plot of IRS values ####
mean_IRS<-rep(NA,steps+1)
mean_IRS<-Value[1,1]
for(i in 1:steps+1)
{
  mean_IRS[i]<-mean(Value[i,])
}

quantiles_IRS<-apply(Value,1, quantile,probs= c(0.025,0.975))
lines_IRS<-t(rbind(mean_IRS,quantiles_IRS))

time_vector<-seq(0,end,by=dt)
data_plot<- data.frame("time"=time_vector, "paths" = Value[,1:100])
data_plot<- melt(data_plot,  id = c('time'))

plot_IRS_Values <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+
  ggtitle("Interest rate swap value 2019")+xlab("Time in years")+
  scale_y_continuous(name="IRS value", labels = scales::comma)+
  theme(plot.title = element_text(hjust = 0.5))

data_quantiles_IRS<-data.frame("time"=time_vector, "quantiles" =lines_IRS)

plot_IRS_Values+geom_line(data=data_quantiles_IRS, aes(x=time, y=quantiles.mean_IRS,linetype="dashed"))+
  geom_line(data=data_quantiles_IRS, aes(x=time, y=quantiles.2.5.))+
  geom_line(data=data_quantiles_IRS, aes(x=time, y=quantiles.97.5.))

data_quantiles_IRS<- melt(data_quantiles_IRS,  id = c('time'))

ggplot(data_quantiles_IRS,aes(time,value))+
  geom_line(aes(colour = variable))+
  scale_y_continuous(name="IRS value", labels = scales::comma)+
  scale_color_hue(name = "Quantiles",labels = c("Average","2.5%", "97.5%"))+
  xlab("Time in years")+
  ggtitle("IRS Value quantiles")+
  theme(plot.title = element_text(hjust = 0.5))

#### Calculates exposure ####

EDE_v<-EDE(Value,ZCB,dt,a,sigma,r,eps)
EDNE_v<-EDNE(Value,ZCB,dt,a,sigma,r,eps)
PFE_v<-PFE(Value)
PFNE_v<-PFNE(Value)
#### plot of exposure ####
time_vector<-seq(0,end,by=dt)

data_exposure<-data.frame("time"=time_vector, "EDE" =EDE_v, "MC"=PFE_v)
data_exposure<-melt(data_exposure,  id = c('time'))

ggplot(data_exposure,aes(time,value))+
  geom_line(aes(colour = variable))+
  scale_y_continuous(name="Exposure", labels = scales::comma)+
  scale_color_hue(name = "Exposure metrics",labels = c("EDE","MC"))+
  xlab("Time in years")+
  ggtitle("Exposure 2019")+
  theme(plot.title = element_text(hjust = 0.5))

data_negative_exposure<-data.frame("time"=time_vector, "EDNE" =EDNE_v, "PFNE"=PFNE_v)
data_negative_exposure<-melt(data_negative_exposure,  id = c('time'))

ggplot(data_negative_exposure,aes(time,value))+
  geom_line(aes(colour = variable))+
  scale_y_continuous(name="Exposure", labels = scales::comma)+
  scale_color_hue(name = "Exposure metrics",labels = c("EDNE", "PFNE"))+
  xlab("Time in years")+
  ggtitle("Negative Exposure 2019")+
  theme(plot.title = element_text(hjust = 0.5))


#### CVA calculations ###

UCVA_v<-rep(0,dim(Value)[1])
UDVA_v<-rep(0,dim(Value)[1])
CVA_bi_v<-rep(0,dim(Value)[1])
DVA_bi_v<-rep(0,dim(Value)[1])
BCVA_v<-rep(0,dim(Value)[1])

for(i in 1:(dim(Value)[1]))
{
  if(i<(dim(Value)[1])-2)
  {
    UCVA_v[i]<-UCVA(i*dt,end,dt,LGD,EDE_v,PD_Europe)
    UDVA_v[i]<-UDVA(i*dt,end,dt,LGD,EDNE_v,PD_Cross)
    CVA_bi_v[i]<-CVA_bi(i*dt,end,dt,LGD,EDE_v,PD_Europe,S_Cross)
    DVA_bi_v[i]<-DVA_bi(i*dt,end,dt,LGD,EDNE_v,PD_Cross,S_Europe)
    BCVA_v[i]<-BCVA(i*dt,end,dt,LGD,EDE_v,EDNE_v,PD_Cross,PD_Europe,S_Europe,S_Cross)
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
time<-seq(0,end,dt)

data_CVA<-data.frame(time,UCVA_v, CVA_bi_v)
data_CVA<-melt(data_CVA,  id = c('time'))

ggplot(data_CVA,aes(time,value))+
  geom_line(aes(colour = variable))+
  scale_y_continuous(name="Adjustment in kr", labels = scales::comma)+
  scale_color_hue(name = "Model",labels = c("UCVA","BCVA"))+
  xlab("Time in years")+
  ggtitle("CVA 2019")+
  theme(plot.title = element_text(hjust = 0.5))

##### DVA Plot ####

data_DVA<-data.frame(time,UDVA_v,DVA_bi_v)
data_DVA<-melt(data_DVA,  id = c('time'))

ggplot(data_DVA,aes(time,value))+
  geom_line(aes(colour = variable))+
  scale_y_continuous(name="Adjustment in kr", labels = scales::comma)+
  scale_color_hue(name = "Model",labels = c("UDVA","BDVA"))+
  xlab("Time in years")+
  ggtitle("DVA 2019")+
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
  ggtitle("Total Adjustment 2019")+
  theme(plot.title = element_text(hjust = 0.5))


