###imports libraries for plotting

library(ggplot2)
library(reshape2)

##Loan parameters

N<-500000 ##Loan notional 
install<-12500 #installments
steps<-40 #number of payments
r<-0.02  #interest rate

#Makes payments

payments<-rep(NA,steps)

for(i in 1:40)
{
  payments[i]<-(install)+(N-(i-1)*install)*r
}
####Calculates exposure

exposure<-rep(NA,steps+1)

for(i in 1:41)
{
  sum<-0
  for(j in i:40)
  {
    sum<-sum+payments[j]/(1+r)^j
  }
  exposure[i]<-sum
}
exposure[41]<-0

##Plots exposure

time<-seq(0,10,3/12)
plot(time,exposure, type="l")

df<-data.frame(time,exposure)
ggplot(df, aes(time,exposure))+geom_line()+
  scale_y_continuous(name="Exposure", labels = scales::comma)+
  scale_color_hue(name = "Exposure",labels = c("EDE"))+
  xlab("Time in years")+
  ggtitle("Discounted Exposure")+
  theme(plot.title = element_text(hjust = 0.5))

###Calculates CVA ##

LGD<-0.6 #Loss given default
PD<-0.002 #probability of default

CVA<-rep(NA,steps+1)
for(i in 1:41)
{
  sum<-0
  for(j in 1:40)
  {
    sum<-sum+LGD*exposure[i+1]*PD
  }
  CVA[i]<-sum
}

CVA[41]<-0

###Plots CVA ###

df<-data.frame(time,CVA)
ggplot(df, aes(time,CVA))+geom_line()+
  scale_y_continuous(name="CVA", labels = scales::comma)+
  scale_color_hue(name = "CVA",labels = c("CVA"))+
  xlab("Time in years")+
  ggtitle("Plot of CVA")+
  theme(plot.title = element_text(hjust = 0.5))


df<-data.frame(time,exposure,CVA)
df<-melt(df,  id = c('time'))

ggplot(df, aes(time,value))+geom_line(aes(colour = variable))+
  scale_y_continuous(name="Exposure", labels = scales::comma)+
  scale_color_hue(name = "Exposure",labels = c("EDE","CVA"))+
  xlab("Time in years")+
  ggtitle("Discounted Exposure")+
  theme(plot.title = element_text(hjust = 0.5))
