###Imports functions ###

source("Probability functions.R")

#####imports Data #####
Traxx_07<-read.csv2("ITRAXX europe 2007.csv", header=T)
Traxx_crossover_07<-read.csv2("ITRAXX Crossover 2007.csv", header=T)
Traxx<-read.csv2("ITRAXX Europe 2019.csv", header=T)
Traxx_crossover<-read.csv2("ITRAXX Crossover 2019.csv", header=T)

#gets functions for default probabilities
prob<-function(x){Default_prob(Traxx,0.6,30,x)}
prob_cross<-function(x){Default_prob(Traxx_crossover,0.6,30,x)}
prob_07<-function(x){Default_prob(Traxx_07,0.6,30,x)}
prob_cross_07<-function(x){Default_prob(Traxx_crossover_07,0.6,30,x)}

###makes data points for plots 

maturity_CDS<-Traxx[[1]]
spread<-Traxx[[2]]/10000
maturity_CDS_cross<-Traxx_crossover[[1]]
spread_cross<-Traxx_crossover[[2]]/10000
maturity_CDS_07<-Traxx_07[[1]]
spread_07<-Traxx_07[[2]]/10000
maturity_CDS_cross_07<-Traxx_crossover_07[[1]]
spread_cross_07<-Traxx_crossover_07[[2]]/10000



lambda<-spread/0.6
lambda_cross<-spread_cross/0.6
lambda_07<-spread_07/0.6
lambda_cross_07<-spread_cross_07/0.6

Prob<-1-exp(-lambda*maturity_CDS)
Prob_cross<-1-exp(-lambda_cross*maturity_CDS_cross)
Prob_07<-1-exp(-lambda_07*maturity_CDS_07)
Prob_cross_07<-1-exp(-lambda_cross_07*maturity_CDS_cross_07)


##### Plot of default probabilities######


plot(prob_cross,
     type="l",
     xlim=c(0,31),
     ylim=c(0,1),
     xlab="Maturity",
     ylab = "Default probability",
     main="Default probabilities")
plot(prob_cross_07,
     add=T,
     col="blue",type="l",
     xlim = c(0,31))
plot(prob,
     add=T,
     col="red",type="l",
     xlim = c(0,31))
plot(prob_07,
     add=T,
     col="green",type="l",
     xlim = c(0,31))
legend("topleft",
       c("Traxx Crossover","Traxx Crossover 2007","Traxx Europe","Traxx Europe 2007"),
       fill=c("black","blue","red", "green")
)
grid()
points(maturity_CDS,Prob)
points(maturity_CDS_cross,Prob_cross)
points(maturity_CDS_07,Prob_07)
points(maturity_CDS_cross_07,Prob_cross_07)

### Survival plots ###


survival<-function(x){survival_prob(Traxx,0.6,30,x)}
survival_Cross<-function(x){survival_prob(Traxx_crossover,0.6,30,x) }
survival_07<-function(x){survival_prob(Traxx_07,0.6,30,x)}
survival_Cross_07<-function(x){survival_prob(Traxx_crossover_07,0.6,30,x) }

survival_data<-exp(-lambda*maturity_CDS)
survival_data_cross<-exp(-lambda_cross*maturity_CDS_cross)
survival_data_07<-exp(-lambda_07*maturity_CDS_07)
survival_data_cross_07<-exp(-lambda_cross_07*maturity_CDS_cross_07)

plot(survival_Cross,
     type="l",
     xlim=c(0,31),
     ylim=c(0,1),
     xlab="Maturity",
     ylab = "Survival probability",
     main="Survival probabilities")
plot(survival_Cross_07,
     add=T,
     col="blue",type="l",
     xlim = c(0,31))
plot(survival,
     add=T,
     col="red",type="l",
     xlim = c(0,31))
plot(survival_07,
     add=T,
     col="green",type="l",
     xlim = c(0,31))
legend("topright",
       c("Traxx Crossover","Traxx Crossover 2007","Traxx Europe","Traxx Europe 2007"),
       fill=c("black","blue","red", "green")
)
grid()
points(maturity_CDS,survival_data)
points(maturity_CDS_cross,survival_data_cross)
points(maturity_CDS_07,survival_data_07)
points(maturity_CDS_cross_07,survival_data_cross_07)

###plot of default porbability between timesteps

PD<-rep(NA,12*31-1)
PD_cross<-rep(NA,12*31-1)
PD_07<-rep(NA,12*31-1)
PD_cross_07<-rep(NA,12*31-1)

for(i in 1:31*12)
{
  PD[i]<-prob((i+1)*1/12)-prob(i*1/12)
  PD_cross[i]<-prob_cross((i+1)*1/12)-prob_cross(i*1/12)
  PD_07[i]<-prob_07((i+1)*1/12)-prob_07(i*1/12)
  PD_cross_07[i]<-prob_cross_07((i+1)*1/12)-prob_cross_07(i*1/12)
}

time<-seq(1/12,31,1/12)
plot(time,PD_cross,type="l",
     ylim=c(0,0.010),
     ylab="Deafult Probability",
     xlab="Time in years",
     main = "Default probabilities between each time step")
lines(time,PD_cross_07, col="blue")
lines(time,PD, col="red")
lines(time,PD_07,col="green")
grid()
legend("topright",
       c("Traxx Crossover","Traxx Crossover 2007","Traxx Europe","Traxx Europe 2007"),
       fill=c("black","blue","red", "green")
)
