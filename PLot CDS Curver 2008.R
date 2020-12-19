###Imports functions ###

source("Probability functions.R")

#####imports Data #####
Traxx_08<-read.csv2("ITRAXX europe 2008.csv", header=T)
Traxx_crossover_08<-read.csv2("ITRAXX Crossover 2008.csv", header=T)
Traxx<-read.csv2("ITRAXX Europe 2019.csv", header=T)
Traxx_crossover<-read.csv2("ITRAXX Crossover 2019.csv", header=T)

#gets functions for default probabilities
prob<-function(x){Default_prob(Traxx,0.6,30,x)}
prob_cross<-function(x){Default_prob(Traxx_crossover,0.6,30,x)}
prob_08<-function(x){Default_prob(Traxx_08,0.6,30,x)}
prob_cross_08<-function(x){Default_prob(Traxx_crossover_08,0.6,30,x)}

###makes data points for plots

maturity_CDS<-Traxx[[1]]
spread<-Traxx[[2]]/10000
maturity_CDS_cross<-Traxx_crossover[[1]]
spread_cross<-Traxx_crossover[[2]]/10000
maturity_CDS_08<-Traxx_08[[1]]
spread_08<-Traxx_08[[2]]/10000
maturity_CDS_cross_08<-Traxx_crossover_08[[1]]
spread_cross_08<-Traxx_crossover_08[[2]]/10000



lambda<-spread/0.6
lambda_cross<-spread_cross/0.6
lambda_08<-spread_08/0.6
lambda_cross_08<-spread_cross_08/0.6

Prob<-1-exp(-lambda*maturity_CDS)
Prob_cross<-1-exp(-lambda_cross*maturity_CDS_cross)
Prob_08<-1-exp(-lambda_08*maturity_CDS_08)
Prob_cross_08<-1-exp(-lambda_cross_08*maturity_CDS_cross_08)


##### Plot of default probabilities######


plot(prob_cross,
     type="l",
     xlim=c(0,30),
     ylim=c(0,1),
     xlab="Maturity",
     ylab = "Default probability",
     main="Default probabilities")
plot(prob_cross_08,
     add=T,
     col="blue",type="l",
     xlim = c(0,30))
plot(prob,
     add=T,
     col="red",type="l",
     xlim = c(0,30))
plot(prob_08,
     add=T,
     col="green",type="l",
     xlim = c(0,30))
legend("topleft",
       c("Traxx Crossover","Traxx Crossover 2008","Traxx Europe","Traxx Europe 2008"),
       fill=c("black","blue","red", "green")
)
grid()
points(maturity_CDS,Prob)
points(maturity_CDS_cross,Prob_cross)
points(maturity_CDS_08,Prob_08)
points(maturity_CDS_cross_08,Prob_cross_08)

### Survival plots ###


survival<-function(x){survival_prob(Traxx,0.6,30,x)}
survival_Cross<-function(x){survival_prob(Traxx_crossover,0.6,30,x) }
survival_08<-function(x){survival_prob(Traxx_08,0.6,30,x)}
survival_Cross_08<-function(x){survival_prob(Traxx_crossover_08,0.6,30,x) }

survival_data<-exp(-lambda*maturity_CDS)
survival_data_cross<-exp(-lambda_cross*maturity_CDS_cross)
survival_data_08<-exp(-lambda_08*maturity_CDS_08)
survival_data_cross_08<-exp(-lambda_cross_08*maturity_CDS_cross_08)

plot(survival_Cross,
     type="l",
     xlim=c(0,30),
     ylim=c(0,1),
     xlab="Maturity",
     ylab = "Survival probability",
     main="Survival probabilities")
plot(survival_Cross_08,
     add=T,
     col="blue",type="l",
     xlim = c(0,30))
plot(survival,
     add=T,
     col="red",type="l",
     xlim = c(0,30))
plot(survival_08,
     add=T,
     col="green",type="l",
     xlim = c(0,30))
legend("topright",
       c("Traxx Crossover","Traxx Crossover 2008","Traxx Europe","Traxx Europe 2008"),
       fill=c("black","blue","red", "green")
)
grid()
points(maturity_CDS,survival_data)
points(maturity_CDS_cross,survival_data_cross)
points(maturity_CDS_08,survival_data_08)
points(maturity_CDS_cross_08,survival_data_cross_08)

###plot of default porbability between timesteps

PD<-rep(NA,12*30-1)
PD_cross<-rep(NA,12*30-1)
PD_08<-rep(NA,12*30-1)
PD_cross_08<-rep(NA,12*30-1)

for(i in 1:360)
{
  PD[i]<-prob((i+1)*1/12)-prob(i*1/12)
  PD_cross[i]<-prob_Cross((i+1)*1/12)-prob_Cross(i*1/12)
  PD_08[i]<-prob_08((i+1)*1/12)-prob_08(i*1/12)
  PD_cross_08[i]<-prob_cross_08((i+1)*1/12)-prob_cross_08(i*1/12)
}

time<-seq(1/12,30,1/12)
plot(time,PD_cross,type="l",
     ylim=c(0,0.010),
     ylab="Deafult Probability",
     xlab="Time in years",
     main = "Default probabilities between each time step")
lines(time,PD_cross_08, col="blue")
lines(time,PD, col="red")
lines(time,PD_08,col="green")
grid()
legend("topright",
       c("Traxx Crossover","Traxx Crossover 2008","Traxx Europe","Traxx Europe 2008"),
       fill=c("black","blue","red", "green")
)
