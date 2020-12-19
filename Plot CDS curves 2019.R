
###Imports functions ###
source("Probability functions.R")

       ##### imports Data #####
Traxx<-read.csv2("ITRAXX Europe 2019.csv", header=T)
Traxx_crossover<-read.csv2("ITRAXX Crossover 2019.csv", header=T)
 
####gets probability functions 
prob<-function(x){Default_prob(Traxx,0.6,30,x)}
prob_cross<-function(x){Default_prob(Traxx_crossover,0.6,30,x)}

###makes data points for plots

maturity_CDS<-Traxx[[1]]
spread<-Traxx[[2]]/10000
maturity_CDS_cross<-Traxx_crossover[[1]]
spread_cross<-Traxx_crossover[[2]]/10000

lambda<-spread/0.6
lambda_cross<-spread_cross/0.6

Prob<-1-exp(-lambda*maturity_CDS)
Prob_cross<-1-exp(-lambda_cross*maturity_CDS_cross)

                             ##### Plot of default probabilities######


plot(prob_cross,
      type="l",
      xlim=c(0,30),
      xlab="Maturity",
      ylab = "Default probability",
      main="Default probabilities")
plot(prob,
     add=T,
     col="red",type="l",
     xlim = c(0,30))
legend("topleft",
        c("Traxx Crossover","Traxx Europe"),
        fill=c("black","red")
 )
grid()
points(maturity_CDS,Prob)
points(maturity_CDS_cross,Prob_cross)
 
### Survival plots ###

 
survival<-function(x){survival_prob(Traxx,0.6,30,x)}
survival_Cross<-function(x){survival_prob(Traxx_crossover,0.6,30,x) }

survival_data<-exp(-lambda*maturity_CDS)
survival_data_cross<-exp(-lambda_cross*maturity_CDS_cross)

plot(survival_Cross,
     type="l",
     xlim=c(0,30),
     xlab="Maturity",
     ylab = "Survival probability",
     main="Survival probabilities")
plot(survival,
     add=T,
     col="red",type="l",
     xlim = c(0,30))
legend("topright",
       c("Traxx Crossover","Traxx Europe"),
       fill=c("black","red")
)
grid()
points(maturity_CDS,survival_data)
points(maturity_CDS_cross,survival_data_cross)

###plot of default porbability between timesteps

PD<-rep(NA,12*30-1)
PD_cross<-rep(NA,12*30-1)

for(i in 1:360)
{
  PD[i]<-prob((i+1)*1/12)-prob(i*1/12)
  PD_cross[i]<-prob_Cross((i+1)*1/12)-prob_Cross(i*1/12)
}

time<-seq(1/12,30,1/12)
plot(time,PD_cross,type="l",
     ylim=c(0,0.005),
     ylab="Deafult Probability",
     xlab="Time in years",
     main = "Default probabilities between each time step")
lines(time,PD, col="red")
legend("topright",  c("Traxx Crossover","Traxx Europe"),
       fill=c("black","red"))
