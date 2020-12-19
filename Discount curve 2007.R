#Imports zero rates and ZCB prices

Rente_data<-read.csv2('Interest rate data 2007.csv', header = T)

zero_rates<-Rente_data[[2]]
maturity<-Rente_data[[1]]
ZCB<-Rente_data[[3]]

#Makes cubic splines

cubic_rates<-splinefun(maturity,zero_rates)
cubic_ZCB<-splinefun(maturity,ZCB)

#####Instantaouses Forward curve######
eps<-0.0001

cubic_forward<-function(x)
{
  return( -(log(cubic_ZCB(x+eps))-log(cubic_ZCB(x)))/eps *100)
}

###Forward derivative curve

forward_deriv<-function(x)
{
  (cubic_forward(x+eps)-cubic_forward(x))/eps
}

####Plots calibrated curves


plot(forward_deriv,
     xlim=c(0,30),
     ylim=c(-2,6),
     col='blue',
     xlab="Time in Years",
     ylab="Interest rate [%]",
     main="Calibrated curves 2007")
plot(cubic_forward,
     add=T,
     xlim=c(0,30),
     ylim=c(-1,1.5),
     col='red')
plot(cubic_rates,
     add=T,
     xlim=c(0,30),
     ylim=c(-1,2.5),
     col='green')
# plot(cubic_ZCB,
#      add=T,
#      xlim=c(0,30),
#      ylim=c(-1,2.5),
#      col='black')
legend("topright",
       c("Zero rates","Forward rates","Forward derivative"),
       fill=c("green","red","blue")
)
grid()
