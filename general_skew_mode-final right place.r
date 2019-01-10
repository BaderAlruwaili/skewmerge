############################################################

#
library(sn)

myfunction<- function(x,alpha,xi=0,omega=1){

    z=(x-xi)/omega
  
# I am making z as the axis
#skewness  

  #pnorm,and dnorm of normal distribution
  PHI<- pnorm(alpha*z)
  phi<- dnorm(alpha*z)

  # To find the mode of skew normal distribution by using the first derivation of skew normal distribution
  # these parameters of the first derivative of skew normal distribution
  #  for more information of a , and p please see the pdf file 

 a<- (-z)*PHI
  b<-alpha*phi

   f<- a+b
  
  return(f)
}




 x=seq(-5,5,by=.1)

plot(z,myfunction(x,4,0,0),typ="l")
abline(h=0,col=2)

alpha=2
xi=0
omega=1


upper=alpha+1

mymode=uniroot(myfunction,lower=xi-5,upper=xi+ 5,alpha=alpha,xi=xi,omega=omega)$root
 # To check if the mode is in the right place   check with other values 
                                        #..of alphavictor
cat(mymode)

curve(dsn(x,xi=xi,omega=omega,alpha=alpha),-6,6)

# to change the name of x, and y
curve(dsn(x,xi=xi,omega=omega,alpha=alpha),-6,6,xlab='x',ylab='Density')

#curve(dsn(x,xi=2,alpha=alpha),-3,3)
abline(v=mymode,col=2 )







##Mode and Mean 
######### UNIVARIATRE
fun=function(z,lambda=2){
  return(0-z*pnorm(lambda*z)+lambda*dnorm(lambda*z))
}

alpha=2

pdf1 <- dmsn(as.matrix(x,ncol=1), 0, 1, alpha=alpha)
plot(x,pdf1,type="l")
pdf1.mean=sqrt(2/pi) * alpha/sqrt(1+alpha^2)
abline(v=pdf1.mean,col=2)  fun=function(z,lambda=2){
  return(0-z*pnorm(lambda*z)+lambda*dnorm(lambda*z))
}

alpha=4

pdf1 <- dmsn(as.matrix(x,ncol=1), 0, 1, alpha=alpha)
plot(x,pdf1,type="l",xlab='x',ylab='Density')
pdf1.mean=sqrt(2/pi) * alpha/sqrt(1+alpha^2)
pdf1.mean
abline(v=pdf1.mean,col=3)


pdf1.mode=uniroot(fun,lower=0,upper=pdf1.mean,lambda=alpha)$root
pdf1.mode
abline(v=pdf1.mode,col=2)

legend("topright",c("Mode","Mean" ),col=c(2,3),lwd=c(2,1),cex=.9)












par(mfrow=c(1,2))


############  this is the final code for mean and mode of skew normal to gother ################################################

#
library(sn)

myfunction<- function(x,alpha,xi=0,omega=1){
  
  z=(x-xi)/omega
  
  # I am making z as the axis
  #skewness  
  
  #pnorm,and dnorm of normal distribution
  PHI<- pnorm(alpha*z)
  phi<- dnorm(alpha*z)
  
  # To find the mode of skew normal distribution by using the first derivation of skew normal distribution
  # these parameters of the first derivative of skew normal distribution
  #  for more information of a , and p please see the pdf file 
  
  a<- (-z)*PHI
  b<-alpha*phi
  
  f<- a+b
  
  return(f)
}




x=seq(-5,5,by=.1)

plot(z,myfunction(x,4,0,0),typ="l")
abline(h=0,col=2)

alpha=4
xi=0
omega=1


upper=alpha+1

mymode=uniroot(myfunction,lower=xi-5,upper=xi+ 5,alpha=alpha,xi=xi,omega=omega)$root
# To check if the mode is in the right place   check with other values 
#..of alphavictor
cat(mymode)

#curve(dsn(x,xi=xi,omega=omega,alpha=alpha),-5,5)

# to change the name of x, and y
curve(dsn(x,xi=xi,omega=omega,alpha=alpha),-5,5,xlab='x',ylab='Density')

#curve(dsn(x,xi=2,alpha=alpha),-3,3)
abline(v=mymode,col=2 )



pdf1 <- dmsn(as.matrix(x,ncol=1), 0, 1, alpha=alpha)
# plot(x,pdf1,type="l",xlab='x',ylab='Density')


pdf1.mean=sqrt(2/pi) * alpha/sqrt(1+alpha^2)
pdf1.mean
abline(v=pdf1.mean,col=3)

legend("topright",c("Mode","Mean" ),col=c(2,3),lwd=c(2,1),cex=.9)














# for -
library(sn)

myfunction<- function(x,alpha,xi=0,omega=1){
  
  z=(x-xi)/omega
  
  # I am making z as the axis
  #skewness  
  
  #pnorm,and dnorm of normal distribution
  PHI<- pnorm(alpha*z)
  phi<- dnorm(alpha*z)
  
  # To find the mode of skew normal distribution by using the first derivation of skew normal distribution
  # these parameters of the first derivative of skew normal distribution
  #  for more information of a , and p please see the pdf file 
  
  a<- (-z)*PHI
  b<-alpha*phi
  
  f<- a+b
  
  return(f)
}




x=seq(-5,5,by=.1)

plot(z,myfunction(x,4,0,0),typ="l")
#abline(h=0,col=2)

alpha=-4
xi=0
omega=1


upper=alpha+1

mymode=uniroot(myfunction,lower=xi-5,upper=xi+ 5,alpha=alpha,xi=xi,omega=omega)$root
# To check if the mode is in the right place   check with other values 
#..of alphavictor
cat(mymode)

#curve(dsn(x,xi=xi,omega=omega,alpha=alpha),-5,5)

# to change the name of x, and y
curve(dsn(x,xi=xi,omega=omega,alpha=alpha),-5,5,xlab='x',ylab='Density')

#curve(dsn(x,xi=2,alpha=alpha),-3,3)
abline(v=mymode,col=2 )



pdf1 <- dmsn(as.matrix(x,ncol=1), 0, 1, alpha=alpha)
# plot(x,pdf1,type="l",xlab='x',ylab='Density')


pdf1.mean=sqrt(2/pi) * alpha/sqrt(1+alpha^2)
pdf1.mean
abline(v=pdf1.mean,col=3)

legend("topright",c("Mode","Mean" ),col=c(2,3),lwd=c(2,1),cex=.9)
















# for 0
library(sn)

myfunction<- function(x,alpha,xi=0,omega=1){
  
  z=(x-xi)/omega
  
  # I am making z as the axis
  #skewness  
  
  #pnorm,and dnorm of normal distribution
  PHI<- pnorm(alpha*z)
  phi<- dnorm(alpha*z)
  
  # To find the mode of skew normal distribution by using the first derivation of skew normal distribution
  # these parameters of the first derivative of skew normal distribution
  #  for more information of a , and p please see the pdf file 
  
  a<- (-z)*PHI
  b<-alpha*phi
  
  f<- a+b
  
  return(f)
}




x=seq(-5,5,by=.1)

plot(z,myfunction(x,4,0,0),typ="l")
abline(h=0,col=2)

alpha=0
xi=0
omega=1


upper=alpha+1

mymode=uniroot(myfunction,lower=xi-5,upper=xi+ 5,alpha=alpha,xi=xi,omega=omega)$root
# To check if the mode is in the right place   check with other values 
#..of alphavictor
cat(mymode)

#curve(dsn(x,xi=xi,omega=omega,alpha=alpha),-5,5)

# to change the name of x, and y
curve(dsn(x,xi=xi,omega=omega,alpha=alpha),-5,5,xlab='x',ylab='Density')

#curve(dsn(x,xi=2,alpha=alpha),-3,3)
abline(v=mymode,col=2 )


legend("topright",c("Mode" ),col=c(2),lwd=c(2,1),cex=.9)





pdf1 <- dmsn(as.matrix(x,ncol=1), 0, 1, alpha=alpha)
 plot(x,pdf1,type="l",xlab='x',ylab='Density')


pdf1.mean=sqrt(2/pi) * alpha/sqrt(1+alpha^2)
pdf1.mean
abline(v=pdf1.mean,col=3)

legend("topright",c("Mean" ),col=c(3),lwd=c(2,1),cex=.9)










