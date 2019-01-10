

library(mixsmsn)
library(mclust)
nstall.packages("sn")
library(sn)
library(mixsmsn)
install.packages("rootSolve")
library(rootSolve)



par(mfrow=c(1,2))
#############################################################################################
# Solve for mode of biavariate skewed normal



fun2=function(z,lambda=c(0,0)){
  F1= 0-z[1]*pnorm(as.numeric(lambda%*%z))+lambda[1]*dnorm(as.numeric(lambda%*%z))
  F2= 0-z[2]*pnorm(as.numeric(lambda%*%z))+lambda[2]*dnorm(as.numeric(lambda%*%z))
  
  return(c(F1=F1,F2=F2))
}


alpha=c(6,3)


ss <- multiroot(f=fun2, start = c(.37,.37),lambda=alpha)
# .37 is the maximum mode this works for small alphas less than 2

x <- seq(-3,3, length.out = 500)
y <- seq(-3, 3, length.out = 500)
d1 <- expand.grid(x = x, y = y)



pdf1 <- dmsn(d1, c(0,0), diag(c(1,1)), alpha=alpha)
contour(x,y,matrix(pdf1,500,500))


# Alternative using contours


ss <- multiroot(f=fun2, start=as.numeric(d1[which(pdf1==max(pdf1))[1],]),lambda=alpha)
points(ss$root[1],ss$root[2],,pch="*",cex=4,col=2)


#####################################################################################################

x<-seq(-3,3,by=0.1)
y<-seq(-3,3,by=0.1)

alpha<-c(6,3)
omega<-matrix(c(1,0,0,1),ncol=2)
omega

z1<-outer(x,y)
z2<-outer(x,y)
for(i in 1:length(x))
  for(j in 1:length(y)){
    z1[i,j]<-dmsn(cbind(x[i],x[j]), xi=c(0,0), omega, alpha, log=FALSE)
  }
for(i in 1:length(x))
  for(j in 1:length(y)){
    z2[i,j]<-dmsn(cbind(x[i],x[j]), xi=c(0,0), omega, alpha=c(0,0), log=FALSE)
  }

contour(x,y,z2)
contour(x,y,z1,cex=0.05,lwd=4,add=TRUE)
abline(h=0,v=0)
ss <- multiroot(f=fun2, start=as.numeric(d1[which(pdf1==max(pdf1))[1],]),lambda=alpha)
points(ss$root[1],ss$root[2],,pch="*",cex=4,col=2)
#legend("topright",c("lambda=(6,3)","lambda=(0,0)"),col=c(1,1),lwd=c(2,1),cex=.9)
legend("topleft", legend = c(expression(paste(lambda, " = (6, 3)  ", )),
                             expression(paste(lambda, " = (0, 0)  ", ))), col=c(1,1),lwd=c(2,1),cex=.9)



contour(x,y,z1)
contour(x,y,z1,cex=0.05,lwd=4,add=TRUE)
ss <- multiroot(f=fun2, start=as.numeric(d1[which(pdf1==max(pdf1))[1],]),lambda=alpha)
points(ss$root[1],ss$root[2],,pch="*",cex=4,col=2)
#legend("topright",c("lambda=(6,3)"),col=c(1,1),lwd=c(2,1),cex=.9)
legend("topleft", legend = c(expression(paste(lambda, " = (6, 3)  ", ))), 
       col=c(1,1),lwd=c(2,1),cex=.9)
abline(h=0,v=0)






##############################################
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Solve for mode of biavariate skewed normal



fun2=function(z,lambda=c(0,0)){
  F1= 0-z[1]*pnorm(as.numeric(lambda%*%z))+lambda[1]*dnorm(as.numeric(lambda%*%z))
  F2= 0-z[2]*pnorm(as.numeric(lambda%*%z))+lambda[2]*dnorm(as.numeric(lambda%*%z))
  
  return(c(F1=F1,F2=F2))
}


alpha=c(-6,-3)


ss <- multiroot(f=fun2, start = c(.37,.37),lambda=alpha)
# .37 is the maximum mode this works for small alphas less than 2

x <- seq(-3,3, length.out = 500)
y <- seq(-3, 3, length.out = 500)
d1 <- expand.grid(x = x, y = y)



pdf1 <- dmsn(d1, c(0,0), diag(c(1,1)), alpha=alpha)
contour(x,y,matrix(pdf1,500,500))


# Alternative using contours


ss <- multiroot(f=fun2, start=as.numeric(d1[which(pdf1==max(pdf1))[1],]),lambda=alpha)
points(ss$root[1],ss$root[2],,pch="*",cex=4,col=2)





################################## 

alpha<-c(-6,-3)
omega<-matrix(c(1,0,0,1),ncol=2)
omega

z1<-outer(x,y)
z2<-outer(x,y)
for(i in 1:length(x))
  for(j in 1:length(y)){
    z1[i,j]<-dmsn(cbind(x[i],x[j]), xi=c(0,0), omega, alpha, log=FALSE)
  }
for(i in 1:length(x))
  for(j in 1:length(y)){
    z2[i,j]<-dmsn(cbind(x[i],x[j]), xi=c(0,0), omega, alpha=c(0,0), log=FALSE)
  }
contour(x,y,z2)
contour(x,y,z1,cex=0.05,lwd=4,add=TRUE)
ss <- multiroot(f=fun2, start=as.numeric(d1[which(pdf1==max(pdf1))[1],]),lambda=alpha)
points(ss$root[1],ss$root[2],,pch="*",cex=4,col=2)
#legend("topright",c("lambda=(-6,-3)","lambda=(0,0)"),col=c(1,1),lwd=c(2,1),cex=.9)
abline(h=0,v=0)
legend("topleft", legend = c(expression(paste(lambda, " = (-6, -3)    ", )),
                             expression(paste(lambda, " = (0, 0)    ", ))), col=c(1,1),lwd=c(2,1),cex=.9)


contour(x,y,z1)
contour(x,y,z1,cex=0.05,lwd=4,add=TRUE)
ss <- multiroot(f=fun2, start=as.numeric(d1[which(pdf1==max(pdf1))[1],]),lambda=alpha)
points(ss$root[1],ss$root[2],,pch="*",cex=4,col=2)
#legend("topright",c("lambda=(-6,-3)"),col=c(1,1),lwd=c(2,1),cex=.9)
legend("topleft", legend = c(expression(paste(lambda, " = (-6, -3)    ", ))), 
       col=c(1,1),lwd=c(2,1),cex=.9)
abline(h=0,v=0)


######################################


#when lamda =0,0
#####################################################################################################

x<-seq(-3,3,by=0.1)
y<-seq(-3,3,by=0.1)

alpha<-c(0,0)
omega<-matrix(c(1,0,0,1),ncol=2)
omega

z1<-outer(x,y)
z2<-outer(x,y)
for(i in 1:length(x))
  for(j in 1:length(y)){
    z1[i,j]<-dmsn(cbind(x[i],x[j]), xi=c(0,0), omega, alpha, log=FALSE)
  }
for(i in 1:length(x))
  for(j in 1:length(y)){
    z2[i,j]<-dmsn(cbind(x[i],x[j]), xi=c(0,0), omega, alpha=c(0,0), log=FALSE)
  }
contour(x,y,z2)
contour(x,y,z1,cex=0.05,lwd=3,add=TRUE)
legend("topleft",c("lambda=(0,0)"),col=c(1,1),lwd=c(2,1),cex=1)
################################## 



# Solve for mode of biavariate skewed normal



fun2=function(z,lambda=c(0,0)){
  F1= 0-z[1]*pnorm(as.numeric(lambda%*%z))+lambda[1]*dnorm(as.numeric(lambda%*%z))
  F2= 0-z[2]*pnorm(as.numeric(lambda%*%z))+lambda[2]*dnorm(as.numeric(lambda%*%z))
  
  return(c(F1=F1,F2=F2))
}


alpha=c(0,0)


ss <- multiroot(f=fun2, start = c(.37,.37),lambda=alpha)
# .37 is the maximum mode this works for small alphas less than 2

x <- seq(-3,3, length.out = 500)
y <- seq(-3, 3, length.out = 500)
d1 <- expand.grid(x = x, y = y)



pdf1 <- dmsn(d1, c(0,0), diag(c(1,1)), alpha=alpha)
contour(x,y,matrix(pdf1,500,500))


# Alternative using contours


ss <- multiroot(f=fun2, start=as.numeric(d1[which(pdf1==max(pdf1))[1],]),lambda=alpha)
points(ss$root[1],ss$root[2],,pch="*",cex=4,col=2)




