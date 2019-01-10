



install.packages("sn")

library(sn)


install.packages("rootSolve")



library(rootSolve)





#-----------------------------------------------------------
# Now starting for bivariate skew mixtures with equal variance to start with 


ridge.skew<-function(x,alpha1,alpha2,xi1,xi2,a){
  # lambda skewnees paramater and xi's the mean     
  z1=(x-xi1)
  z2=(x-xi2)
  
  comp1= -z1 + alpha1 * dnorm(alpha1%*%z1)/pnorm(alpha1%*%z1)
  comp2= -z2 + alpha2 * dnorm(alpha2%*%z2)/pnorm(alpha2%*%z2)
  
  return( ((1-a)*comp1+a*comp2))   
  
}




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#    %%%%%%%  NEW SLIDE
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ridgeline.skew<-function(alpha1,alpha2,xi1,xi2,by=.01){
  
  # d=length(xi1)
  avec <- seq(0,1,by=by)#alpha
  
  ridge=matrix(0,length(avec),2);
  dens=rep(0,length(avec));
  
  ind=1;
  
  mymode=xi2
  ind=1
  
  for ( a in avec){
    
    ss<- multiroot(f=function(x) ridge.skew(x,alpha1,alpha2,xi1,xi2,a), mymode)
    mymode<-ss$root
    ridge[ind,]=mymode;
    dens[ind]= (dmsn(mymode, xi1, diag(2), alpha1)+dmsn(mymode, xi2, diag(2), alpha2))/2
    ind=ind+1
    
    #points(mymode[1],mymode[2],pch="*",cex=2,col=2)
    
    
  }
  return(data.frame(alpha=avec,ridge=ridge,dens=dens))
}






contour.skew<-function(alpha1,alpha2,xi1,xi2,by=.01){
  x <- seq(-2,4, length.out = 500)
  y <- seq(-2, 4, length.out = 500)
  d1 <- expand.grid(x = x, y = y)
  
  
  
  #    xi1 <- c(0, 0)
  Omega1 <- diag(2)
  #     Omega[2,1] <- Omega[1,2] <- 0
  #     alpha1 <- c(0,a)
  pdf1 <- dmsn(d1, xi1, Omega1, alpha1)
  #   contour(x,y,matrix(pdf1,500,500))
  
  
  Omega2 <- diag(2)
  #    alpha2<- c(a,0)
  
  pdf2 <- dmsn(d1, xi2, Omega2, alpha2)
  pdf=0.5*(pdf1+pdf2)
  
  contour(x,y,matrix(pdf,500,500))
  #title(main="Two dimensional equal variance example with three modes",sub="mean at (0,0) and (1, -1) unit variance and skewness=10")
}




xi1=c(1,-1);
xi2=c(-1,1)
alpha1=c(-2,-4)
alpha2=rev(alpha1)

aa=ridgeline.skew(alpha1,alpha2,xi1,xi2)

par(mfrow=c(1,2))

contour.skew(alpha1,alpha2,xi1,xi2)


lines(aa$ridge.1,aa$ridge.2,col=2,lwd=2)

plot(aa$alpha,aa$dens,type="l",xlab=expression(alpha),ylab= "density")

####################################################################################################################
#####################################################################################


### for 3D  THE CODE IS
###########################################################





#-----------------------------------------------------------
# Now starting for bivariate skew mixtures with equal variance to start with 


ridge.skew<-function(x,alpha1,alpha2,xi1,xi2,a){
  # lambda skewnees paramater and xi's the mean     
  z1=(x-xi1)
  z2=(x-xi2)
  
  comp1= -z1 + alpha1 * dnorm(alpha1%*%z1)/pnorm(alpha1%*%z1)
  comp2= -z2 + alpha2 * dnorm(alpha2%*%z2)/pnorm(alpha2%*%z2)
  
  return( ((1-a)*comp1+a*comp2))   
  
}




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#    %%%%%%%  NEW SLIDE
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ridgeline.skew<-function(alpha1,alpha2,xi1,xi2,by=.01){
  
  
  ## Here we changed the x1i to d , to added extrea paremeter to get 4 mode 
  d=length(xi1)
  avec <- seq(0,1,by=by)#alpha
  
  ridge=matrix(0,length(avec),d);
  dens=rep(0,length(avec));
  
  ind=1;
  
  mymode=xi2
  ind=1
  
  for ( a in avec){
    
    ss<- multiroot(f=function(x) ridge.skew(x,alpha1,alpha2,xi1,xi2,a), mymode)
    mymode<-ss$root
    ridge[ind,]=mymode;
    dens[ind]= (dmsn(mymode, xi1, diag(d), alpha1)+dmsn(mymode, xi2, diag(d), alpha2))/2
    ind=ind+1
    
    #points(mymode[1],mymode[2],pch="*",cex=2,col=2)
    
    
  }
  return(data.frame(alpha=avec,ridge=ridge,dens=dens))
}




#3d example

xi2=c(0,0,0);
xi1=c(1/sqrt(2),2,1/sqrt(2))
alpha1=c(4,0,0)
alpha2=rev(alpha1)

aa=ridgeline.skew(alpha1,alpha2,xi1,xi2)
plot(aa$alpha,aa$dens,type="l",xlab=expression(alpha),ylab=expression(Density ))


##########
par(mfrow=c(1,1))

#3d example
xi2=c(0,0,0);
xi1=c(1,1.559,1.559)
alpha1=c(-2.2,-6.5,10)
alpha2=rev(alpha1)
aa=ridgeline.skew(alpha1,alpha2,xi1,xi2)
plot(aa$alpha,aa$dens,type="l",xlab=expression(alpha),ylab=expression(Density ))





#3d example
xi2=c(0,0,0);
xi1=c(-1,-1.559,-1.559)
alpha1=c(2.2,6.5,-10)
alpha2=rev(alpha1)
aa=ridgeline.skew(alpha1,alpha2,xi1,xi2)
plot(aa$alpha,aa$dens,type="l",xlab=expression(alpha),ylab=expression(Density ))













#########################
#3d example
xi2=c(0,0,0);
xi1=c(1,1.559,1.559)
alpha1=c(-2.2,-6.5,10)
alpha2=rev(alpha1)
aa=ridgeline.skew(alpha1,alpha2,xi1,xi2)
plot(aa$alpha,aa$dens,type="l")









#5d example



xi2=rep(0,5);
xi1=rep(1,5)
alpha1=rep(0,5);
#alpha1=c(4,0,0,3,2)
alpha2=rev(alpha1)

aa=ridgeline.skew(alpha1,alpha2,xi1,xi2)
plot(aa$alpha,aa$dens,type="l")


