library(sn)
library(mixsmsn)
library(rootSolve)
library(expm)
#install.packages("fpc")
library(fpc)
#install.packages("MASS")
library(MASS)
##

  source("Ridgeline-different varince-Merging_final.R")



flow <- read.delim("../FlowCytometryData/LCL.020806plate7DQCD95A01.scatters.txt")


flow=flow[1:1000,]
flow.std=flow
flow.std[,1]=flow[,1]/sd(flow[,1])
flow.std[,2]=flow[,2]/sd(flow[,2])



set.seed(1)
mixsmsn.out<-smsn.mmix(flow.std, nu=3, g=5, get.init = TRUE, criteria = TRUE,
                       group = TRUE, family = "Skew.normal")

# mixsmsn.out<-smsn.mmix(flow, nu=3, g=5, get.init = TRUE, criteria = TRUE,
  #                     group = TRUE, family = "Skew.normal")




# plots with new program
aa=ridgeline.diagnosis.skew(mixsmsn.out, ridgelineplot = "merged",data=flow.std)


plot(flow.std,col=mixsmsn.out$group,pch=16)



plot(flow.std,col=aa$merged.group,pch=16)
plot(flow,col=aa$merged.group,pch=16)


plot(flow.std,col=aa$merged.group.ratio,pch=16)
plot(flow,col=aa$merged.group.ratio,pch=16)


aa=ridgeline.diagnosis.skew(mixsmsn.out, ridgelineplot = "all",data=flow.std)
aa=ridgeline.diagnosis.skew(mixsmsn.out, ridgelineplot = "matrix",data=flow.std)

aa=ridgeline.diagnosis.skew(mixsmsn.out,compute.ratio=TRUE, ridgelineplot = "merged",data=flow.std)
aa=ridgeline.diagnosis.skew(mixsmsn.out,compute.ratio=TRUE, ratiocutoff = .6, ridgelineplot = "merged",data=flow.std)









