library(tidyverse)
library(DescTools)


# read data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
asset = read.csv("C:/Users/TOsosanya/Desktop/NSW/randgen model Assets.csv")
attach(asset)

Xn = MySeed_T0
Xn = 4*Xn*(1-Xn)
#Xn = 4*Xn*(1-Xn)
MyRandom = (2/pi) * sin(pi * ((Xn)^(0.5)))


#Functions

set.seed(3)

GenRandDef = function(X,Y){
  runif(X*Y*100)
}

EncodeCond = function(X,Y){
  X * 10^(Y-1)
}

# Recover individual condition score from the overall condition hash
DecodeCond = function(X,Y){
  floor(X / (10^(Y-1))) - 10*floor(X / (10^Y))
}

# Deteriorate a component based upon a randomly generated number
DetElement = function(X,Z){
  ifelse(X == 0, 0, ifelse(Z > 0.9, 2, ifelse(Z > 0.7, 1, 0)))
}

# Deteriorate condition of components in the condition hash based on position reference
DetHashElem = function(X,Y,Z){
  EncodeCond(ifelse(DecodeCond(X,Z) + DetElement(DecodeCond(X,Z),GenRandDef(Y,Z))>9,9,
                     DecodeCond(X,Z) + DetElement(DecodeCond(X,Z),GenRandDef(Y,Z))),Z)
}

AverageCond = function(X){
  aggregate(range(1,5,1),DecodeCond(ConditionHash,X),mean)
}


# Run expressions

# read in compA in position 1, compB in position 2 etc. AND add them all together
ConditionHash <- EncodeCond(CompA_T0,1)+
  EncodeCond(CompB_T0,2)+
  EncodeCond(CompC_T0,3)+
  EncodeCond(CompD_T0,4)+
  EncodeCond(CompE_T0,5)
ConditionHash

# deteriorate each comp condition 
ConditionHash = ifelse(ConditionHash>0,DetHashElem(ConditionHash,Xn,1), ConditionHash)
ConditionHash
ConditionHash = ifelse(ConditionHash>0,ConditionHash + DetHashElem(ConditionHash,Xn,2), ConditionHash)
ConditionHash
ConditionHash = ifelse(ConditionHash>0,ConditionHash + DetHashElem(ConditionHash,Xn,3), ConditionHash)
ConditionHash
ConditionHash = ifelse(ConditionHash>0,ConditionHash + DetHashElem(ConditionHash,Xn,4), ConditionHash)
ConditionHash
ConditionHash = ifelse(ConditionHash>0,ConditionHash + DetHashElem(ConditionHash,Xn,5), ConditionHash)
ConditionHash


print(AverageCond(0))


# for each timesteps 10% of CompA moves to B and CompB to C and Comp C to D etc.

Xn
DetHashElem(ConditionHash,Xn,1)
DetHashElem(ConditionHash,Xn,3)





