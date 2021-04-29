library("mlogit")
library(dfidx)
library(dplyr)
library(tidyr)
data("Heating", package = "mlogit")

H <- dfidx(Heating, choice = "depvar", varying = c(3:12))
H

m <- mlogit(depvar ~ ic + oc + income + agehed + rooms , H)
summary(m)

m <- mlogit(depvar ~ ic + oc | 0, H)
summary(m)

tabb<- data.frame(depvar=c("gc","gr","ec","er","hp"), y=c(1,2,3,4,5))
heat2 <- left_join(Heating, tabb, by="depvar")
heat_long1 <- pivot_longer(heat2, c("ic.gc", "ic.gr","ic.ec","ic.er","ic.hp"), names_to="t1", values_to="ic")
heat_long <- pivot_longer(heat_long1, c("oc.gc", "oc.gr","oc.ec","oc.er","oc.hp"), names_to="t2", values_to="oc")

heat_long["y"]=ifelse(heat_long["y"]==1,1,0)
heating = subset(heat_long, select = -c(depvar,idcase) )
summary(heating)

m <- mlogit(y ~ ic + oc + income + agehed + rooms , heating)
summary(m)


Data <-list(y=y,x=x, p=3)
MCMC <- list(R=10000, nprint=25000)

draws<- rmnpGibbs(Data=Data, Mcmc =MCMC)


library(bayesm)

library(mlogit)

library(dplyr)



# gc - Gas  central (1)    gr - Gas  room (2)

# ec - Elec central (3)    er - Elec room (4)

# hp - Heat pump (5)



# Retrieve from the mlogit package

data(Heating)



# Convert depvar to numeric format and call it y

tabb  <- data.frame(depvar=c("gc", "gr", "ec", "er", "hp"), y=c(1,2,3,4,5))

heat2 <- left_join(Heating, tabb, by="depvar")



# Set up X matrix for Income type vars (literally income)

Xd <- cbind(heat2$income)



# Set up X for Price type vars (installation cost)

Xa <- cbind(heat2$ic.gc, heat2$ic.gr, heat2$ic.ec, heat2$ic.er, heat2$ic.hp)



# Toss them into createX()

X <- createX (p=5, na=1, nd=1, Xa=Xa, Xd=Xd, DIFF=TRUE)



# Input parameters

heatData <- list(y=heat2$y, X=X, p=5)

heatMCMC <- list(R=100000, nprint=250000)



# Run it

heatOut <- rmnpGibbs(Data=heatData, Mcmc=heatMCMC)



# Look at the structure of X

head(X)



# The different values output by rmnpGibs() are S3 objects

summary(heatOut$betadraw, burnin=10000)

plot(heatOut$betadraw)
