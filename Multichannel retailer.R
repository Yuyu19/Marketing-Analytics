library(dplyr)
by_cust<-group_by(lines.df,Cust_ID)
RFM.df<-summarise(by_cust, Recen=max(OrderDate),Freq=n(),Mon=sum(LineDollars))
summary(RFM.df)
hist(RFM.df$Recen)
hist(RFM.df$Freq)
hist(RFM.df$Mon)

install.packages("bayesm")
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