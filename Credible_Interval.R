library(ggplot2)
hist(credible$click)
library("MCMCpack")
A=credible%>%
  filter(ad=="A")
sum(A$click)
B=credible%>%
  filter(ad=="B")
sum(B$click)
posterior_a<-MCbinomialbeta(29,1005,alpha=1,beta = 1,mc=10000)
posterior_b<-MCbinomialbeta(90,997,alpha=1,beta = 1,mc=10000)
posterior_delta = posterior_a - posterior_b
summary(posterior_delta)
ggplot(data.frame(delta=as.numeric(posterior_delta)),aes(x=delta)) + geom_density() + theme_minimal()
ggplot(data.frame(delta=as.numeric(posterior_a)),aes(x=delta)) + geom_density() + theme_minimal()
ggplot(data.frame(delta=as.numeric(posterior_b)),aes(x=delta)) + geom_density() + theme_minimal()

HPDinterval(posterior_a, prob = 0.95)
HPDinterval(posterior_b, prob = 0.95)#b is better
