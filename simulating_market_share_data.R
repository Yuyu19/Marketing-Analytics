A<-rbinom(1000,1,prob=1/3)
A
hist(A)
sum(A)

B<-rbeta(1000,1,2)
B
C<-rbinom(1000, 1, B)
C
hist(C)
sum(C)


