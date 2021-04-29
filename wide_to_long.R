library(tidyr)
set.seed(5675)
n <- 10

y1 <-  5 + rnorm(n) %>% round(digits=2)
y2 <- 15 + rnorm(n) %>% round(digits=2)
y3 <- 25 + rnorm(n) %>% round(digits=2)

demo <- sample(c("A", "B"), n, replace=TRUE)

df <- data.frame(demo, y1, y2, y3)
rm(demo, y1, y2, y3)

long <- pivot_longer(df, c("y1", "y2","y3"), names_to="t", values_to="y")



