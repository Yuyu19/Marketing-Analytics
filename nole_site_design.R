library('BayesFactor')
library(dplyr)
library(ggplot2)
#1.the best combination
df[which.max(df$sales), ] #garnet westcott passive

#2b
?anovaBF
anovaBF(sales ~ color + pic + copy, data = df, whichRandom = NULL,
        progress=TRUE) #important factors# color + pic + color:pic + copy#
#FINAL MODEL#
#color + pic + color:pic + copy + color:copy + pic:copy + color:pic:copy#

#3 interaction plots
## 3 way interaction 
(cleaned_data <- df %>% 
    mutate(
      color = color %>% as.factor,
      pic = pic %>% as.factor,
      copy = copy %>% as.factor
    ) %>% 
    group_by(color, pic,copy) %>% 
    summarise(
      mean_diff = mean(sales, na.rm = TRUE)
    ) )
#color-pic
cleaned_data %>% 
  ggplot(aes(color, mean_diff)) +
  geom_line(size = 1.2, aes(group = pic, color = pic)) +
  geom_point(size = 2.6, aes(color = pic), shape = 15)
#color-copy
cleaned_data %>% 
  ggplot(aes(color, mean_diff)) +
  geom_line(size = 1.2, aes(group = copy, color = copy)) +
  geom_point(size = 2.6, aes(color = copy), shape = 15)
#copy-pic
cleaned_data %>% 
  ggplot(aes(copy, mean_diff)) +
  geom_line(size = 1.2, aes(group = pic, color = pic)) +
  geom_point(size = 2.6, aes(color = pic), shape = 15)


##2 way interaction 
(cleaned_data <- df %>% 
    mutate(
      color = color %>% as.factor,
      pic = pic %>% as.factor
    ) %>% 
    group_by(color, pic) %>% 
    summarise(
      mean_diff = mean(sales, na.rm = TRUE)
    ) )
#color-pic
cleaned_data %>% 
  ggplot(aes(color, mean_diff)) +
  geom_line(size = 1.2, aes(group = pic, color = pic)) +
  geom_point(size = 2.6, aes(color = pic), shape = 15)
