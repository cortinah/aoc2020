library(tidyverse)
data <- read.csv("input-2020-13",sep=',',header = F)
# Part 1
data <- data %>% select_if(.,is.numeric)
  
ts<- data[1,1]
data <- data[-1,]
data <- as_data_frame(t(data))
colnames(data)=c('id')
data <- data %>% mutate(rounds=ceiling(ts/id)) 
data %>% mutate(wait=(id*rounds)-ts) %>% mutate(id*wait) %>% arrange(wait)



# Part 2
data <- read.csv("input-2020-13",sep=',',header = F, skip = 1)
colnames(data)=1:ncol(data)
data <- data %>% select_if(.,is.numeric)


i=1
repeat{ 
t = i*19

t2 = (t %% 41) -9
t3 = (t %% 643) -19
t4 = (t %% 17) -36
t5 = (t %% 13) -37
t6 = (t %% 23) -42
t7 = (t %% 509) -50
t8 = (t %% 37) -56
t9 = (t %% 29) -79

if (t2==0 & t3==0 & t4==0 & t5==0
    & t6==0 & t7==0 & t8==0 & t9==0) {print(t);break()}
i=i+1
  
}
