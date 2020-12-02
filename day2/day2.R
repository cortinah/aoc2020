library(readr)
data <- read_delim("input-2020-2",delim = ' ', col_names = F)


# Part 1
data$X2<-unlist(strsplit(data$X2,":"))
nums<-unlist(strsplit(data$X1,"-"))
data$X1<-as.numeric(nums[rep(c(T,F), 1000)])
data$X4<-as.numeric(nums[rep(c(F,T), 1000)])

library(purrr)
library(stringr)
data$sum <- map2_int(data$X3, data$X2, ~str_count(.x,.y))

data$check <- ifelse(data$sum<=data$X4 & data$sum>=data$X1, T, F)
table(data$check)

# Part 2
data$check1 <- substr(data$X3,data$X1,data$X1)==data$X2
data$check2 <- substr(data$X3,data$X4,data$X4)==data$X2
data$xor <- xor(data$check1, data$check2)
table(data$xor)
