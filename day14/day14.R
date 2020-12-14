library(tidyverse)
data <- read.csv("input-2020-14",sep='=',header = F)

# Part 1
data <- data %>% separate(col=V1, into = c('inst', 'addr','t'), sep=c(4,-2),convert = T ) %>% select(addr, V2) %>% rename(val=V2) %>% mutate(addr=ifelse(is.na(addr),'mask',addr)) %>% mutate(mask=val)%>% mutate(val=as.numeric(val)) %>% mutate(mask=trimws(mask)) %>% separate(mask, sep=1:36,into=as.character(36:1))

data <- data %>% mutate(addr=as.numeric(addr)) %>% mutate_if(is.character, as.numeric)


for (i in 1:nrow(data)) {
  if (!is.na(data[i,2])) data[i,3:38]=as.binary(data[i,2],n=36)
}

maskf <-function(mask, val)  as.numeric(ifelse(is.na(mask),val,mask))

for (i in 1:nrow(data)) {
  if (is.na(data[i,2])) mask=data[i, 3:38]
  if (!is.na(data[i,2])) data[i, 3:38]=maskf(mask, data[i, 3:38])
}

data <- data %>% filter(!is.na(val))

for (i in 1:nrow(data)) {
  data[i,39] = as.numeric(as.binary(data[i,3:38],n=36,logic=T))
}
data <- data %>% select(addr, V39) %>% mutate(d=duplicated(addr))

data <- data %>% group_by(addr) %>% mutate(cum=cumsum(d))
data <- data %>% group_by(addr) %>% top_n(1)

sum(data$V39) #11884151942312
