library(tidyverse)

data <- tibble(x=readLines("input-2020-6"))

answers <- data %>% mutate(group=cumsum(x=="")) %>% 
  filter(x !='') %>%
  add_count(group, name='group_total') %>%
  separate_rows(x,sep="") %>%
  filter( x != "")

answers %>% distinct(group, x)

##part 2
answers %>% count(group, x, group_total) %>%
  filter(n==group_total)


## first try
group <- data %>% mutate(group=cumsum(x==""))
group <- group %>% filter(!x=="")

g <-group %>% group_by(group) %>% summarise_all(funs(trimws(paste(., collapse = ''))))

g[,3]<-map_int(g[,2,drop=T], ~sum(!!str_count(., letters)))
colSums(g[,3])