library(tidyverse)

data <- tibble(x=readLines("input-2020-7"))

answers <- data %>% mutate(y=str_replace_all(x,' bags','')) %>%
  mutate(y=str_replace_all(y,' bag','')) %>%
  mutate(bag=str_extract(x,"^[a-z]+\\s[a-z]+")) %>%
  mutate(set1=str_extract(x,"contain\\s[1-9]\\s[a-z]+\\s[a-z]+")) %>%
  mutate(set2=str_extract(y, "(,\\s[1-9]\\s[a-z]+\\s[a-z]+){1}")) %>%
  mutate(set3=str_extract(y, "(,\\s[1-9]\\s[a-z]+\\s[a-z]+){2}")) %>%
  mutate(set4=str_extract(y,"[a-z]+\\s[a-z]+.$")) %>%
  select(bag,set1,set2,set3,set4)

answers <- answers %>% mutate(set1=str_extract(set1,"[a-z]+\\s[a-z]+")) %>%
  mutate(set4=str_replace(set4,"\\.",""))

## 3
answers %>% filter(set1=='shiny gold' | set2=='shiny gold')

answers %>% filter(set1=='dark cyan' | set2=='dark cyan' |
                     set1=='dull coral' | set2=='dull coral'|
                     set1=='clear gold' | set2=='clear gold')
