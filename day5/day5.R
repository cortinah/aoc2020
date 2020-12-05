library(tidyverse)


data <- tibble(x=readLines("input-2020-5"))

pass <- function(code) {
  row <- c(l=0,u=127); seat <-c(l=0,u=7)
  
  for (i in 1:7) {
  if (substr(code,i,i)=='F') row <- c(l=unname(row['l']), u=unname((((row['u']-row['l'])+1)/2)+row['l']-1)) else
                    row <- c(l=unname((((row['u']-row['l'])+1)/2)+row['l']), u=unname(row['u']))}
  
  for (i in 8:10) {
    if (substr(code,i,i)=='L') seat <- c(l=unname(seat['l']), u=unname((((seat['u']-seat['l'])+1)/2)+seat['l']-1)) else
      seat <- c(l=unname((((seat['u']-seat['l'])+1)/2)+seat['l']), u=unname(seat['u']))}
  
  
  unname((row['u']*8)+seat['u'])
}

max(apply(data[,1],1, pass))


### Part 2
pass <- function(code) {
  row <- c(l=0,u=127); seat <-c(l=0,u=7)
  
  for (i in 1:7) {
    if (substr(code,i,i)=='F') row <- c(l=unname(row['l']), u=unname((((row['u']-row['l'])+1)/2)+row['l']-1)) else
      row <- c(l=unname((((row['u']-row['l'])+1)/2)+row['l']), u=unname(row['u']))}
  
  for (i in 8:10) {
    if (substr(code,i,i)=='L') seat <- c(l=unname(seat['l']), u=unname((((seat['u']-seat['l'])+1)/2)+seat['l']-1)) else
      seat <- c(l=unname((((seat['u']-seat['l'])+1)/2)+seat['l']), u=unname(seat['u']))}
  
  
  c(r=unname(row['u']),s=unname(seat['u']))
}

seats <- as.tibble(t(apply(data[,1],1, pass)))

seats %>% group_by(r) %>% summarize(count=n()) %>% arrange(count)
seats %>% filter(r==80)
80*8

                                    