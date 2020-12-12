library(tidyverse)
data <- read.csv("input-2020-12", header = F)

# Part 1
data <- data %>% mutate(dir=substr(V1,1,1), n=as.numeric(substr(V1,2,4)))
data <- data %>% select(dir, n)

solve <- function(data){
pos <- c(x=0,y=0); dir <- 2
dirs <- c('N','E','S','W')

  for (i in 1:nrow(data)){
    if (data[i,1]=='R') {dir=1+((dir-1+data[i,2]/90) %% 4)}
    if (data[i,1]=='L') {dir=1+((dir-1-data[i,2]/90) %% 4)}
    if (data[i,1]=='S') {pos['y']=pos['y']-data[i,2]}
    if (data[i,1]=='N') {pos['y']=pos['y']+data[i,2]}
    if (data[i,1]=='W') {pos['x']=pos['x']-data[i,2]}
    if (data[i,1]=='E') {pos['x']=pos['x']+data[i,2]}
    if (data[i,1]=='F')       {
        if (dir==1) {pos['y']=pos['y']+data[i,2]}
        if (dir==2) {pos['x']=pos['x']+data[i,2]}
        if (dir==3) {pos['y']=pos['y']-data[i,2]}
        if (dir==4) {pos['x']=pos['x']-data[i,2]}
                               }    }
unname(abs(pos['x']) + abs(pos['y']))  }

solve(data)

# Part 2
solve <- function(data,iter=nrow(data)){
  pos <- c(x=0, y=0); way <- c(x=10, y=1)
  
  for (i in 1:iter){
     
    if (data[i,1]=='R') {
      times=data[i,2]/90
      for (k in 1:times) {
        x2=way['y']; y2=-way['x']
        way['x']<-x2
        way['y']<-y2 }}
    
    if (data[i,1]=='L') {
      times=data[i,2]/90
      for (k in 1:times) {
        x2=-way['y']; y2=way['x']
        way['x']<-x2
        way['y']<-y2 } } 
    
    if (data[i,1]=='S') {way['y']=way['y']-data[i,2]}
    if (data[i,1]=='N') {way['y']=way['y']+data[i,2]}
    if (data[i,1]=='W') {way['x']=way['x']-data[i,2]}
    if (data[i,1]=='E') {way['x']=way['x']+data[i,2]}
    
    if (data[i,1]=='F') {  
      pos['y']=pos['y'] + (data[i,2]*way['y'])
      pos['x']=pos['x'] + (data[i,2]*way['x'])
    }
  }
 unname(abs(pos['x'])+abs(pos['y']))  }

solve(data)
