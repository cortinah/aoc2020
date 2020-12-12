data <- read.csv("input-2020-11", header = F)

# Part 1
data <- data.frame(do.call("rbind", strsplit(as.character(data$V1), "", fixed = TRUE)))

evolve <- function(data) {
  data_out <- data
  for (i in 1:nrow(data)) {
      for (j in 1:ncol(data)) {
      
      a<-as.character(data[i-1,j]); b<-as.character(data[i-1,j+1]);
      c<-as.character(data[i,j+1]); d<-as.character(data[i+1,j+1])
      e<-as.character(data[i+1,j]); f<-as.character(data[i+1,j-1]);
      g<-as.character(data[i,j-1]); h<-as.character(data[i-1,j-1])
      
      n <- c(a,b,c,d,e,f,g,h); n <- n[!is.na(n)]
      
      if (data[i,j]=='L') {
            if (any(n=='#')==F) data_out[i,j]='#'
                } 
      
      if (data[i,j]=='#') { 
            if (sum(n=='#') >= 4) data_out[i,j]='L'
        } 
     }
  }
  return(data_out)   }

loop <- function(data) {
  data_out <- evolve(data)
  while(any(data_out != data)) {
    data <- data_out
    data_out <- evolve(data) }
  return(data_out) }

data_out <- loop(data)
sum(data_out=='#')

# Part 2

evolve2 <- function(data) {
  data_out <- data
  for (i in 1:nrow(data)) {
      for (j in 1:ncol(data)) {
      
      a<-as.character(data[1:i-1,j]); b<-as.character(data[i-1,j+1]);
      c<-as.character(data[i,j+1]); d<-as.character(data[i+1,j+1])
      e<-as.character(data[i+1,j]); f<-as.character(data[i+1,j-1]);
      g<-as.character(data[i,j-1]); h<-as.character(data[i-1,j-1])
      
      n <- c(a,b,c,d,e,f,g,h); n<-n[!is.na(n)]
      
      if (data[i,j]=='L') {
        if (any(n=='#')==F) data_out[i,j]='#'
      } 
      
      if (data[i,j]=='#') { 
        if (sum(n=='#') >= 5) data_out[i,j]='L'
      } 
    }
  }
  return(data_out) }


loop2 <- function(data) {
  data_out <- evolve2(data)
  while(any(data_out != data)) {
    data <- data_out
    data_out <- evolve(data) }
  return(data_out)
}

data_out <- loop2(data)
sum(data1=='#')
