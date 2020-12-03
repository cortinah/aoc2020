library(readr)
library(stringr)

data <- readLines("input-2020-3")
l <- nchar(data[1])

data <- unlist(str_match_all(data, ".{1}"))
data <- matrix(data, ncol=l, byrow = T)

check <- function (m, pos) {
  l <- ncol(m)
  return(ifelse((m[pos[2],((pos[1]-1) %% l) +1]=='#'), 1, 0))
}


solve1 <- function(data,r=3,d=1) {
pos <- c(1,1)
trees <- 0L

while (pos[2] <= nrow(data)) {
  t <- check(data,pos)
  trees <- trees+t
  pos[1] <- pos[1]+r
  pos[2] <- pos[2]+d }

return(trees)
}

solve1(data)

#### Part 2
solve1(data,1,1)*solve1(data,3,1)*solve1(data,5,1)*solve1(data,7,1)*solve1(data,1,2)

