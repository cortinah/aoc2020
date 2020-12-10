data <- as.numeric(readLines("input-2020-9"))

# Part 1
check <- function(data, l=25) {
    for (i in 1:(length(data)-l)) {
        p <- expand.grid(data[i:(i+l-1)], data[i:(i+l-1)])
        p[,3] <- p[,1]+p[,2]
        p <- unique(p[,3])
        if (!data[i+l] %in% p) break()
    }
  return(data[i+l])
}

invalid <- check(data)

# Part 2
check <- function(data, len, inv) {
  for (i in 1:(length(data)-len)) {
     if (sum(data[i:(i+len-1)]) == inv)
       print(max((data[i:(i+len-1)]))+min((data[i:(i+len-1)])))
    }
}

range <- function(data, invalid)
  {for (i in 2:length(data)) check(data, i, invalid)}

range(data, invalid)
