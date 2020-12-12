data <- as.numeric(readLines("input-2020-10"))

# Part 1
data <- c(0,sort(data),max(data)+3)
prod(table(diff(data)))

#Part 2
r <- rle(diff(data))
7^sum(r$l==4&r$v==1)*4^sum(r$l==3&r$v==1)*2^sum(r$l==2&r$v==1)
