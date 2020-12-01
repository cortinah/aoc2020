data <- as.numeric(readLines("input-2020-1"))

solve1 <- function ()
  for (i in data) {
    
    for (j in data) {
      
      if (i+j == 2020) return(i*j)
    } }


######
solve2 <- function()
  for (i in data) {

    for (j in data) {

     for (k in data) {

      if (i+j+k == 2020) return(i*j*k)
     } } }