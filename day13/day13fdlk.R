library(tidyverse)
input <- readLines("input-2020-13")
timestamp <- as.integer(input[[1]])
buses <- tibble(id = input[[2]]) %>%
  separate_rows(id) %>%
  mutate(row = as.integer(row_number())) %>%
  filter(id != "x") %>%
  type_convert(cols(id = col_integer()))
buses
# Part 1

first <-
  buses %>%
  mutate(wait = id - timestamp %% id) %>%
  filter(wait == min(wait))
first$id[[1]] * first$wait[[1]]


# Part 2

library(numbers)
normalize <- function(x, mod) {
  x <- x %% mod
  if (x < 0) {
    x <- x + mod
  }
  x
}
chinese_no_overflow <- function(a, n) {
  ans <- a[[1]]
  lcm <- n[[1]]
  for (i in 2:length(a)) {
    pom <- extGCD(lcm, n[i])
    d <- pom[[1]]
    x1 <- pom[[2]]
    if ((a[i] - ans) %% d != 0) {
      print("no solutions")
      return(NA)
    }
    
    ans <- normalize(
      ans + x1 * (a[i] - ans) %/% d %% (n[i] %/% d) * lcm,
      (n[i] %/% d) * lcm
    )
    lcm <- LCM(lcm, n[i])
  }
  ans
}
options(digits=22)
buses <- buses %>%
  mutate(mod = (id - row + 1))
chinese_no_overflow(buses$mod, buses$id)
