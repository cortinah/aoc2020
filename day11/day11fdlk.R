library(tidyverse)
library(Matrix)
input <- read_csv(file = "input-2020-11", col_names = c("seat"))
t <- input %>%
  separate_rows(seat, sep = "", convert = TRUE) %>%
  drop_na()
seats <- matrix(data = t$seat, nrow = nrow(input), byrow = TRUE)

seats<-seats[,-1]
dim(seats)

seats <- seats[]=='L'
life_step <- function(seats, d) {
  # form the neighboring sums
  nrow <- dim(d)[[1]]
  ncol <- dim(d)[[2]]
  d_eu <- rbind(d[-1, , drop = FALSE], 0)
  d_ed <- rbind(0, d[-nrow, , drop = FALSE])
  d_le <- cbind(d[, -1, drop = FALSE], 0)
  d_re <- cbind(0, d[, -ncol, drop = FALSE])
  d_lu <- cbind(d_eu[, -1, drop = FALSE], 0)
  d_ru <- cbind(0, d_eu[, -ncol, drop = FALSE])
  d_ld <- cbind(d_ed[, -1, drop = FALSE], 0)
  d_rd <- cbind(0, d_ed[, -ncol, drop = FALSE])
  pop <- d_eu + d_ed + d_le + d_re + d_lu + d_ru + d_ld + d_rd
  d <- (!d & pop == 0) | (d & pop < 4)
  d & seats
}
state <- seats & FALSE
next_state <- life_step(seats, state)
while (any(next_state != state)) {
  state <- next_state
  next_state <- life_step(seats, state)
}
sum(state)
#2386

####


seat_coords <- as_tibble(which(seats, arr.ind = TRUE)) %>%
  mutate(number = row_number())

num_seats <- nrow(seat_coords)

seat_num <- function(i, j) {
  filter(seat_coords, row == i & col == j)$number
}


neighbor_seat <- function(i, j, di, dj) {
  while (((i <- i + di) %in% 1:nrow(seats)) &
         ((j <- j + dj) %in% 1:ncol(seats))) {
    if (seats[i, j]) {
      return(seat_num(i, j))
    }
  }
  NA
}


neighbors <- function(i, j) {
  n <- c(
    neighbor_seat(i, j, -1, -1),
    neighbor_seat(i, j, 0, -1),
    neighbor_seat(i, j, 1, -1),
    neighbor_seat(i, j, 1, 0),
    neighbor_seat(i, j, 1, 1),
    neighbor_seat(i, j, 0, 1),
    neighbor_seat(i, j, -1, 1),
    neighbor_seat(i, j, -1, 0)
  )
  n <- n[!is.na(n)]
  sparseMatrix(
    i = rep(seat_num(i, j), length(n)),
    j = n,
    dims = c(num_seats, num_seats)
  )
}


neighbor_matrix <- sparseMatrix(c(), c(), dims = c(num_seats, num_seats))
for (number in 1:num_seats) {
  row <- seat_coords$row[[number]]
  col <- seat_coords$col[[number]]
  neighbor_matrix <- neighbor_matrix + neighbors(row, col)
}
dim(neighbor_matrix)


life_step <- function(d) {
  pop <- (neighbor_matrix %*% d)[, 1]
  d <- (!d & pop == 0) | (d & pop < 5)
}
state <- rep(0, num_seats)
next_state <- life_step(state)
while (any(next_state != state)) {
  state <- next_state
  next_state <- life_step(state)
}
sum(state)
