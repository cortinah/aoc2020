library(tidyverse)
input <- tibble(line = readLines("input-2020-14")) %>%
  extract(line, "mask", "mask = ([01X]{36})", remove = FALSE) %>%
  extract(line, c("address", "value"), "mem\\[(\\d+)\\] = (\\d+)", convert = TRUE)
input

# Part 1

i2b <- function(value) {
  result <- logical(36)
  index <- 36
  while (value > 0) {
    result[index] <- value %% 2 == 1
    value <- value %/% 2
    index <- index - 1
  }
  result
}
b2d <- function(bits) {
  which(bits) %>%
    {36 - .} %>%
    {2 ** .} %>%
    sum()
}
apply_mask <- function(value, mask) {
  bits <- i2b(value)
  mask <- unlist(str_split(mask, ""))
  masked_bits <-
    map_lgl(1:36, function(index) {
      switch(mask[index],
             "1" = TRUE,
             "0" = FALSE,
             "X" = bits[index]
      )
    })
  b2d(masked_bits)
}

mem <- double()
for (i in 1:nrow(input)) {
  if (!is.na(input$mask[[i]])) {
    mask <- input$mask[[i]]
  } else {
    mem[input$address[[i]]] <- apply_mask(input$value[[i]], mask)
  }
}
options(digits = 22)
sum(mem, na.rm = TRUE)


# Part 2

library(hash)
expand_masks <- function(mask) {
  if (str_detect(mask, "X")) {
    c(
      expand_masks(str_replace(mask, "X", "0")),
      expand_masks(str_replace(mask, "X", "1"))
    )
  } else {
    b2d(unlist(str_split(mask, "")) == "1")
  }
}
apply_mask_v2 <- function(value, mask) {
  bits <- i2b(value)
  mask <- unlist(str_split(mask, ""))
  applied <- map_chr(1:36, function(index) {
    switch(mask[index],
           "1" = "1",
           "0" = if (bits[[index]]) "1" else "0",
           "X" = "X"
    )
  })
  expand_masks(str_c(applied, collapse = ""))
}
apply_mask_v2(42, "000000000000000000000000000000X1001X")


mem <- hash()
for (i in 1:nrow(input)) {
  if (!is.na(input$mask[[i]])) {
    mask <- input$mask[[i]]
  } else {
    addresses <- apply_mask_v2(input$address[[i]], mask)
    for (j in 1:length(addresses)) {
      mem[addresses[[j]]] <- input$value[[i]]
    }
  }
}
sum(values(mem))
