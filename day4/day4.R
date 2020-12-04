library(tidyverse)


data <- tibble(x=readLines("input-2020-4"))

required <- c('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid')

fields <- data %>% mutate(passport=cumsum(x=="")) %>%
  mutate(m=str_match_all(x, "(...)\\:([^ ]+)")) %>%
  mutate(f = map(m, ~ .[, 2]),
         v = map(m, ~ .[, 3])) %>%
  unnest(c(f,v)) %>%
  filter(f %in% required)

fields %>% count(passport) %>% summarise(answer=sum(n==7))

####

fields %>%
  extract(v, c("height","unit"), "(\\d+)(cm|in)", convert = T, remove = F) %>%
  mutate(valid = case_when(f=='byr' ~ between(as.integer(v), 1920, 2002),
                           f=='iyr' ~ between(as.integer(v), 2010, 2020),
                           f=='ecl' ~ v %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
                           f=='eyr' ~ between(as.integer(v), 2020, 2030),
                           f=='hgt' ~ ifelse(unit == 'cm', between(height, 150, 193),
                                             between(height, 59, 76)),
                           f=='hcl' ~ str_detect(v, "^#[0-9a-f]{6}$"),
                           f=='pid' ~ str_detect(v, "^[0-9]{9}$"))) %>%
  filter(valid) %>%
  count(passport) %>%
  summarise(answer=sum(n==7))

         