library(tidyverse)
library(rvest)

baseURL <- 'https://en.wikipedia.org/wiki/List_of_national_parks_of_the_United_States'
listURL <- read_html(baseURL)

table <- listURL %>%
  html_nodes('.wikitable') %>%
  html_table()

write.csv(table, file = "NP_Information.csv", row.names=FALSE)
