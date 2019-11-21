library(tidyverse)

## Read all data
np <- read.csv("data/NP_Information.csv", fileEncoding="UTF-8-BOM")

visits <- data.frame(
  Year = integer(),
  JAN = integer(),
  FEB = integer(),
  MAR = integer(),
  APR = integer(),
  MAY = integer(),
  JUN = integer(),
  JUL = integer(),
  AUG = integer(),
  SEP = integer(),
  OCT = integer(),
  NOV = integer(),
  DEC = integer(),
  PARK = character()
)

for (i in seq(1, nrow(np))) {
  temp <- read_csv(paste0("data/", np$File[i]), col_names = TRUE, skip = 2)
  temp <- mutate(temp, Park = np$Name[i])
  if ("Total" %in% colnames(temp)) {
    temp <- select(temp, -c(Total))
  }
  visits <- rbind(visits, temp)
}


## ---------------Tidy data----------------- ##

tidy_visits <- visits %>%
  gather(key='Month',value='Visits', JAN:DEC)

np_test <- separate(np, col = "Location" , into = c("State", "X", "Y", "Z"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
np_test <- subset(np_test, select = -c(X, Y))
np_test <- separate(np_test, col = "Z" , into = c("X", "Y", "Lat", "Long"), sep = " ")
np_test <- mutate(np_test, Lat = as.numeric(str_replace(Lat, ";","")), Long = as.numeric(iconv(Long, 'utf-8', 'ascii', sub='')))
np <- subset(np_test, select = -c(X, Y))

colnames(np)[colnames(np)=="Date.established.as.park.5..9."] <- "Date"
colnames(np)[colnames(np)=="Area..2018..10."] <- "Area"
np <- np %>%
  separate(col = "Area" , into = c("Area (acres)", "Area (kms)"), sep = " acres \\(") %>%
  mutate(Area = as.numeric(str_remove_all(`Area (acres)`,",")))

write_rds(tidy_visits, path = 'data/NP Visits.rds')
write_rds(np, path = 'data/NP General Information.rds')

#####

library(usmap)
library(ggplot2)

test <- np %>%
  group_by(state) %>%
  summarize(parks = n())

state_data <- data.frame(fips = c("01", "02", "04"), parks = c(1, 5, 8))
df <- map_with_data(test, na = 0)


plot_usmap("states",
  data = test, values = "parks", color = "dark green"
) + 
  scale_fill_continuous(
    low = "green", high = "dark green", name = "Number of Parks", label = scales::comma
  ) + 
  labs(title = "Amount of National Parks per State") +
  theme(legend.position = "right")

install.packages("treemap")
devtools::install_github("timelyportfolio/d3treeR")
library(treemap)
library(d3treeR)
species <- read_csv('data/species.csv')
b <- species %>%
  separate(col = "Park Name" , into = c("Park Name", "X"), sep = " National Park") %>%
  subset(select = -c(X))
write_rds(b, 'data/NP Species.rds')
  
test <- species %>%
  group_by(`Park Name`,Category, Order, Family) %>%
  summarise(size = n()) %>%
  filter(`Park Name` == "Acadia National Park")
  
t <- treemap(test,c("Category", "Order", "Family"), "size")
inter <- d3tree2( t ,  rootname = "General" )

