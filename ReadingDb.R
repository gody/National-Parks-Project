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
np_test <- np %>%
  separate(col = "Area" , into = c("Area (acres)", "Area (kms)"), sep = " acres \\(") %>%
  mutate(`Area (acres)` = as.numeric(`Area (acres)`))

np_test$`Area (acres)`

write_rds(tidy_visits, path = 'data/NP Visits.rds')
write_rds(np, path = 'data/NP General Information.rds')

###### WORK IN PROGRESS

library(rgdal)
states <- readOGR("National_Park_Service__Park_Unit_Boundaries/National_Park_Service__Park_Unit_Boundaries.shp",
                  layer = "National_Park_Service__Park_Unit_Boundaries", GDAL1_integer64_policy = TRUE)
library(maptools)
crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
parks <- readShapePoly("National_Park_Service__Park_Unit_Boundaries/National_Park_Service__Park_Unit_Boundaries.shp",proj4string=crswgs84,verbose=TRUE)
