##Import data
cig <- read.csv("~/datathon-2021/Datasets/Active_Cigarette_Tobacco_Retailers.csv", header=TRUE)

##Filter out all Harris county data
library(dplyr)
cig %>% filter(County == "HARRIS") -> cig2

##Extract lat-long data from mutate_geocode()
#install.packages("ggmap")
library(ggmap)

addresses <- data.frame(
  address = cig2$`Address`,
  stringsAsFactors = FALSE
)

register_google(key = "AIzaSyCuy6doO0e43MAzGy8r0TB1ChsOJ9PXTHk")
longitude_df <- mutate_geocode(addresses, address)
