##Import data
cig <- read.csv("~/datathon-2021/Datasets/Active_Cigarette_Tobacco_Retailers.csv", header=TRUE)

##Filter out all Harris county data
library(dplyr)
cig %>% filter(County == "HARRIS" & City == "HOUSTON") -> cig2

##Extract lat-long data from mutate_geocode()
#install.packages("ggmap")
library(ggmap)
cig2$full_address <- paste0(cig2$Address, ' Houston, TX ', cig2$Zip.Code)
addresses <- data.frame(
  address = cig2$`full_address`,
  stringsAsFactors = FALSE
)

register_google(key = "AIzaSyCuy6doO0e43MAzGy8r0TB1ChsOJ9PXTHk")
longitude_df <- mutate_geocode(addresses, address)
longitude_df2 <- right_join(cig2, longitude_df, by = c("full_address" = "address"))
#Join zip code in
#dataframe2 <- right_join(longitude_df2, cig2, by = c("address" = "Address"))
longitude_df2 %>% select("Address", "lat", "lon", "Location.Name", "City", "State", "Zip.Code") -> dataframe3

#Import zip code median data
zip_codes <- read.csv("~/datathon-2021/Datasets/zip_codes.csv")

#Transform latitude longitude column
library(stringr)
lat_long <- str_split(zip_codes$Location, ", ")
lat <- rep(NA, 96)
long <- rep(NA, 96)
for(i in 1:96) {
  lat[i] = lat_long[[i]][1]
  long[i] = lat_long[[i]][2]
}

zip_codes$ziplat <- lat
zip_codes$ziplong <- long
dataframe4 <- right_join(zip_codes, dataframe3, by = "Zip.Code")

#Start plotting ggmap
library(sf)
library(mapview)
library(tidyr)
#dataframe4 %>% select("lon", "lat", "Location.Name") -> plotting 
#plotting %>% drop_na() -> plotting2

#plotting2 <- read.csv("~/datathon-2021/plotting2.csv")
locations_sf <- st_as_sf(plotting2[,2:4], coords = c("lon", "lat"), crs = 4326)
a <- mapview(locations_sf, legend = F)
a
#==================
##Lasso
library(glmnet)

qqqq <- as.data.frame.matrix(table(dataframe4$Zip.Code))
qqqq$Var2 <- as.numeric(as.character(qqqq$Var1))
yeshaha <- inner_join(density, qqqq, by = c("Zip.Code" = "Var2"))
yeshaha$pop <- as.numeric(gsub(",", "", yeshaha$Population))
yeshaha$sqmile <- yeshaha$pop / yeshaha$People...Sq..Mile
yeshaha$smokedensity <- yeshaha$Freq / yeshaha$sqmile
yeshaha2 <- yeshaha
yeshaha3 <- inner_join(yeshaha2, zip_codes[, c(2,6)], by = "Zip.Code")
plot(yeshaha3$Avg..Income.H.hold, yeshaha3$smokedensity)
