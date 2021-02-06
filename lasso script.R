crashes <- read.csv("~/datathon-2021/Datasets/data_new/crashes 2016.csv")

cig <- read.csv("~/datathon-2021/Datasets/Active_Cigarette_Tobacco_Retailers.csv", header=TRUE)

##Filter out all Harris county data
library(dplyr)
library(stringr)
cig %>% filter(County == "HARRIS" & City == "HOUSTON") -> cig2
#zip_codes <- read.csv("~/datathon-2021/Datasets/zip_codes.csv")
lat_long <- str_split(zip_codes$Location, ", ")
lat <- rep(NA, 96)
long <- rep(NA, 96)
for(i in 1:96) {
  lat[i] = lat_long[[i]][1]
  long[i] = lat_long[[i]][2]
}

zip_codes$ziplat <- lat
zip_codes$ziplong <- long
dataframe4 <- right_join(zip_codes, cig2, by = "Zip.Code")

qqqq <- data.frame(table(dataframe4$Zip.Code))
qqqq$Var2 <- as.numeric(as.character(qqqq$Var1))
density <- read.csv("~/datathon-2021/Datasets/density.csv")
yeshaha <- inner_join(density, qqqq, by = c("Zip.Code" = "Var2"))
yeshaha$pop <- as.numeric(gsub(",", "", yeshaha$Population))
yeshaha$sqmile <- yeshaha$pop / yeshaha$People...Sq..Mile
yeshaha$smokedensity <- yeshaha$Freq / yeshaha$sqmile
yeshaha2 <- yeshaha
yeshaha3 <- inner_join(yeshaha2, zip_codes[, c(2,6)], by = "Zip.Code")
plot(yeshaha3$Avg..Income.H.hold, yeshaha3$smokedensity)

library(glmnet)
x_vars <- model.matrix(Avg..Income.H.hold ~ People...Sq..Mile + smokedensity, yeshaha3)[, -1]
y_var <- yeshaha3$Avg..Income.H.hold
lambda_seq <- 10^seq(2, -2, by = -.1)

set.seed(86)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(x_vars[train,], y_var[train],
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[x_test,])

final <- as.data.frame(cbind(y_test, pred))

sum((final$y_test - final$`1`)^2) / 48
# Checking the first six obs
head(final)
coef(lasso_best)

library(choroplethr)
library(choroplethrZip)

#Transform yeshaha3
library(RColorBrewer)
library(viridis)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

heatmap_data <- dplyr::select(yeshaha3, c("Zip.Code", "People...Sq..Mile", "smokedensity", "Avg..Income.H.hold"))
hm1 <- heatmap_data[,c(1, 3)]
colnames(hm1) <- c("region", "value")
hm1$region <- as.character(hm1$region)
map1 <- zip_choropleth(hm1,
               county_zoom = 48201,
               zip_zoom = heatmap_data$Zip.Code,
               title       = "Density of Smoking Retailers by Zip Code",
               legend      = "Smoke Shop Density") + theme_map()
map1$ggplot_scale =  scale_fill_brewer(name="Population", palette=2, drop=FALSE)
map1

map3 <- ZipChoropleth$new(hm1)
map3$title <- "Density of Smoking Retailers by Zip Code"
map3$ggplot_scale <- scale_fill_brewer(palette = "OrRd")
map3$set_zoom_zip(state_zoom = NULL, county_zoom = 48201, msa_zoom = NULL,
                  zip_zoom = heatmap_data$Zip.Code)
a <- map3$render()
a + theme_map()

hm2 <- heatmap_data[,c(1, 4)]
colnames(hm2) <- c("region", "value")
hm2$region <- as.character(hm2$region)
zip_choropleth(hm2,
               county_zoom = 48201,
               zip_zoom = heatmap_data$Zip.Code,
               title       = "Houston Income Demographics",
               legend      = "Household Income Bracket")  + theme_map()  

hm2$value <- -1 * hm2$value
map4 <- ZipChoropleth$new(hm2)
map4$title <- "Houston Income Demographics"
map4$ggplot_scale <- scale_fill_brewer(palette = "OrRd")
map4$set_zoom_zip(state_zoom = NULL, county_zoom = 48201, msa_zoom = NULL,
                  zip_zoom = heatmap_data$Zip.Code)
b <- map4$render()
b + theme_map()  + labs(subtitle = "Darker areas have lower median houehold incomes.") + theme(legend.position = "none")

hm5 <- heatmap_data[-c(1, 88, 18, 89, 93),]
ggplot(data = hm5, aes(x = smokedensity, y = Avg..Income.H.hold)) + geom_point(alpha = 0.75) + geom_smooth(method = "lm", color = "#ea5455") + theme_minimal() + xlab("Density of smoke shops in shops/square mile") + ylab("Median Hosehold Income") +
  ggtitle("Negative Correlation Between Median Household Income and 
          Smoke Shop Density") + theme(legend.position = "none")
