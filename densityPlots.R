library(data.table)
library(rLakeAnalyzer)
library(dplyr)
rm(list = ls())
#custom to allow different palettes
wtr.heat.map_customColor <- function (wtr, ...) 
{
  depths = get.offsets(wtr)
  n = nrow(wtr)
  wtr.dates = wtr$datetime
  wtr.mat = as.matrix(wtr[, -1])
  y = depths
  filled.contour(wtr.dates, y, wtr.mat, ylim = c(max(depths), 
                                                 0), nlevels = 100, 
                  ylab = "Depths (m)", ...)
}


# orig_model <- fread('densityPlots/original_model_GLM.csv')
# hybrid_model <- fread('densityPlots/hybrid_model_GLM-xgBoosted.csv')
# obs <- fread('densityPlots/obsmod.csv')

twtr <- load.ts('densityPlots/hybrid_model_GLM-xgBoosted.tsv')

#return df with all col with `temp` converted to density
getTempDF <- function(df) {
  tempCols <- grep(pattern = "temp", x = names(df), value = TRUE)
  nonTempCols <- grep(pattern = "temp", x = names(df), value = TRUE, invert = TRUE)
  newDF <- df[nonTempCols]
  #loop over temp cols to convert each to density
  for(c in tempCols) {
    rhoColName <- sub(pattern = "temp", replacement = "wtr", x = c)
    newDF[[rhoColName]] <- water.density(df[[c]])
  }
  return(newDF)
}

#subtract density of layer below from current layer
#don't pass in date col!
laggedCol <- function(x) {
  diff <- x - shift(x, type = "lead") 
  return(diff)
}

getDensityDiff <- function(df) {
  dates <- twtr$datetime
  noDates <- t(select(twtr, -datetime))
  twtr_diff <- apply(X = noDates, MARGIN = 2, FUN = laggedCol)
  #back to GLM format
  final <- data.frame(datetimes = dates, t(twtr_diff))
  return(final)
}
twtr <- getTempDF(as.data.frame(twtr))
twtr <- getDensityDiff(twtr)
wtr.heat.map_customColor(twtr[10000:11000,], plot.title = title("Density of layer minus layer below"), 
                         color.palette=colorRampPalette(c("blue","white","red"), 
                                 bias = 1, space = "rgb"), zlim = c(-0.5, .5), key.title = title("kg/m^3"))


#make it long
library(tidyr)
library(lubridate)
twtr_long <- gather(twtr, depth, deltaRho, -datetimes)
twtr_long <- mutate(twtr_long, doy = yday(datetimes), year = year(datetimes),
                    depth_num = as.numeric(gsub(pattern = "wtr_", replacement = "", x = depth)))

#now group by, summarize (sum negs), heatmap
twtr_group <- filter(twtr_long, depth_num > 0) %>% group_by(doy, depth_num) %>% 
            summarize(sum = sum(deltaRho)) 

ggplot(twtr_group, aes(doy, depth_num)) + 
  geom_tile(aes(fill = sum), colour = "white") + 
  scale_fill_gradient(low = "red", high = "white") + scale_y_reverse() + 
  ggtitle("Total seasonal density instability") + ylab("Depth (m)") + xlab("Day of year") +
  labs(fill = "Sum\ndensity\ninstability")

#now through time
twtr_byYear <- filter(twtr_long, depth_num > 0) %>% group_by(year, depth_num) %>% 
  summarize(sum = sum(deltaRho)) 
ggplot(twtr_byYear, aes(year, depth_num)) + 
  geom_tile(aes(fill = sum), colour = "white") + 
  scale_fill_gradient(low = "red", high = "white") + scale_y_reverse() + 
  ggtitle("Density instability by year") + ylab("Depth (m)") + xlab("Year") +
  labs(fill = "Sum\ndensity\ninstability")
