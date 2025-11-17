###############################################################################
#
# This script reads in the output files containing SOS time series data 
# (sps-data.csv) and metrics (sps-metrics.csv) and outputs data in the format
# for the SWP Data Library (data.salmonwatersheds.ca)
#
# Created by Steph Peacock
# on September 18, 2025
###############################################################################
library(dplyr)

# Load sps output

sps_data <- read.csv("output/sps-data.csv")
sps_metrics <- read.csv("output/sps-metrics.csv")
sps_trends <- read.csv("output/sps-trends_plotting.csv")

###############################################################################
# 	dataset 551: Time series of Regional Salmon Abundance by species. 
# This dataset contains both the raw annual abundance and the smoothed time 
# series shown in the trends plots.
###############################################################################

names(sps_data)

dat551 <- sps_data %>% left_join(
	sps_trends %>% 
		dplyr::select(region, species, year, spawners, total_return) %>% 
		rename(
			"spawnersAnomaly" = spawners, 
			"runsizeAnomaly" = total_return)) %>%
	dplyr::select(
		"region", "species", "year", "spawners", "smoothedSpawners", "spawnersAnomaly", "runsize", "smoothedRunsize","runsizeAnomaly", "spawners_short_trend",  "spawners_short_trend_lwr",  "spawners_short_trend_upr",  "spawners_long_trend", "spawners_long_trend_lwr", "spawners_long_trend_upr",  "runsize_short_trend", "runsize_short_trend_lwr", "runsize_short_trend_upr", "runsize_long_trend", "runsize_long_trend_lwr", "runsize_long_trend_upr",  "source_id") %>%
	mutate(datasetversion = strftime(Sys.Date(), format = "%Y%m%d"))

names(dat551)

# Round abundance numbers to whole
var_to_smooth <- c( "spawners", "smoothedSpawners", "runsize", "smoothedRunsize", "spawners_short_trend",  "spawners_short_trend_lwr",  "spawners_short_trend_upr",  "spawners_long_trend", "spawners_long_trend_lwr", "spawners_long_trend_upr",  "runsize_short_trend", "runsize_short_trend_lwr", "runsize_short_trend_upr", "runsize_long_trend", "runsize_long_trend_lwr", "runsize_long_trend_upr")

dat551[, var_to_smooth] <- round(dat551[, var_to_smooth])


# Write
write.csv(dat551, file = "../Data Library/dataset551_sps-data.csv", row.names = FALSE)
write.csv(dat551, file = paste0("../Data Library/archive/dataset551_sps-data_", Sys.Date(), ".csv"), row.names = FALSE)


###############################################################################
# dataset 552: State and Trends in Salmon Abundance for each region and 
# species, including the values for the current state, short-term trend, 
# long-term trend, the generation length, and the years of data.
###############################################################################


names(sps_metrics)

dat552 <- sps_metrics %>%
	mutate(datasetversion = strftime(Sys.Date(), format = "%Y%m%d"))

var_to_smooth <- c("current_abundance", "average_abundance", "previous_gen_abundance")
dat552[, var_to_smooth] <- round(dat552[, var_to_smooth])

# Add NAs as requested by Katy (https://pacificsalmonfdn.slack.com/archives/CKNVB4MCG/p1763413860237679?thread_ts=1762990928.939799&cid=CKNVB4MCG)
dat552$short_trend_cat[dat552$short_trend_cat == ""] <- NA 
dat552$long_trend_cat[dat552$long_trend_cat == ""] <- NA


# Write
write.csv(dat552, file = "../Data Library/dataset552_sps-metrics.csv", row.names = FALSE)
write.csv(dat552, file = paste0("../Data Library/archive/dataset552_sps-metrics_", Sys.Date(), ".csv"), row.names = FALSE)


