###############################################################################
# Calculate metrics from sps-data.csv
# called on in docs/methods-results.Rmd 
# Author: Steph Peacock
# Date: Oct 29, 2023

###############################################################################

library(tidyverse)

# Write output files?
write.output <- FALSE

# Read in data 

# Compiled spawner and run size
sps_dat <- read.csv("output/sps-data.csv")

# Generation length
genLength <-read.csv("data/gen_length_regions.csv")

# # Number of streams
# numStreams <- read.csv("output/num-surveys/numStreams_all-regions.csv")

# Define variables
regions <- unique(unique(sps_dat$region))
species <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")

# Source script with code for plotting functions
source("code/functions.R")
source("code/colours.R")

###############################################################################
# Create metrics for each region and species
###############################################################################

# Add predictions of short- and long-term trends to sps_dat
sps_dat$spawners_short_trend <- NA
sps_dat$spawners_short_trend_lwr <- NA
sps_dat$spawners_short_trend_upr <- NA
sps_dat$spawners_long_trend <- NA
sps_dat$spawners_long_trend_lwr <- NA
sps_dat$spawners_long_trend_upr <- NA
sps_dat$runsize_short_trend <- NA
sps_dat$runsize_short_trend_lwr <- NA
sps_dat$runsize_short_trend_upr <- NA
sps_dat$runsize_long_trend <- NA
sps_dat$runsize_long_trend_lwr <- NA
sps_dat$runsize_long_trend_upr <- NA

# Note indices of these columns for filling in later...
columns.short <- rbind(grep("spawners_short_trend", names(sps_dat)),
											 grep("runsize_short_trend", names(sps_dat)))
											
columns.long <- rbind(grep("spawners_long_trend", names(sps_dat)),
											 grep("runsize_long_trend", names(sps_dat)))


# Set up empty data frame to store metrics output
sps_metrics <- data.frame(
	region = rep(regions, each = length(species) * 2),
	species = rep(rep(species, each = 2), length(regions)),
	type = rep(c("Spawners", "Run Size"), length(species) * length(regions)),
	current_status = NA,
	short_trend = NA,
	short_trend_cat = "", # cateogrical: stable, increasing, decreasing
	long_trend = NA,
	long_trend_cat = "",
	current_abundance = NA,
	current_abundance_year = NA,
	average_abundance = NA,
	previous_gen_abundance = NA,
	gen_length = NA
)

pdf(file = "output/ignore/figures/sps-metrics_detailed.pdf", width = 6, height = 4, pointsize = 10)
par(mar = c(3, 4, 2, 1))

for(r in 1:length(regions)){ # for each region
	for(s in 1:length(species)){ # for each species
		
		#----------
		# Check if there are any data	
		if(length(which(sps_dat$region == regions[r] & sps_dat$species == species[s])) == 0){ # If no data
			
			# sps_metrics[which(sps_metrics$region == regions[r] & sps_metrics$species == species[s]), c("n_indicator", "n_nonindicator")] <- 0
			
		#----------
		} else { # If there ARE data
			
			# 0. Index for relevant rows of sps_metrics and sps_dat:
			ind <- which(sps_metrics$region == regions[r] & sps_metrics$species == species[s])
			ind_dat <- which(sps_dat$region == regions[r] & sps_dat$species == species[s])
			
			# 1. Input generation length into metrics output
			g <- genLength$gen_length[which(genLength$region == regions[r] & genLength$species == species[s])]
			if(length(g) > 0){
				sps_metrics$gen_length[ind] <- g
			}
			
			# # Indicator and non-indicator streams (removed from output)
			# if(length(which(numStreams$region == regions[r] & numStreams$species == species[s])) > 0){
			# 	sps_metrics[ind[1], c("n_indicator", "n_nonindicator")] <- numStreams[which(numStreams$region == regions[r] & numStreams$species == species[s]), c("indicator", "nonindicator")]
			# }	
			
			#---------------------------------------------------------------------------
			# 2. Calculate metrics from data
			#---------------------------------------------------------------------------
			for(i in 1:2){ # For run size (aka total return) and current abundance
				
				# If there are data...
				if(sum(!is.na(sps_dat[ind_dat, c("smoothedSpawners", "smoothedRunsize")[i]])) > 0){
					
					# Extract abundance (y) and year (x)
					y <- sps_dat[ind_dat, c("smoothedSpawners", "smoothedRunsize")[i]]
					x <- sps_dat$year[ind_dat]
					
					# Input current abundance and year into metrics output
					sps_metrics$current_abundance[ind[i]] <- tail(y[!is.na(y)], 1)
					sps_metrics$current_abundance_year[ind[i]] <- tail(x[!is.na(y)], 1)
					
					# Historical (geometric mean) abundance
					H <-  exp(mean(log(y), na.rm = TRUE))
					sps_metrics$average_abundance[ind[i]] <- H
					
					# Previous generation abundance
					sps_metrics$previous_gen_abundance[ind[i]] <- y[which(x == (tail(x[!is.na(y)], 1) - g))]
					
					# Current status (current abundance as a percentage of historical average)
					sps_metrics$current_status[ind[i]] <- (sps_metrics$current_abundance[ind[i]] - H)/H
					
					#----------
					# Calculate short-term trend (three generations) as the average annual
					# percent change in abundance
					#----------
					x2 <- c((max(x[!is.na(y)]) - g*3 + 1):max(x[!is.na(y)]))
					y2 <- y[match(x2, x)]
					fit.short <- lm(log(y2) ~ x2)
					
					sps_metrics$short_trend[ind[i]] <- as.numeric(exp(fit.short$coefficients[2]) - 1)
					
					# Category of short trend: stable (non-signficiant slope) = arrows-left-right
					if(summary(fit.short)$coefficients[2, "Pr(>|t|)"] >= 0.05){ # If trend is not significant
						sps_metrics$short_trend_cat[ind[i]] <- "arrows-left-right"
					} else if(summary(fit.short)$coefficients[2, "Estimate"] < 0){
						sps_metrics$short_trend_cat[ind[i]] <- "arrow-down"
					} else if(summary(fit.short)$coefficients[2, "Estimate"] > 0){
						sps_metrics$short_trend_cat[ind[i]] <- "arrow-up"
					}
					
					# Predict spawners or total return and include in sps_dat
					y.pred_short <- predict(fit.short, se.fit = TRUE)
					sps_dat[ind_dat[match(x2[!is.na(y2)], sps_dat$year[ind_dat])], columns.short[i, 1]] <- exp(y.pred_short$fit) # Mean prediction
					sps_dat[ind_dat[match(x2[!is.na(y2)], sps_dat$year[ind_dat])], columns.short[i, 2]] <- exp(y.pred_short$fit - 1.96*y.pred_short$se.fit) # Lower 95% prediction interval
					sps_dat[ind_dat[match(x2[!is.na(y2)], sps_dat$year[ind_dat])], columns.short[i, 3]] <- exp(y.pred_short$fit + 1.96*y.pred_short$se.fit) # Upper 95% prediction interval
					
					#----------
					# Calculate long-term trend (all time) as the average annual
					# percent change in abundance
					#----------
					fit.long <- lm(log(y) ~ x)
					sps_metrics$long_trend[ind[i]] <- as.numeric(exp(fit.long$coefficients[2]) - 1)
					
					# Category of short trend: stable (non-signficiant slope) = arrows-left-right
					if(summary(fit.long)$coefficients[2, "Pr(>|t|)"] >= 0.05){ # If trend is not signficiant
						sps_metrics$long_trend_cat[ind[i]] <- "arrows-left-right"
					} else if(summary(fit.long)$coefficients[2, "Estimate"] < 0){
						sps_metrics$long_trend_cat[ind[i]] <- "arrow-down"
					} else if(summary(fit.long)$coefficients[2, "Estimate"] > 0){
						sps_metrics$long_trend_cat[ind[i]] <- "arrow-up"
					}
					
					# Predict spawners or total return and include in sps_dat
					y.pred_long <- predict(fit.long, se.fit = TRUE)
					sps_dat[ind_dat[match(x[!is.na(y)], sps_dat$year[ind_dat])], columns.long[i, 1]] <- exp(y.pred_long$fit)
					sps_dat[ind_dat[match(x[!is.na(y)], sps_dat$year[ind_dat])], columns.long[i, 2]] <- exp(y.pred_long$fit - 1.96*y.pred_long$se.fit)
					sps_dat[ind_dat[match(x[!is.na(y)], sps_dat$year[ind_dat])], columns.long[i, 3]] <- exp(y.pred_long$fit + 1.96*y.pred_long$se.fit)
					
					#-------------------------------------------------------------------------
					# Plot
					#-------------------------------------------------------------------------
					
					yraw <- sps_dat[ind_dat, c("spawners", "runsize")[i]]
					plot(x, yraw*10^-3, "l", col = SWP_cols['stone1'], xlab = "", ylab = paste(c("Spawners", "Run size")[i], "(thousands)"), bty = "l", las = 1, lty = 3)
					lines(x, y*10^-3, lwd = 1.5, col = SWP_cols['stone3'])
					
					abline(h = sps_metrics$average_abundance[ind[i]]*10^-3, lty = 2, col = SWP_cols['stone3'])
					
					points(sps_metrics$current_abundance_year[ind[i]], sps_metrics$current_abundance[ind[i]]*10^-3, pch = 19, col = SWP_cols['stone3'])
					points(sps_metrics$current_abundance_year[ind[i]] - g, sps_metrics$previous_gen_abundance[ind[i]]*10^-3, pch = 21, bg = "#FFFFFF", col = SWP_cols['stone3'], lwd = 1.5)
					
					polygon(x = c(x2[!is.na(y2)], rev(x2[!is.na(y2)])), 
									y = c(exp(y.pred_short$fit + 1.96*y.pred_short$se.fit), rev(exp(y.pred_short$fit - 1.96*y.pred_short$se.fit)))*10^-3,
									border = NA, col = paste0(SWP_cols['clay1'], 30))
					
					lines(x2[!is.na(y2)], exp(y.pred_short$fit)*10^-3, col = SWP_cols['clay1'], lwd = 2)
					
					text(x[1], 0.9*max(yraw, na.rm = TRUE)*10^-3, paste0("Short-term trend: ", round(sps_metrics$short_trend[ind[i]]*100, 1), "% (", sps_metrics$short_trend_cat[ind[i]], ")"), col = SWP_cols['clay1'], adj = 0, cex = 0.8)
					
				
					polygon(x = c(x[!is.na(y)], rev(x[!is.na(y)])), 
									y = c(exp(y.pred_long$fit + 1.96*y.pred_long$se.fit), rev(exp(y.pred_long$fit - 1.96*y.pred_long$se.fit)))*10^-3,
									border = NA, col = paste0(SWP_cols['tidal2'], 30))
					
					lines(x[!is.na(y)], exp(y.pred_long$fit)*10^-3, col = SWP_cols['tidal2'], lwd = 2)
					
					text(x[1], 0.95 * max(yraw, na.rm = TRUE)*10^-3, paste0("Long-term trend: ", round(sps_metrics$long_trend[ind[i]]*100, 1), "% (", sps_metrics$long_trend_cat[ind[i]], ")"), col = SWP_cols['tidal2'], adj = 0, cex = 0.8)
					
					text(x[1], max(yraw, na.rm = TRUE)*10^-3, paste0("Current status: ", round(sps_metrics$current_status[ind[i]]*100, 1), "%"), adj = 0, cex = 0.8, col = SWP_cols['stone3'])
					mtext(side= 3, paste(regions[r], species[s], c("spawners", "run size")[i]))
					
					legend("topright", lwd = c(1.5, 1), lty = c(1, 3), col = c(SWP_cols['stone3'], SWP_cols['stone1']), c("Smoothed", "Raw"), bty = "n", cex = 0.8)

					
				}} # end spawners/run size
		} #end if there ARE data
	} # end species
} # end region

dev.off()

###############################################################################
# Hard-coded checks 
###############################################################################

#------------------------------------------------------------------------------
# Is there a spawner abundance estimate in the most recent generation?
#------------------------------------------------------------------------------

sps_metrics[which(sps_metrics$current_abundance_year < max(sps_metrics$current_abundance_year, na.rm = TRUE) - sps_metrics$gen_length), ]

# Just Haida Gwaii Chinook; set to NA
sps_metrics[which(sps_metrics$current_abundance_year < max(sps_metrics$current_abundance_year, na.rm = TRUE) - sps_metrics$gen_length), c("current_status", "current_abundance", "previous_gen_abundance")] <- NA

# sps_metrics[which(sps_metrics$current_abundance_year < max(sps_metrics$current_abundance_year, na.rm = TRUE) - sps_metrics$gen_length), c("short_trend_cat", "long_trend_cat")] <- ""


#------------------------------------------------------------------------------
# Is current spawner abundance < 1000?
#------------------------------------------------------------------------------

# sps_metrics[which(sps_metrics$current < 1000), ]
# 
# # Create new variable to flag if abundance is below critical threshold of 1000 spawners
# sps_metrics$critical <- ifelse(sps_metrics$current < 1000, 1, 0)
# 

#------------------------------------------------------------------------------
# Are there at least 20 years of data to establish a meaningful baseline?
#------------------------------------------------------------------------------

# Which regions and species have <20 years of spawner data?
dum_sp <- sps_dat %>% select(region, species, year, smoothedSpawners) %>%
	filter(!is.na(smoothedSpawners)) %>%
	mutate(regionspecies = paste(region, species)) %>%
	group_by(regionspecies) %>%
	summarise(nyears = length(year), rangeyears = paste(min(year), max(year), sep = "-"))

dum_sp2 <- sps_dat %>% select(region, species, year, smoothedSpawners) %>%
	filter(!is.na(smoothedSpawners)) %>%
	mutate(regionspecies = paste(region, species)) %>%
	group_by(regionspecies) %>%
	summarise(region = unique(region), species = unique(species), nyears = length(year), minyear = min(year), maxyear = max(year))

# Which regions and species have <20 years of run size data?
dum_rs <- sps_dat %>% select(region, species, year, smoothedRunsize) %>%
	filter(!is.na(smoothedRunsize)) %>%
	mutate(regionspecies = paste(region, species)) %>%
	group_by(regionspecies) %>%
	summarise(nyears = length(year), rangeyears = paste(min(year), max(year), sep = "-")) %>% 
	mutate(regionspeciestype = paste(regionspecies, "Run Size"))

dum_rs2 <- sps_dat %>% select(region, species, year, smoothedRunsize) %>%
	filter(!is.na(smoothedRunsize)) %>%
	mutate(regionspecies = paste(region, species)) %>%
	group_by(regionspecies) %>%
	summarise(region = unique(region), species = unique(species), nyears = length(year), minyear = min(year), maxyear = max(year)) %>% 
	mutate(regionspeciestype = paste(regionspecies, "Run Size"))

# Add spawners number of years and range of years to sps_metrics
sps_metrics <- sps_metrics %>% 
	mutate(regionspeciestype = paste(region, species, type)) %>% 
	left_join(
		dum_sp %>% 
			mutate(regionspeciestype = paste(regionspecies, "Spawners")) %>%
			select(regionspeciestype, nyears, rangeyears)
	) 

# Add run size number of years and range of years to sps_metrics

sps_metrics[match(dum_rs$regionspeciestype, sps_metrics$regionspeciestype), c("nyears", "rangeyears")] <- dum_rs[, c("nyears", "rangeyears")]

# Remove dummy variable for matching
sps_metrics <- sps_metrics %>% select(-regionspeciestype)

# Change current status to Unknown for those
sps_metrics$current_status[which(sps_metrics$nyears < 20)] <- NA

###############################################################################
# Write output
###############################################################################

if(write.output){
	write.csv(sps_metrics, file = "output/sps-metrics.csv", row.names = FALSE)
	write.csv(sps_dat, "output/sps-data.csv", row.names = FALSE)
	
	write.csv(sps_metrics, file = paste0("output/archive/sps-metrics_", Sys.Date(), ".csv"), row.names = FALSE)
	write.csv(sps_dat, file = paste0("output/archive/sps-data_", Sys.Date(), ".csv"), row.names = FALSE)
}

###############################################################################
# Summary output for internal use, including total % change values
###############################################################################

#------------------------------------------------------------------------------
# Append years to sps_metrics_temp
#------------------------------------------------------------------------------

sps_metrics_temp <- sps_metrics %>% 
	mutate(regionspeciestype = paste(region, species, type)) %>% 
	left_join(
		dum_sp2 %>% 
			mutate(regionspeciestype = paste(regionspecies, "Spawners")) %>%
			select(regionspeciestype, nyears, minyear, maxyear)
	) 

# Add run size number of years and range of years to sps_metrics

sps_metrics_temp[match(dum_rs2$regionspeciestype, sps_metrics_temp$regionspeciestype), c("nyears", "minyear", "maxyear")] <- dum_rs2[, c("nyears", "minyear", "maxyear")]

sps_metrics_temp <- sps_metrics_temp %>% 
	mutate(rangeyears_length = maxyear - minyear + 1)

#------------------------------------------------------------------------------
# Create internal summary
#------------------------------------------------------------------------------

sps_summary_internal <- sps_metrics_temp %>%
	mutate(current_status = round(current_status*100)) %>%
	mutate(short_trend_per_yr = round(short_trend*100, 1)) %>%
	mutate(short_trend_total = round((exp(log(short_trend + 1) * (3 * gen_length - 1)) - 1)*100, 1)) %>%
	mutate(short_trend_years = paste(maxyear - 3 * gen_length + 1, maxyear, sep = "-")) %>%
	mutate(long_trend_per_year = round(long_trend*100, 1)) %>%
	mutate(long_trend_total = round((exp(log(long_trend + 1) * (rangeyears_length - 1)) - 1)*100, 1)) %>%
	mutate(long_trend_years = paste(minyear, maxyear, sep = "-")) %>%
	mutate(current_abundance = round(current_abundance)) %>%
	mutate(average_abundance = round(average_abundance)) %>%
	mutate(previous_gen_abundance = round(previous_gen_abundance)) %>%
	select(region, species, type, current_status, short_trend_per_yr, short_trend_total, short_trend_years, short_trend_cat, long_trend_per_year, long_trend_total, long_trend_years, long_trend_cat, current_abundance, current_abundance_year, average_abundance, previous_gen_abundance, gen_length, nyears, rangeyears) %>%
	filter(paste(region, species) %in% c("Yukon Pink", "Yukon Sockeye", "Yukon Steelhead", "Columbia Chum", "Columbia Coho", "Columbia Pink") == FALSE) # Filter out regions/species not known to exist

if(write.output){
	write.csv(sps_summary_internal, file = paste0("output/archive/sps-summary-internal_", Sys.Date(), ".csv"), row.names = FALSE)
	write.csv(sps_summary_internal, file = "output/sps-summary-internal.csv", row.names = FALSE)
}

###############################################################################
# Clean up data for plotting by Tactica
###############################################################################

#------------------------------------------------------------------------------
# Summary
#------------------------------------------------------------------------------
sps_summary <- sps_metrics %>%
	mutate(current_status = round(current_status*100)) %>%
	mutate(status_offset_x = 0) %>%
	mutate(status_offset_y = 0) %>%
	mutate(region_label_offset_x = 0) %>%
	mutate(region_label_offset_y = 0) %>%
	mutate(short_trend = round(short_trend*100, 1)) %>%
	mutate(long_trend = round(long_trend*100, 1)) %>%
	mutate(current_abundance = round(current_abundance)) %>%
	mutate(average_abundance = round(average_abundance)) %>%
	mutate(previous_gen_abundance = round(previous_gen_abundance)) %>%
	filter(paste(region, species) %in% c("Yukon Pink", "Yukon Sockeye", "Yukon Steelhead", "Columbia Chum", "Columbia Coho", "Columbia Pink") == FALSE) # Filter out regions/species not known to exist
	
# Change from Run Size to Total return [abundance]
sps_summary$type[sps_summary$type == "Run Size"] <- "Total return"

# Are there regions within a species that are within 5 percent
# Within spawners and run size, for each species, offset status for regions if needed
P <- 3 # threshold for y-axis offset
PL <- 5 # threshold for label offset
for(i in 1:2){ # for spawners and run type
	for(s in 1:6){ # for each species
		
		sps_summary.is <- sps_summary[which(sps_summary$type ==  c("Spawners", "Total return")[i] & sps_summary$species == species[s]), c("region", "current_status", "status_offset_x", "status_offset_y", "region_label_offset_y")]
		o <- order(sps_summary.is$current_status)
		sps_summary.is <- sps_summary.is[o,]

		# If zero diff, add in offset x
		if(length(which(diff(sps_summary.is$current_status) == 0)) > 0){
			
			ind <- which(diff(sps_summary.is$current_status) == 0)
			if(length(ind) > 1){
				stop("More than one region has the same value.")
			} else {
				regions.same <- sps_summary.is$region[c(ind, ind + 1)]
				sps_summary$status_offset_x[which(sps_summary$type == c("Spawners", "Total return")[i] & sps_summary$species == species[s] & sps_summary$region %in% regions.same)] <- c(-1, 1)
			}
		}
		
		# If within P percent, then offset y
		if(length(which(diff(sps_summary.is$current_status) < P & diff(sps_summary.is$current_status) > 0)) > 0){
			dum <- which(diff(sps_summary.is$current_status) < P & diff(sps_summary.is$current_status) > 0)
			while(length(dum) > 0){
				diff <- diff(sps_summary.is$current_status + sps_summary.is$status_offset_y)[dum]
				sps_summary.is[dum, "status_offset_y"] <- sps_summary.is[dum, "status_offset_y"] - ceiling((3-diff)/2)
				sps_summary.is[dum + 1, "status_offset_y"] <- sps_summary.is[dum + 1, "status_offset_y"] + ceiling((3-diff)/2)
				
				# Check order if preseved
				if(sum(order(sps_summary.is$current_status) - order(sps_summary.is$current_status + sps_summary.is$status_offset_y)) > 0){
					stop("Order is changed.")
				}
				dum <- which(diff(sps_summary.is$current_status + sps_summary.is$status_offset_y) < P & diff(sps_summary.is$current_status) > 0)
			}
			
			sps_summary$status_offset_y[which(sps_summary$type == c("Spawners", "Total return")[i] & sps_summary$species == species[s])] <- sps_summary.is$status_offset_y[match(sps_summary$region[which(sps_summary$type == c("Spawners", "Total return")[i] & sps_summary$species == species[s])], sps_summary.is$region)]
			
		}
		
		# If within PL percent, then offset y for label (lowering lower)
		if(length(which(diff(sps_summary.is$current_status + sps_summary.is$status_offset_y) < PL)) > 0){
			dum <- which(diff(sps_summary.is$current_status + sps_summary.is$status_offset_y) < PL)
			while(length(dum) > 0){
				# diff <- diff(sps_summary.is$current_status + sps_summary.is$status_offset_y)[dum]
				sps_summary.is[dum[1], "region_label_offset_y"] <- sps_summary.is[dum[1], "region_label_offset_y"] - 1
				
				# Check order if preseved
				if(sum(order(sps_summary.is$current_status) - order(sps_summary.is$current_status + sps_summary.is$status_offset_y + sps_summary.is$region_label_offset_y)) > 0){
					stop("Order is changed.")
				}
				dum <- which(diff(sps_summary.is$current_status + sps_summary.is$status_offset_y + sps_summary.is$region_label_offset_y) < PL)
			}
			
			sps_summary$region_label_offset_y[which(sps_summary$type == c("Spawners", "Total return")[i] & sps_summary$species == species[s])] <- sps_summary.is$region_label_offset_y[match(sps_summary$region[which(sps_summary$type == c("Spawners", "Total return")[i] & sps_summary$species == species[s])], sps_summary.is$region)]
			
		}
	
		
		plot(rep(1,dim(sps_summary.is)[1]), sps_summary.is$current_status, pch = 19, cex = 1.5, col = rainbow(9), xlim = c(0.5, 3.5), main = paste(species[s], c("Spawners", "Total return")[i]))
		points(rep(1.2,dim(sps_summary.is)[1]) + sps_summary.is$status_offset_x/25,  sps_summary.is$current_status + sps_summary.is$status_offset_y, pch = 19, cex = 1.5, col = rainbow(9))
		points(rep(1.2,dim(sps_summary.is)[1])+ sps_summary.is$status_offset_x/25, sps_summary.is$current_status + sps_summary.is$status_offset_y, pch = 19, cex = 0.5)
		segments(x0 = 1.2 + sps_summary.is$status_offset_x/25, x1 = 1.7, y0 = sps_summary.is$current_status + sps_summary.is$status_offset_y, y1 = sps_summary.is$current_status + sps_summary.is$status_offset_y + sps_summary.is$region_label_offset_y)
		text(1.7, sps_summary.is$current_status + sps_summary.is$status_offset_y + sps_summary.is$region_label_offset_y, sps_summary.is$region, adj = 0)
		
		} # end species
	}

# Change region name
sps_summary$region[sps_summary$region == "Transboundary"] <- "Northern Transboundary"

if(write.output){
	write.csv(sps_summary, file = paste0("output/archive/sps-summary_", Sys.Date(), ".csv"), row.names = FALSE)
	write.csv(sps_summary, file = "output/sps-summary.csv", row.names = FALSE)
}

#------------------------------------------------------------------------------
# Region profile
#------------------------------------------------------------------------------

# Function to add commas if >= 10,000
prettierNum <- function(x){
	x1 <- character(length(x))
	for(i in 1:length(x)){
		if(is.na(x[i])){
			x1[i] <- "?"
		} else {
			if(x[i] >= 1000){
				x1[i] <- prettyNum(x[i], big.mark=",", preserve.width="none")
			} else {
				x1[i] <- as.character(x[i])
			}
		}}
	return(x1)
}


makePrettyNums <- function(x){
	x1 <- rep(NA, length(x))
	
	for(i in 1:length(x)){
	
	if(!is.na(x[i])){
		if(x[i] < 100){
		x1[i] <- round(x[i])
	} else if(x[i] < 1000){ # ROund to nearest 10
		x1[i] <- round(x[i]*10^-1)*10^1
	} else { # Round to nearest 100
		x1[i] <- round(x[i]*10^-2)*10^2
	}
	}
	}
	
	x2 <- prettierNum(x1)
	
	return(x2)
}

sps_profile <- sps_metrics %>%
	filter(type == "Spawners") %>%
	mutate(currentyears = paste(current_abundance_year - gen_length + 1, current_abundance_year, sep = "-")) %>%
	select(region, species, current_abundance, currentyears, average_abundance, rangeyears) %>%
	mutate(current_abundance = makePrettyNums(current_abundance)) %>%
	mutate(average_abundance = makePrettyNums(average_abundance)) %>%
	rename(spawner_current_years = "currentyears",  spawner_current_abundance = "current_abundance", spawner_average_abundance = "average_abundance", spawner_average_years = "rangeyears") %>%
	# Add total abundance
	left_join(sps_metrics %>%
	filter(type == "Run Size") %>%
		mutate(currentyears = paste(current_abundance_year - gen_length + 1, current_abundance_year, sep = "-")) %>%
		select(region, species, current_abundance, currentyears, average_abundance, rangeyears) %>%
		mutate(current_abundance = makePrettyNums(current_abundance)) %>%
		mutate(average_abundance = makePrettyNums(average_abundance)) %>%
		rename(total_current_years = "currentyears", total_current_abundance = "current_abundance", total_average_abundance = "average_abundance", total_average_years = "rangeyears")) %>%
	# Filter out regions/species not known to exist
	filter(paste(region, species) %in% c("Yukon Pink", "Yukon Sockeye", "Yukon Steelhead", "Columbia Chum", "Columbia Coho", "Columbia Pink") == FALSE) 

# Change years to blank
sps_profile[is.na(sps_profile$spawner_average_years), c("spawner_current_years", "spawner_average_years")] <- ""
sps_profile[is.na(sps_profile$total_average_years), c("total_current_years", "total_average_years")] <- ""

# Change region name
sps_profile$region[sps_profile$region == "Transboundary"] <- "Northern Transboundary"

if(write.output){
	write.csv(sps_profile, file = paste0("output/archive/sps_profile_", Sys.Date(), ".csv"), row.names = FALSE)
	write.csv(sps_profile, file = "output/sps-profile.csv", row.names = FALSE)
}

#------------------------------------------------------------------------------
# Trends
#------------------------------------------------------------------------------

trends_plotting <- sps_dat %>%
	select(region, species, year) %>%
	mutate(spawners = NA, 
				 spawners_short_trend = NA,
				 spawners_long_trend = NA,
				 total_return = NA,
				 total_return_short_trend = NA,
				 total_return_long_trend = NA)

# Loop through each species and region and normalize fields to yield % of historical baseline
for(r in 1:length(regions)){ # for each region
	for(s in 1:length(species)){ # for each species
		
		ind <- which(trends_plotting$region == regions[r] & trends_plotting$species == species[s])
		
		for(i in 1:2){ # for spawners and total return (aka run size)
			# Extract historical baseline
			H <- sps_metrics$average_abundance[which(sps_metrics$region == regions[r] & sps_metrics$species == species[s] & sps_metrics$type == c("Spawners", "Run Size")[i])]
			
			if(!is.na(H)){ # If there is a baseline
				
				var1 <- names(trends_plotting)[grep(c("spawners", "total_return")[i], names(trends_plotting))]
				var2 <- c(c("smoothedSpawners", "smoothedRunsize")[i], paste0(c("spawners", "runsize")[i], "_short_trend"), paste0(c("spawners", "runsize")[i], "_long_trend"))
				
				trends_plotting[ind, var1] <- round((sps_dat[ind, var2] - H)/H * 100, 1)
				
				rm(var1, var2, H)
			} 
		} # end i
	} # end s
} # end r

# Change region name
trends_plotting$region[trends_plotting$region == "Transboundary"] <- "Northern Transboundary"

if(write.output){
	write.csv(trends_plotting, file = paste0("output/archive/sps-trends_plotting_", Sys.Date(), ".csv"), row.names = FALSE)
	write.csv(trends_plotting, file = "output/sps-trends_plotting.csv", row.names = FALSE)
}
