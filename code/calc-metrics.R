###############################################################################
# Calculate metrics from sps-data.csv
# called on in docs/methods-results.Rmd 
# Author: Steph Peacock
# Date: Oct 29, 2023

###############################################################################

# Read in data 

# Compiled spawner and run size
sps_dat <- read.csv("output/sps-data.csv")

# Generation length
genLength <-read.csv("data/gen_length_regions.csv")

# Number of streams
numStreams <- read.csv("output/numStreams_all-regions.csv")

# Define variables
regions <- unique(unique(sps_dat$region))
species <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")

# Source script with code for plotting functions
source("code/functions.R")

###############################################################################
# Create metrics for each region and species
###############################################################################

sps_metrics <- data.frame(
	region = rep(regions, each = length(species) * 2),
	species = rep(rep(species, each = 2), length(regions)),
	type = rep(c("Spawners", "Run Size"), length(species) * length(regions)),
	status = NA,
	short_trend = NA,
	short_trend_cat = "", # cateogrical: stable, increasing, decreasing
	long_trend = NA,
	long_trend_cat = "",
	current = NA,
	current_year = NA,
	hist = NA,
	prevGen = NA,
	n_indicator = NA,
	n_nonindicator = NA,
	generation_length = NA
)

pdf(file = "output/ignore/figures/sps-metrics_detailed.pdf", width = 6, height = 4, pointsize = 10)
par(mar = c(3, 4, 2, 1))

for(r in 1:length(regions)){
	for(s in 1:length(species)){
		
		# Check if there are any data	
		if(length(which(sps_dat$region == regions[r] & sps_dat$species == species[s])) == 0 & length(which(numStreams$region == regions[r] & numStreams$species == species[s])) == 0){
			sps_metrics[which(sps_metrics$region == regions[r] & sps_metrics$species == species[s]), c("n_indicator", "n_nonindicator")] <- 0
		
			
		} else { # If there ARE data
			
			# Index for relevant rows of sps_metrics:
			ind <- which(sps_metrics$region == regions[r] & sps_metrics$species == species[s])
			ind_dat <- which(sps_dat$region == regions[r] & sps_dat$species == species[s])
			
			# Generation length
			g <- genLength$gen_length[which(genLength$region == regions[r] & genLength$species == species[s])]
			if(length(g) > 0){
				sps_metrics$generation_length[ind] <- g
			}
			
			# Indicator and non-indicator streams
			if(length(which(numStreams$region == regions[r] & numStreams$species == species[s])) > 0){
				sps_metrics[ind[1], c("n_indicator", "n_nonindicator")] <- numStreams[which(numStreams$region == regions[r] & numStreams$species == species[s]), c("indicator", "nonindicator")]
			}	
			
			for(i in 1:2){ # For run size and current abundance
				# If there are data...
				if(sum(!is.na(sps_dat[ind_dat, c("smoothedSpawners", "smoothedRunsize")[i]])) > 0){
					
					y <- sps_dat[ind_dat, c("smoothedSpawners", "smoothedRunsize")[i]]
					x <- sps_dat$year[ind_dat]
					
					# Fill in current abundance and year
					sps_metrics$current[ind[i]] <- tail(y[!is.na(y)], 1)
					sps_metrics$current_year[ind[i]] <- tail(x[!is.na(y)], 1)
					
					# Historical 
					sps_metrics$hist[ind[i]] <- mean(y, na.rm = TRUE)
					
					# Previous generation
					sps_metrics$prevGen[ind[i]] <- y[which(x == (tail(x[!is.na(y)], 1) - g))]
					
					# Status
					sps_metrics$status[ind[i]] <- (sps_metrics$current[ind[i]] - sps_metrics$hist[ind[i]])/sps_metrics$hist[ind[i]]
					
					# Short trend: 3 gen
					# if(species[s] == "Pink"){
					# 	x2 <- tail(x[!is.na(y)], g*2)
					# 	fit.short <- lm(log(tail(y[!is.na(y)], g*3) + 10e-10) ~ tail(x[!is.na(y)], g*3))
					# 
					# } else {
						x2 <- tail(x[!is.na(y)], g*3 - 1)
						fit.short <- lm(log(tail(y[!is.na(y)], g*3 - 1) + 10e-10) ~ x2)
					# }
					sps_metrics$short_trend[ind[i]] <- as.numeric(exp(fit.short$coefficients[2]) - 1)
					
					# Cateogry of short trend
					if(summary(fit.short)$coefficients[2, "Pr(>|t|)"] >= 0.05){ # If trend is not signficiant
						sps_metrics$short_trend_cat[ind[i]] <- "arrows-left-right"
					} else if(summary(fit.short)$coefficients[2, "Estimate"] < 0){
						sps_metrics$short_trend_cat[ind[i]] <- "arrow-down"
					} else if(summary(fit.short)$coefficients[2, "Estimate"] > 0){
						sps_metrics$short_trend_cat[ind[i]] <- "arrow-up"
					}
					
					# Long trend
					fit.long <- lm(log(y[!is.na(y)] + 10e-10) ~ x[!is.na(y)])
					sps_metrics$long_trend[ind[i]] <- as.numeric(exp(fit.long$coefficients[2]) - 1)
					
					# Cateogry of long trend
					if(summary(fit.long)$coefficients[2, "Pr(>|t|)"] >= 0.05){ # If trend is not signficiant
						sps_metrics$long_trend_cat[ind[i]] <- "arrows-left-right"
					} else if(summary(fit.long)$coefficients[2, "Estimate"] < 0){
						sps_metrics$long_trend_cat[ind[i]] <- "arrow-down"
					} else if(summary(fit.long)$coefficients[2, "Estimate"] > 0){
						sps_metrics$long_trend_cat[ind[i]] <- "arrow-up"
					}
					
					#Plot
					# if(3 == 2){
					yraw <- sps_dat[ind_dat, c("spawners", "runsize")[i]]
						plot(x, yraw, "l", col = grey(0.7), xlab = "", ylab = c("spawners", "runsize")[i], bty = "l")
						lines(x, y, lwd = 1.5)
						abline(h = sps_metrics$hist[ind[i]], lty = 2)
						points(sps_metrics$current_year[ind[i]], sps_metrics$current[ind[i]], pch = 19)
						points(sps_metrics$current_year[ind[i]] - g, sps_metrics$prevGen[ind[i]], pch = 21, bg = "#FFFFFF")
						y.short <- predict(fit.short, se.fit = TRUE)
						polygon(x = c(x2, rev(x2)), 
										y = c(exp(y.short$fit + 1.96*y.short$se.fit), rev(exp(y.short$fit - 1.96*y.short$se.fit))),
										border = NA, col = "#0000FF30")
						lines(x2, exp(y.short$fit), col = "#0000FF")
						text(x[1], 0.9*max(yraw, na.rm = TRUE), paste("Metric 3:", round(sps_metrics$short_trend[ind[i]], 2), sps_metrics$short_trend_cat[ind[i]]), col = "#0000FF", adj = 0, cex = 0.8)
						
						y.long <- predict(fit.long, se.fit = TRUE)
						polygon(x = c(x[!is.na(y)], rev(x[!is.na(y)])), 
										y = c(exp(y.long$fit + 1.96*y.long$se.fit), rev(exp(y.long$fit - 1.96*y.long$se.fit))),
										border = NA, col = "#FF000030")
						lines(x[!is.na(y)], exp(y.long$fit), col = "#FF0000")
						text(x[1], 0.95 * max(yraw, na.rm = TRUE), paste("Metric 2:", round(sps_metrics$long_trend[ind[i]], 2), sps_metrics$long_trend_cat[ind[i]]), col = "#FF0000", adj = 0, cex = 0.8)
						text(x[1], max(yraw, na.rm = TRUE), paste("Metric 1:", round(sps_metrics$status[ind[i]], 2)), adj = 0, cex = 0.8)
						mtext(side= 3, paste(regions[r], species[s], c("spawners", "runsize")[i]))
						
						legend("topright", lwd = c(1.5, 1), col = c(1, grey(0.70)), c("Smoothed", "Raw"), bty = "n", cex = 0.8)
					# }
					
				}} # end spawners/run size
		} #end if there ARE data
	} # end species
} # end region

dev.off()

write.csv(sps_metrics, file = "output/sps-metrics.csv")
		