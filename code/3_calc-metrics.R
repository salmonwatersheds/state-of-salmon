###############################################################################
# Calculate metrics from sps-data.csv
# called on in docs/methods-results.Rmd 
# Author: Steph Peacock
# Date: Oct 29, 2023

###############################################################################

# Read in data 

# Compiled spawner and run size
sps_dat <- read.csv("output/sps-data-raw.csv")

# Generation length
genLength <-read.csv("data/gen_length_regions.csv")

# Number of streams
numStreams <- read.csv("output/num-surveys/numStreams_all-regions.csv")

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
sps_dat$short_trend <- NA
sps_dat$short_trend_lwr <- NA
sps_dat$short_trend_upr <- NA
sps_dat$long_trend <- NA
sps_dat$long_trend_lwr <- NA
sps_dat$long_trend_upr <- NA

# Set up empty data frame to store metrics output
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
		if(length(which(sps_dat$region == regions[r] & sps_dat$species == species[s])) == 0 & length(which(numStreams$region == regions[r] & numStreams$species == species[s])) == 0){ # If no data
			
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
					
					# Extract abundance (y) and year (x)
					y <- sps_dat[ind_dat, c("smoothedSpawners", "smoothedRunsize")[i]]
					x <- sps_dat$year[ind_dat]
					
					# Fill in current abundance and year
					sps_metrics$current[ind[i]] <- tail(y[!is.na(y)], 1)
					sps_metrics$current_year[ind[i]] <- tail(x[!is.na(y)], 1)
					
					# Historical 
					sps_metrics$hist[ind[i]] <- exp(mean(log(y), na.rm = TRUE))
					
					# Previous generation
					sps_metrics$prevGen[ind[i]] <- y[which(x == (tail(x[!is.na(y)], 1) - g))]
					
					# Status
					sps_metrics$status[ind[i]] <- (sps_metrics$current[ind[i]] - sps_metrics$hist[ind[i]])/sps_metrics$hist[ind[i]]
					
					# Short trend: 3 generations
					# if(species[s] == "Pink"){
					# 	x2 <- tail(x[!is.na(y)], g*2)
					# 	fit.short <- lm(log(tail(y[!is.na(y)], g*3) + 10e-10) ~ tail(x[!is.na(y)], g*3))
					# 
					# } else {
						x2 <- tail(x[!is.na(y)], g*3 - 1)
						fit.short <- lm(log(tail(y[!is.na(y)], g*3 - 1)) ~ x2)
					# }
					sps_metrics$short_trend[ind[i]] <- as.numeric(exp(fit.short$coefficients[2]) - 1)
					
					# Put in prediction (just for spawners)
					if(i == 1){
					y.pred_short <- predict(fit.short, se.fit = TRUE)
					sps_dat$short_trend[tail(ind_dat, g*3 - 1)] <- exp(y.pred_short$fit)
					sps_dat$short_trend_lwr[tail(ind_dat, g*3 - 1)] <- exp(y.pred_short$fit - 1.96*y.pred_short$se.fit)
					sps_dat$short_trend_upr[tail(ind_dat, g*3 - 1)] <- exp(y.pred_short$fit + 1.96*y.pred_short$se.fit)
					}
					
					# plot(x, y, "o")
					# lines(x, sps_dat$short_trend[ind_dat], lwd = 2)
					# lines(x, sps_dat$short_trend_lwr[ind_dat], col = grey(0.8))
					# lines(x, sps_dat$short_trend_upr[ind_dat], col = grey(0.8))

					# Cateogry of short trend
					if(summary(fit.short)$coefficients[2, "Pr(>|t|)"] >= 0.05){ # If trend is not signficiant
						sps_metrics$short_trend_cat[ind[i]] <- "arrows-left-right"
					} else if(summary(fit.short)$coefficients[2, "Estimate"] < 0){
						sps_metrics$short_trend_cat[ind[i]] <- "arrow-down"
					} else if(summary(fit.short)$coefficients[2, "Estimate"] > 0){
						sps_metrics$short_trend_cat[ind[i]] <- "arrow-up"
					}
					
					# Long trend
					fit.long <- lm(log(y) ~ x)
					sps_metrics$long_trend[ind[i]] <- as.numeric(exp(fit.long$coefficients[2]) - 1)
					
					# Put in prediction (just for spawners)
					if(i == 1){
						y.pred_long <- predict(fit.long, se.fit = TRUE)
					# if(length(y.pred_long$fit) != length(ind_dat)){ stop("Length mismatch.")}
					sps_dat$long_trend[ind_dat[match(x[!is.na(y)], sps_dat$year[ind_dat])]] <- exp(y.pred_long$fit)
					sps_dat$long_trend_lwr[ind_dat[match(x[!is.na(y)], sps_dat$year[ind_dat])]] <- exp(y.pred_long$fit - 1.96*y.pred_long$se.fit)
					sps_dat$long_trend_upr[ind_dat[match(x[!is.na(y)], sps_dat$year[ind_dat])]] <- exp(y.pred_long$fit + 1.96*y.pred_long$se.fit)
					}
					
					# Category of long trend
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
						plot(x, yraw*10^-3, "l", col = SWP_cols['stone1'], xlab = "", ylab = paste(c("Spawners", "Run size")[i], "(thousands)"), bty = "l", las = 1, lty = 3)
						lines(x, y*10^-3, lwd = 1.5, col = SWP_cols['stone3'])
						abline(h = sps_metrics$hist[ind[i]]*10^-3, lty = 2, col = SWP_cols['stone3'])
						
						points(sps_metrics$current_year[ind[i]], sps_metrics$current[ind[i]]*10^-3, pch = 19, col = SWP_cols['stone3'])
						points(sps_metrics$current_year[ind[i]] - g, sps_metrics$prevGen[ind[i]]*10^-3, pch = 21, bg = "#FFFFFF", col = SWP_cols['stone3'], lwd = 1.5)
						y.short <- predict(fit.short, se.fit = TRUE)
						polygon(x = c(x2, rev(x2)), 
										y = c(exp(y.short$fit + 1.96*y.short$se.fit), rev(exp(y.short$fit - 1.96*y.short$se.fit)))*10^-3,
										border = NA, col = paste0(SWP_cols['clay1'], 30))
						lines(x2, exp(y.short$fit)*10^-3, col = SWP_cols['clay1'], lwd = 2)
						text(x[1], 0.9*max(yraw, na.rm = TRUE)*10^-3, paste("Short-term trend:", round(sps_metrics$short_trend[ind[i]], 2), sps_metrics$short_trend_cat[ind[i]]), col = SWP_cols['clay1'], adj = 0, cex = 0.8)
						
						y.long <- predict(fit.long, se.fit = TRUE)
						polygon(x = c(x[!is.na(y)], rev(x[!is.na(y)])), 
										y = c(exp(y.long$fit + 1.96*y.long$se.fit), rev(exp(y.long$fit - 1.96*y.long$se.fit)))*10^-3,
										border = NA, col = paste0(SWP_cols['tidal2'], 30))
						lines(x[!is.na(y)], exp(y.long$fit)*10^-3, col = SWP_cols['tidal2'], lwd = 2)
						text(x[1], 0.95 * max(yraw, na.rm = TRUE)*10^-3, paste("Long-term trend:", round(sps_metrics$long_trend[ind[i]], 2), sps_metrics$long_trend_cat[ind[i]]), col = SWP_cols['tidal2'], adj = 0, cex = 0.8)
						text(x[1], max(yraw, na.rm = TRUE)*10^-3, paste("Current status:", round(sps_metrics$status[ind[i]], 2)), adj = 0, cex = 0.8, col = SWP_cols['stone3'])
						mtext(side= 3, paste(regions[r], species[s], c("spawners", "run size")[i]))
						
						legend("topright", lwd = c(1.5, 1), lty = c(1, 3), col = c(SWP_cols['stone3'], SWP_cols['stone1']), c("Smoothed", "Raw"), bty = "n", cex = 0.8)
					# }
					
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

sps_metrics[which(sps_metrics$current_year < max(sps_metrics$current_year, na.rm = TRUE) - sps_metrics$generation_length), ]

# Just Haida Gwaii Chinook; set to NA
sps_metrics[which(sps_metrics$current_year < max(sps_metrics$current_year, na.rm = TRUE) - sps_metrics$generation_length), c("status", "short_trend", "long_trend", "current", "prevGen")] <- NA

sps_metrics[which(sps_metrics$current_year < max(sps_metrics$current_year, na.rm = TRUE) - sps_metrics$generation_length), c("short_trend_cat", "long_trend_cat")] <- ""


#------------------------------------------------------------------------------
# Is current spawner abundance < 1000?
#------------------------------------------------------------------------------

# sps_metrics[which(sps_metrics$current < 1000), ]
# 
# # Create new variable to flag if abundance is below critical threshold of 1000 spawners
# sps_metrics$critical <- ifelse(sps_metrics$current < 1000, 1, 0)
# 
# # How many have very few data?
dum_sp <- sps_dat %>% select(region, species, year, smoothedSpawners) %>%
	mutate(regionspecies = paste(region, species)) %>%
	group_by(regionspecies) %>%
	summarise(nyears = length(year), rangeyears = paste(min(year), max(year), sep = "-"))
dum_sp[which(dum_sp$nyears < 20),]

dum_rs <- sps_dat %>% select(region, species, year, smoothedRunsize) %>%
	filter(!is.na(smoothedRunsize)) %>%
	mutate(regionspecies = paste(region, species)) %>%
	group_by(regionspecies) %>%
	summarise(nyears = length(year), rangeyears = paste(min(year), max(year), sep = "-")) %>% 
	mutate(regionspeciestype = paste(regionspecies, "Run Size"))

sps_metrics <- sps_metrics %>% 
	mutate(regionspeciestype = paste(region, species, type)) %>% 
	left_join(
		dum_sp %>% 
			mutate(regionspeciestype = paste(regionspecies, "Spawners")) %>%
			select(regionspeciestype, nyears, rangeyears)
	) 

sps_metrics[match(dum_rs$regionspeciestype, sps_metrics$regionspeciestype), c("nyears", "rangeyears")] <- dum_rs[, c("nyears", "rangeyears")]

sps_metrics <- sps_metrics %>% select(-regionspeciestype)

# Change current status to Unknown for those
sps_metrics$status[which(sps_metrics$nyears < 20)] <- NA
###############################################################################
# Write output
###############################################################################

write.csv(sps_metrics, file = "output/sps-metrics.csv")
write.csv(sps_dat, "output/sps-data.csv")
