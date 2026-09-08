################################################################################
# Explore SoS 2026 output for Key Findings
# Author: Steph Peacock
# Date: Sept 4, 2026
###############################################################################

library(dplyr)

# Load functions
source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/functions_general.R")
# Get dropbox directory
Dropbox_dir <- paste0(get_XDrive(), "1_PROJECTS/1_Active/State of Salmon/2_Data & Analysis/state-of-salmon")

# Compiled spawner and run size
sps_dat <- read.csv("output/sps-data.csv")
trends_plotting <- read.csv("output/sps-trends_plotting.csv")
sps_summary <- read.csv("output/sps-summary.csv")

###############################################################################
# Key statistic
###############################################################################

sps_summary %>% 
	group_by(type) %>%
	summarise(n_regionalpops = sum(!is.na(current_status)),
						n_below_average = sum(current_status < 0, na.rm = TRUE),
						ppn_below_average = sum(current_status < 0, na.rm = TRUE)/sum(!is.na(current_status)),
						n_declining = sum(short_trend_cat == "arrow-down"),
						ppn_declining = sum(short_trend_cat == "arrow-down")/sum(!is.na(current_status)),
						ppn_stable = sum(short_trend_cat == "arrows-left-right")/sum(!is.na(current_status)),
						ppn_inc = sum(short_trend_cat == "arrow-up")/sum(!is.na(current_status))
						)

###############################################################################
# Plots by species
###############################################################################
species_vec <- unique(sps_summary$species) 
regions <- unique(sps_summary$region)

region.cols <- PNWColors::pnw_palette("Bay", n = 10)
names(region.cols) <- regions

region_abbr <- c("YU", "TB", "HG", "NA", "SK", "CC", "EI", "WI", "FR", "CO")

yrs <- sort(unique(sps_dat$year))

quartz()
par(mfrow = c(5,2), mar = c(3,3,1,1),oma = c(1,1,1,0))

for(s in 1:6){
	
	for(j in 1:10){
		plot(range(yrs), c(-100, 150), "n", bty = "l", las = 1, xlab = "", ylab = "% anomaly", xlim = c(2000, 2025))
		mtext(side = 3, line = 0.5, regions[j], col = region.cols[j])
		abline(h = 0, lty =2)
		
		abline(v = seq(1950, 2025, 5), col = grey(0.8), lwd = 0.5)
		
		for(i in 1:length(region.cols)){
			sps_dat.si <- sps_dat %>% filter(species == species_vec[s], region == regions[i])
			trends_plotting.si <- trends_plotting %>% filter(species == species_vec[s], region == regions[i])
			sps_summary.si <- sps_summary %>% filter(species == species_vec[s], region == regions[i], type == "Spawners")
			
			if(nrow(sps_dat.si) > 0){
				points(sps_dat.si$year, (sps_dat.si$spawners- sps_summary.si$average_abundance)/sps_summary.si$average_abundance*100, col = paste0(region.cols[i], 30), pch = 19, cex = 0.8)# lwd = 0.8, )
				lines(trends_plotting.si$year, trends_plotting.si$spawners, lwd = 1.2, col = paste0(region.cols[i], 30))
				text(2025, tail(trends_plotting.si$spawners,1), region_abbr[i], col = paste0(region.cols[i], 30), pos = 4, xpd = NA)
			}
		} # end i
		
		sps_dat.sj <- sps_dat %>% filter(species == species_vec[s], region == regions[j])
		trends_plotting.sj <- trends_plotting %>% filter(species == species_vec[s], region == regions[j])
		sps_summary.sj <- sps_summary %>% filter(species == species_vec[s], region == regions[j], type == "Spawners")
		
		if(nrow(sps_dat.sj) > 0){
			points(sps_dat.sj$year, (sps_dat.sj$spawners- sps_summary.sj$average_abundance)/sps_summary.sj$average_abundance*100, "o", col = region.cols[j], pch = 19, lwd = 0.5)
			lines(trends_plotting.sj$year, trends_plotting.sj$spawners, lwd = 1.5, col = region.cols[j])
			text(2025, tail(trends_plotting.sj$spawners,1), region_abbr[j], col = region.cols[j], pos = 4, xpd = NA)
			lines(trends_plotting.sj$year, trends_plotting.sj$spawners_short_trend, lwd = 3, col = region.cols[j])
		}
		
	}# end j
}# end s
	
	
}
