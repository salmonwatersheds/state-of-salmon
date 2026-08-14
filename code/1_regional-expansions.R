###############################################################################
# Regional expansions of spawner abundance
# Drawing directly from database for spawner surveys
# June 26, 2023
# Steph Peacock (speacock@psf.ca)
###############################################################################

# Note: This differs from the code in archive/expanding-NuSEDS.R because here
# I draw on spawner survey data from the SWP Database. This better reflects 
# info currently in the PSE.

library(dplyr)
library(sf)
library(PNWColors)

# Source functions to apply expansion factors based on indicator/non-indicator
# streams (English et al. 2018; code provided by LGL Ltd.)
source("code/expansion-functions.R")

source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/functions_general.R")
X_Drive <- get_XDrive()

###############################################################################
# Import spawner survey data
###############################################################################

# # Read in all spawner survey data
# See 0_spawner-survey-additions.R for changes made to what's in the PSE, 
# mostly related to indicator/non-indicator designations and adding preliminary
# 2025 data for WVI

spawner_surveys.all <- read.csv("data/spawner_surveys_revised_2026-08-14.csv")

#------------------------------------------------------------------------------
# Define variables
#------------------------------------------------------------------------------

# Arrange regions from north to south
# Note: VIMI split not yet reflected in spawner survey output; deal with this lower
# down when assigning regions based on location of survey
unique(spawner_surveys.all$region)

regions <- c("Yukon", "Northern Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "West Vancouver Island", "East Vancouver Island & Mainland Inlets", "Fraser", "Columbia")
spawner_surveys.all$region <- factor(spawner_surveys.all$region, levels = regions)

# Arrange species (already done in new output)
unique(spawner_surveys.all$species_name)

species_all <- unique(spawner_surveys.all$species_name)

#------------------------------------------------------------------------------
# Read in spatial boundaries of PSE regions for subsetting streams
#------------------------------------------------------------------------------
# Note: the region field in the spawner_surveys.all data can only assume one value for CU,
# resulting in some streams from larger (pink) CUs being erroneously assigned to 
# Nass or Skeena (for example).

library(sf)
library(PNWColors)

# 2025 boundaries split EVIMI and WVI
pse_regions <- readRDS(paste0(get_XDrive(), "1_PROJECTS/1_Active/State of Salmon/2_Data & Analysis/state-of-salmon/data/ignore/pse-regions-2025/pse-regions-2025.rds"))
plot(st_geometry(pse_regions), col = pnw_palette("Sunset2", n = 10)[c(1,6,2,7,3,8,4,9,5,10)])

# Make spatial object for streams
# Any missing coordinates?
streamid <- unique(spawner_surveys.all$streamid)
stream_points <- data.frame(
	streamid = streamid,
	lon = spawner_surveys.all$longitude[match(streamid, spawner_surveys.all$streamid)], 
	lat = spawner_surveys.all$latitude[match(streamid, spawner_surveys.all$streamid)]) %>%
	st_as_sf(coords = c("lon", "lat"), crs = 4269)

###############################################################################
# Select region
###############################################################################
regionnames <- c("Haida Gwaii", "Nass", "Skeena", "Central Coast", "East Vancouver Island & Mainland Inlets", "West Vancouver Island", "Fraser") # Only include regions for which we want to do expansions

for(R in c(1:length(regionnames))){
	
	r <- regionnames[R] 
	
	# Subset region of interest
	pse_region <- pse_regions[which(pse_regions$regionname == r), ]
	# May need to make valid
	pse_region <- st_make_valid(pse_region)
	
	# buffer out to ensure all points are included
	pse_region_buffered <- st_buffer(pse_region, dist = 100)
	
	# Process spatial points
	intrscts <- st_intersects(stream_points, pse_region_buffered, sparse = FALSE)
	incl <- which(c(intrscts) == TRUE)
	
	# Plot region boundaries and streams
	plot(st_geometry(pse_region), main = r)
	plot(st_geometry(pse_region_buffered), add = TRUE, border = 4)
	plot(st_geometry(stream_points[incl,]), pch = 1, col = 3, cex = 0.8, add = TRUE)
	plot(st_geometry(stream_points[-incl,]), pch = 1, col = 2, cex = 0.8, add = TRUE)
	
	spawner_surveys <- spawner_surveys.all[which(spawner_surveys.all$streamid %in% streamid[incl]), ]
	
	
	# Not all species are necessarily present in each region
	# E.g., no sockeye or pink in Yukon
	# Create vector of species that are present in the selected region:
	species_vec <- sort(unique(spawner_surveys$species_name))
	n.species <- length(species_vec)
	
	# Extract years (note: include continuous vector of years even if there are no
	# data for a certain year; this is required by the expansion functions)
	yrs <- c(min(spawner_surveys$year):max(spawner_surveys$year))
	n.yrs <- length(yrs)
	
	# Extract all stream names for the given region
	streams <- sort(unique(spawner_surveys$stream_name_pse))
	n.streams <- length(streams)
	
	###############################################################################
	# Expand to regional scale
	###############################################################################
	
	#------------------------------------------------------------------------------
	# Massage spawner surveys into array for use in expansion functions
	#------------------------------------------------------------------------------
	
	spawner_surveys_mat <- list(); length(spawner_surveys_mat) <- n.species
	
	indicator <- list(); length(indicator) <- n.species
	
	# Create empty data table for number of indicator and non-indicator per species
	numStreams <- data.frame(
		species = species_all,
		indicator = rep(0, length(species_all)),
		nonindicator = rep(0, length(species_all))
	)
	
	for(s in 1:n.species){
		
		spawner_surveys.s <- subset(spawner_surveys, species_name == species_vec[s])
		
		# Which streams are monitored for that species
		streams.s <- sort(unique(spawner_surveys.s$stream_name_pse))
		
		# Extract indicator (Y/N) for each stream for species s
		indicator[[s]] <- tapply(spawner_surveys.s$indicator, spawner_surveys.s$stream_name_pse, unique)
		numStreams$indicator[which(numStreams$species == species_vec[s])] <- length(which(indicator[[s]] == "Y"))
		numStreams$nonindicator[which(numStreams$species == species_vec[s])] <- length(which(indicator[[s]] == "N"))
		
		# Extract spawner data for each year
		spawner_surveys_mat[[s]] <- array(NA, dim = c(length(streams.s), n.yrs), dimnames = list(streams.s, yrs))
		for(y in 1:n.yrs){ 
			spawner_surveys.sy <- subset(spawner_surveys, species_name == species_vec[s] & year == yrs[y])
			spawner_surveys_mat[[s]][, y]  <- spawner_surveys.sy$stream_observed_count[match(streams.s, spawner_surveys.sy$stream_name_pse)] 
		} # end yrs
	} # end species
	
	
	
	#------------------------------------------------------------------------------
	# Create arrays to store observed and expanded counts for each species
	#------------------------------------------------------------------------------
	region_spawners <- array(NA, dim = c(2, n.species, n.yrs), dimnames = list(c("observed", "expanded"), species_vec, yrs))
	expansion_factors <- list(); length(expansion_factors) <- n.species
	
	for(s in 1:n.species){
		
		if(length(which(indicator[[s]] == "Y")) == 0){
			
			warning(paste0("No indicator stream for ", species_vec[s], " in ", r, ". No expansion done."))
			
		} else if(length(which(indicator[[s]] == "Y")) == 1){
			warning(paste0("Only one indicator stream for ", species_vec[s], " in ", r, ". Observed = expanded."))
			
			region_spawners[1, s, ] <- spawner_surveys_mat[[s]][which(indicator[[s]] == "Y"), ]
			region_spawners[2, s, ] <- spawner_surveys_mat[[s]][which(indicator[[s]] == "Y"), ]
			
		} else {
			exp1 <- ExpFactor1(sampledSpawners = t(spawner_surveys_mat[[s]][which(indicator[[s]] == "Y"), ]), years = yrs)
			
			
			if(length(which(indicator[[s]] == "N")) == 1){
				
				exp2 <- ExpFactor2(spawnersInd = t(spawner_surveys_mat[[s]][which(indicator[[s]] == "Y"), ]),
													 spawnersNonInd = as.matrix(spawner_surveys_mat[[s]][which(indicator[[s]] == "N"), ]),
													 years = yrs)
			} else if(length(which(indicator[[s]] == "N")) == 0){
				exp2 <- list(1)
			} else {
				exp2 <- ExpFactor2(spawnersInd = t(spawner_surveys_mat[[s]][which(indicator[[s]] == "Y"), ]),
													 spawnersNonInd = t(spawner_surveys_mat[[s]][which(indicator[[s]] == "N"), ]),
													 years = yrs)
			}
			# returned 8090 = no decades have sufficient data...
			
			if(length(unique(exp2[[1]])) > 1){
				stop(paste0("Expansion Factor 2 differs by decade for ", species_vec[s], ". Check"))
			}
			
			# return time series for region/species
			region_spawners[1, s, ] <- apply(spawner_surveys_mat[[s]][which(indicator[[s]] == "Y"), ], 2, sum, na.rm = TRUE)
			region_spawners[2, s, ] <- apply(spawner_surveys_mat[[s]][which(indicator[[s]] == "Y"), ], 2, sum, na.rm = TRUE) * exp1[[1]] * exp2[[1]]
			
			expansion_factors[[s]] <- list(
				exp1 = exp1[[1]],
				exp2 = exp2[[1]]
			)
		}
		
	} # end s species

# 	###############################################################################
# 	# Plot expansion factors
# 	###############################################################################
# 	# Initiate diagnostic plots
# 	pdf(file = paste0("output/expanded-spawners/figures/expansion_factors_", r, ".pdf"), width = 7, height = 10, pointsize = 12)
# 	# quartz(width = 7, height = 9, pointsize = 14)
# 	par(mfrow = c(3,1), mar = c(4,5,2,1), oma = c(0,0,2,0))
# 	
# 	for(s in 1:n.species){
# 		if(sum(!is.na(region_spawners[2,s,])) > 0){
# 			
# 			plot(yrs, region_spawners[2,s,]*10^-3, "o", bty = "l", las = 1, xlab = "", ylab = "Spawners (thousands)", ylim = c(0, max(region_spawners[2,s,]*10^-3, na.rm = TRUE)), xpd = NA)
# 			
# 			abline(v = seq(1940, 2025, 5), lty = 3, col = grey(0.6))
# 			abline(h = pretty(region_spawners[2, s, ]*10^-3), lty = 3, col = grey(0.6))
# 			points(yrs, region_spawners[1, s, ]*10^-3, "o", col = 2, xpd = NA)
# 			mtext(side = 3, outer = TRUE, paste0(r, " ", species_vec[s]))
# 			legend("topright", pch = 1, col = c(1,2), c("Expanded", "Observed (indicator)"), bg = "white", lwd = 1)
# 			
# 			plot(yrs, expansion_factors[[s]]$exp1, col = ifelse(expansion_factors[[s]]$exp1 == 1, 1, 4), las = 1, ylab = "Expansion Factor 1", xlab = "", bty = "l")
# 			abline(v = seq(1940, 2025, 5), lty = 3, col = grey(0.6))
# 			abline(h = pretty(expansion_factors[[s]]$exp1), lty = 3, col = grey(0.6))
# 			
# 			if(length(expansion_factors[[s]]$exp2) == 1){
# 				exp2.s <- rep(expansion_factors[[s]]$exp2, length(yrs))
# 			} else {
# 				exp2.s <- expansion_factors[[s]]$exp2
#  			}
# 			plot(yrs, exp2.s,  las = 1, ylab = "Expansion Factor 2", xlab = "", bty = "l")
# 			abline(v = seq(1940, 2025, 5), lty = 3, col = grey(0.6))
# 			abline(h = pretty(expansion_factors[[s]]$exp1), lty = 3, col = grey(0.6))
# 			
# 		}
# 	}
# 	dev.off()
	###############################################################################
	# Save output
	###############################################################################
	
	saveRDS(region_spawners, file = paste0("output/expanded-spawners/", r, "-spawners.rds"))
	saveRDS(expansion_factors, file = paste0("output/expanded-spawners/", r, "-expansion-factors.rds"))
	
	write.csv(numStreams, file = paste0("output/num-surveys/", r, "-numSurveys.csv"), row.names = FALSE)
	
} # end r
