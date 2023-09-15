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

###############################################################################
# Import data (this has been pulled from the SWP database using pull-data.R)
###############################################################################

# Read in all spawner survey data
spawner_surveys.all <-read.csv("data/spawner_surveys.csv", na.strings = c(-989898)) %>%
	# subset(species_name != 'Steelhead') %>% # Remove steelhead 
	subset(!is.na(year)) %>% # Remove stream with no data
	subset(year >= 1950) # Use only data from 1950 to present

#------------------------------------------------------------------------------
# Define variables
#------------------------------------------------------------------------------

# Arrange regions from north to south
regions <- c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")
spawner_surveys.all$region <- factor(spawner_surveys.all$region, levels = regions)

# Arrange species
species_pooled <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead") 

species_xref <- data.frame(
	species = sort(unique(spawner_surveys.all$species_name)), 
	species_pooled = c("Chinook", "Chum", "Coho", "Sockeye", "Pink", "Pink", "Sockeye", "Steelhead") 
)

# Create variable in data for pooled species
spawner_surveys.all$species_pooled <- species_xref$species_pooled[match(spawner_surveys.all$species_name, species_xref$species)]

#------------------------------------------------------------------------------
# Read in spatial boundaries of PSE regions for subsetting streams
#------------------------------------------------------------------------------
# Note: the region field in the spawner_surveys.all data can only assume one value for CU,
# resulting in some streams from larger (pink) CUs being erroneously assigned to 
# Nass or Skeena (for example).

library(sf)

pse_regions <- st_read("data/ignore/pse-regions/se_boundary_regions_simple.shp") %>% st_transform(crs = 4269)

###############################################################################
# Select region
###############################################################################

for(R in 1:6){

	r <- c("Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser")[R]
pse_region <- pse_regions[which(pse_regions$Region == r), ]
streamid <- unique(spawner_surveys.all$streamid)
stream_points <- data.frame(
	streamid = streamid,
	lon = spawner_surveys.all$longitude[match(streamid, spawner_surveys.all$streamid)], 
	lat = spawner_surveys.all$latitude[match(streamid, spawner_surveys.all$streamid)]) %>%
	st_as_sf(coords = c("lon", "lat"), crs = 4269)

intrscts <- st_intersects(stream_points, pse_region, sparse = FALSE)
incl <- which(c(intrscts) == TRUE)

spawner_surveys <- spawner_surveys.all[which(spawner_surveys.all$streamid %in% streamid[incl]), ]

# Not all species are necessarily present in each region
# E.g., no sockeye or pink in Yukon
# Create vector of species that are present in the selected region:
species <- sort(unique(spawner_surveys$species_pooled))
n.species <- length(species)

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

# Source functions to apply expansion factors based on indicator/non-indicator
# streams (English et al. 2018; code provided by LGL Ltd.)
source("code/expansion-functions.R")

#------------------------------------------------------------------------------
# Massage spawner surveys into array for use in expansion functions
#------------------------------------------------------------------------------

spawner_surveys_mat <- list(); length(spawner_surveys_mat) <- n.species

indicator <- list(); length(indicator) <- n.species

# Create data table of number of indicator and non-indicator per species
numStreams <- data.frame(
	species = species_pooled,
	indicator = rep(0, length(species_pooled)),
	nonindicator = rep(0, length(species_pooled))
)

for(s in 1:n.species){
	
	spawner_surveys.s <- subset(spawner_surveys, species_pooled == species[s])
	
	# Which streams are monitored for that species
	streams.s <- sort(unique(spawner_surveys.s$stream_name_pse))
	
	# Extract indicator (Y/N) for each stream for species s
	indicator[[s]] <- tapply(spawner_surveys.s$indicator, spawner_surveys.s$stream_name_pse, unique)
	numStreams$indicator[which(numStreams$species == species[s])] <- length(which(indicator[[s]] == "Y"))
	numStreams$nonindicator[which(numStreams$species == species[s])] <- length(which(indicator[[s]] == "N"))
	
	# Extract spawner data for each year
	spawner_surveys_mat[[s]] <- array(NA, dim = c(length(streams.s), n.yrs), dimnames = list(streams.s, yrs))
	for(y in 1:n.yrs){ 
		spawner_surveys.sy <- subset(spawner_surveys, species_pooled == species[s] & year == yrs[y])
		spawner_surveys_mat[[s]][, y]  <- spawner_surveys.sy$stream_observed_count[match(streams.s, spawner_surveys.sy$stream_name_pse)] 
	} # end yrs
} # end species

#------------------------------------------------------------------------------
# Create arrays to store observed and expanded counts for each species
#------------------------------------------------------------------------------
region_spawners <- array(NA, dim = c(2, n.species, n.yrs), dimnames = list(c("observed", "expanded"), species, yrs))
expansion_factors <- list(); length(expansion_factors) <- n.species

for(s in 1:n.species){
	
	if(length(which(indicator[[s]] == "Y")) == 1){
		warning(paste0("Only one indicator stream for ", species[s], " in ", r, ". Can't expand."))
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
	
	if(length(exp2[[1]]) > 1){
		stop("Expansion Factor 2 differs by decade. Check")
	}
	
	# return time series for region/species
	region_spawners[1, s, ] <- apply(spawner_surveys_mat[[s]][which(indicator[[s]] == "Y"), ], 2, sum, na.rm = TRUE)
	region_spawners[2, s, ] <- apply(spawner_surveys_mat[[s]][which(indicator[[s]] == "Y"), ], 2, sum, na.rm = TRUE) * exp1[[1]] * exp2[[1]]
	
	expansion_factors[[s]] <- list(
		exp1 = exp1[[1]],
		exp2 = exp2[[1]]
	)
		}
	
}

###############################################################################
# Save output
###############################################################################

saveRDS(region_spawners, file = paste0("output/", r, "-spawners.rds"))
saveRDS(expansion_factors, file = paste0("output/", r, "-expansion-factors.rds"))

write.csv(numStreams, file = paste0("output/", r, "-numStreams.csv"), row.names = FALSE)

} # end r
