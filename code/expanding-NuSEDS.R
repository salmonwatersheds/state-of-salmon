###############################################################################
# Expansion of NuSEDS spawner counts to both CU and regional scales
#
# For code that expands from spawner surveys in database, see regional-expansions.R
# Author: Steph Peacock
# Date: July 10, 2023

###############################################################################

library(dplyr)
library(sf)
library(viridis)
library(stringr)

###############################################################################
# Read in data, subset, and filter
###############################################################################

# Read in all area NuSEDs and calculate MAX_ESTIMATE
dat <- read.csv("data/All Areas NuSEDS.csv") %>% subset(ANALYSIS_YR >= 1950)
var_in_MAX_ESTIMATE <- c("NATURAL_ADULT_SPAWNERS", "NATURAL_JACK_SPAWNERS", "NATURAL_SPAWNERS_TOTAL", "ADULT_BROODSTOCK_REMOVALS", "JACK_BROODSTOCK_REMOVALS", "TOTAL_BROODSTOCK_REMOVALS", "OTHER_REMOVALS", "TOTAL_RETURN_TO_RIVER")
anydata <- apply(!is.na(dat[, var_in_MAX_ESTIMATE]), 1, sum)
dat <- dat[which(anydata > 0), ]
dat$MAX_ESTIMATE <- apply(dat[, var_in_MAX_ESTIMATE], 1, max, na.rm = TRUE)

# Clean up Estimate Classification field of nuseds
dat$ESTIMATE_CLASSIFICATION[dat$ESTIMATE_CLASSIFICATION == "PRESENCE/ABSENCE (TYPE-6)"] <- "PRESENCE-ABSENCE (TYPE-6)"
dat$ESTIMATE_CLASSIFICATION[dat$ESTIMATE_CLASSIFICATION =="NO SURVEY THIS YEAR"] <- "NO SURVEY"
dat$ESTIMATE_CLASSIFICATION[dat$ESTIMATE_CLASSIFICATION ==""] <- "UNKNOWN"

# Re-level ESTIMATE_CLASSIFICATION for easy numeric calculations
dat$ESTIMATE_CLASSIFICATION <- factor(
	dat$ESTIMATE_CLASSIFICATION,
	levels = c(
		"TRUE ABUNDANCE (TYPE-1)",
		"TRUE ABUNDANCE (TYPE-2)",
		"RELATIVE ABUNDANCE (TYPE-3)",
		"RELATIVE ABUNDANCE (TYPE-4)",
		"RELATIVE ABUNDANCE (TYPE-5)",
		"PRESENCE-ABSENCE (TYPE-6)",
		"RELATIVE: CONSTANT MULTI-YEAR METHODS",
		"RELATIVE: VARYING MULTI-YEAR METHODS",
		"NO SURVEY",
		"UNKNOWN"))

# Subset to only include data since 1950

# Read in CU link (note that API no longer works!)
# nuseds_cu <- read.csv("https://open.canada.ca/data/en/datastore/dump/fc475853-b599-4e68-8d80-f49c03ddc01c?bom=True")
nuseds_cu <- read.csv("data/ignore/conservation_unit_system_sites.csv")

#----------------------------------------------------------------------------
# Match region in NuSEDS
#----------------------------------------------------------------------------

pse_regions <- read.csv("data/appendix1_full.csv", na.strings = '-989898')

# # Calculate generation length
# gen_length <- read.csv("data/gen_length.csv")
# pse_regions$gen_length <- gen_length$gen_length[match(pse_regions$cuid, gen_length$cuid)]
# pse_regions$reg_sp <- paste(pse_regions$region, pse_regions$species_abbr, sep = "-")
# x <- tapply(pse_regions$gen_length, pse_regions$reg_sp, median, na.rm = TRUE)
# write.csv(x, file = "data/gen_length_regions.csv")

# Match PSE region to NuSEDS
nuseds_cu$REGION <- pse_regions$region[match(nuseds_cu$FULL_CU_IN, pse_regions$cu_index)]

cus_no_region <- unique(nuseds_cu$CU_NAME[is.na(nuseds_cu$REGION)])

nuseds_cu$region[nuseds_cu$CU_NAME %in% c("FRANCOIS-EARLY SUMMER TIMING", "FRANCOIS-LATE TIMING","MIDDLE FRASER", "UPPER FRASER")] <- "Fraser"

# Fill in missing regions

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("INTERIOR FRASER<<BIN>>", "FRASER RIVER<<BIN>>","FRASER RIVER MIGRATORY COUNTS<<BIN>>", "FRASER-CROSS-CU SUPPLEMENTATION EXCLUSION<<BIN>>", "FRASER-EARLY SUMMER TIMING", "MIDDLE FRASER", "UPPER FRASER", "FRANCOIS-LATE TIMING", "CARIBOO-SUMMER TIMING", "FRASER-HARRISON FALL TRANSPLANT_FA_0.3<<BIN>>", "ALOUETTE_EARLY SUMMER<<EXTIRPATED>>", "FRASER-MISCELLANEOUS<<BIN>>", "FRANCOIS-EARLY SUMMER TIMING", "INDIAN/KRUGER-EARLY SUMMER TIMING"))] <- "Fraser"

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("OSOYOOS"))] <- "Columbia"

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("OWIKENO-LATE TIMING", "WHALEN", "(N)SYLVIA CREEK", "HECATE STRAIT-FJORDS", "HECATE LOWLANDS"))] <- "Central Coast"

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("BABINE", "PRUDHOMME", "SHAWATLAN", "TAHLO/MORRISON", "BOWSER", "(N)ONERKA", "NASS-SKEENA ESTUARY", "MIDDLE-UPPER SKEENA"))] <- "Skeena"

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("CLEMENTS", "SPLIT MOUNTAIN/LEVERSON", "OWEEGEE"))] <- "Nass"


nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("SOUTHERN FJORDS", "SOUTHERN BC-CROSS-CU SUPPLEMENTATION EXCLUSION<<BIN>>", "(N)GLENDALE", "OWOSSITSA", "GREAT CENTRAL/SPROAT<<BIN>>", "PACK", "SOUTH-MISCELLANEOUS<<BIN>>", "HOMATHKO-KLINAKLINI-SMITH-RIVERS-BELLA COOLA-DEAN","VILLAGE BAY"))] <- "Vancouver Island & Mainland Inlets"

nuseds_cu$REGION[which(nuseds_cu$CU_NAME %in% c("(P)HATCHERY EXCLUSION-PALLANT CREEK", "(N)MAYER", "NORTH HAIDA GWAII", "EAST HAIDA GWAII"))] <- "Haida Gwaii"


# Somehow some populations with no CU were assigned to the wrong region sometimes. Reassign.
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "WHALEN CREEK" & nuseds_cu$SPECIES == "Chinook")] <- "Central Coast"
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "OWEEGEE CREEK" & nuseds_cu$SPECIES %in% c("Pink", "Chum"))] <- "Nass"
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "SYLVIA CREEK" & nuseds_cu$SPECIES == "Chinook")] <- "Central Coast"
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "SOMASS-SPROAT-GC SYSTEM")] <- "Vancouver Island & Mainland Inlets"
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "PACK LAKE CREEK")] <- "Vancouver Island & Mainland Inlets"
nuseds_cu$REGION[which(nuseds_cu$WATERBODY == "SHAWNIGAN CREEK")] <- "Vancouver Island & Mainland Inlets"


###############################################################################
# Select region
###############################################################################

regions <- c("Yukon", "Transboundary", "Nass", "Skeena", "Haida Gwaii", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")

# r <- "Central Coast"

spawner_surveys <- dat[which(dat$POP_ID %in% nuseds_cu$POP_ID[nuseds_cu$REGION == r]), ]

#Re-name to match spawner survey database output
names(spawner_surveys)[which(names(spawner_surveys) == "SPECIES")] <- "species_pooled"
names(spawner_surveys)[which(names(spawner_surveys) == "ANALYSIS_YR")] <- "year"
names(spawner_surveys)[which(names(spawner_surveys) == "WATERBODY")] <- "stream_name"
spawner_surveys$stream_name <- str_to_title(spawner_surveys$stream_name)

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
streams <- sort(unique(spawner_surveys$stream_name))
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

for(s in 1:n.species){
	
	spawner_surveys.s <- subset(spawner_surveys, species_pooled == species[s])
	
	# Which streams are monitored for that species
	streams.s <- sort(unique(spawner_surveys.s$stream_name))
	POP_ID.s <- spawner_surveys.s$POP_ID[match(streams.s, spawner_surveys.s$stream_name)]
	
	# Extract indicator (Y/N) for each stream for species s
	indicator[[s]] <- nuseds_cu$IS_INDICATOR[match(POP_ID.s, nuseds_cu$POP_ID)]
	
	# Extract spawner data for each year
	spawner_surveys_mat[[s]] <- array(NA, dim = c(length(streams.s), n.yrs), dimnames = list(streams.s, yrs))
	for(y in 1:n.yrs){ 
		spawner_surveys.sy <- subset(spawner_surveys, species_pooled == species[s] & year == yrs[y])
		if(nrow(spawner_surveys.sy) > 0){
		spawner_surveys_mat[[s]][, y]  <- spawner_surveys.sy$MAX_ESTIMATE[match(streams.s, spawner_surveys.sy$stream_name)] 
		}
	} # end yrs
} # end species

#------------------------------------------------------------------------------
# Create arrays to store observed and expanded counts for each species
#------------------------------------------------------------------------------
region_spawners <- array(NA, dim = c(2, n.species, n.yrs), dimnames = list(c("observed", "expanded"), species, yrs))
expansion_factors <- list(); length(expansion_factors) <- n.species

for(s in 1:n.species){
	
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
	
	# If there are no indicator stream monitored, then exp1 = Inf. Set to NA.
	exp1[[1]][which(exp1[[1]] == Inf)] <- NA
	
	# Return time series for region/species
	region_spawners[1, s, ] <- apply(spawner_surveys_mat[[s]][which(indicator[[s]] == "Y"), ], 2, sum, na.rm = TRUE)
	region_spawners[2, s, ] <- apply(spawner_surveys_mat[[s]][which(indicator[[s]] == "Y"), ], 2, sum, na.rm = TRUE) * exp1[[1]] * exp2[[1]]
	
	expansion_factors[[s]] <- list(
		exp1 = exp1[[1]],
		exp2 = exp2[[1]]
	)
	
}

###############################################################################
# Check output
###############################################################################
if(3 == 2){
	sp_cols <- viridis(n = 5)
	par(mfrow = c(3,2), mar = c(4,4,2,1), oma = c(1,1,1,0))
	for(s in 1:n.species){
		plot(yrs, expansion_factors[[s]]$exp1, type = "o", bty = "l", main = species[s])
	}
	
	par(mfrow = c(1,1), mar = c(4,4,2,1), oma = c(1,1,1,0))
	plot(range(yrs), c(1, 6), "n", las = 1, bty = "l", ylab = "Expansion Factor 1", xlab = "")
	for(s in 1:n.species){
		lines(yrs, expansion_factors[[s]]$exp1, type = "o", bty = "l", col = sp_cols[s], pch = 19, cex = 0.6, lwd = 1.5)
	}
	legend("topleft", pch = 19, lwd = 1.5, col = sp_cols, legend = species, pt.cex = 0.6, bty = "n")
}
###############################################################################
# Save output
###############################################################################

saveRDS(region_spawners, file = paste0("output/", r, "-spawners_nuseds.rds"))
saveRDS(expansion_factors, file = paste0("output/", r, "-expansion-factors_nuseds.rds"))
