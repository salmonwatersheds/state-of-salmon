###############################################################################
# Tweaks to spawner survey data for SOS expansions
# April 29, 2025
# Steph Peacock (speacock@psf.ca)
###############################################################################
library(dplyr)
# Here, we read in spawner survey data in the PSE, and revise the indicator/
# non-indicator designations that are used in expansions where needed
# Also add 2024 data from escapement bulletin for WVI
source("https://raw.githubusercontent.com/salmonwatersheds/population-indicators/refs/heads/master/code/functions_general.R")

# Read in spawner survey data from PSE, *including POP_ID field* 
# Note that TTC data and other data for Yukon aren't in NuSEDS. Best to start with PSF data and then add POPID
spawner_surveys.all <- read.csv(paste0(get_XDrive(), "1_PROJECTS/1_Active/State of Salmon/2_Data & Analysis/state-of-salmon/data/ignore/dataset2_spawner-surveys_2026-03-16.csv")) %>%
	dplyr::filter(year >= 1950, !is.na(stream_observed_count)) %>% # Use only data from 1950 to present
		dplyr::select(region, species_name, species_qualified, streamid, stream_name_pse, GFE_ID, indicator, latitude, longitude, year, stream_observed_count, source_id) %>%
	left_join(read.csv(paste0(get_XDrive(), "1_PROJECTS/1_Active/State of Salmon/2_Data & Analysis/state-of-salmon/data/ignore/3_nuseds_cuid_streamid_Reynolds_2026-03-16.csv")) %>%
							rename("species_name" = SPECIES,
										 "species_qualified" = SPECIES_QUALIFIED,
										 "indicator_nuseds" = IS_INDICATOR) %>%
							select(streamid, POP_ID, indicator_nuseds) %>%
							distinct(.keep_all = TRUE)
							) %>%
	select(region, species_name, species_qualified, streamid, POP_ID, stream_name_pse, GFE_ID, indicator_nuseds, latitude, longitude, year, stream_observed_count, source_id)
	
###############################################################################
# Change indicator/non-indicator designations from NuSEDS
###############################################################################

# Check indicator designation from LGL - does it match NuSEDS?
# Reference for LGL Indicators: English, K.K. 2016. Review of Escapement Indicator Streams for the North and Central Coast Salmon Monitoring Program Final Report. Report prepared by LGL Ltd for Pacific Salmon Foundation and Fisheries and Oceans Canada. Available from https://salmonwatersheds.ca/document/lib_440/.

lgl <- read.csv("data/LGL_Indicators_2017.csv") %>%
	dplyr::select(POP_ID, Indicator, SPP, GFE_ID, SYS_NM)#, CU_findex, CU_name, CU_index)
species_qualified <- sort(unique(lgl$SPP))

# Does LGL have a single indicator designation for a POP_ID?
z <- tapply(lgl$Indicator, lgl$POP_ID, function(x){length(unique(x))})
which(z == 2) # yes
lgl %>% filter(POP_ID %in% as.numeric(names(z[which(z == 2)]))) # For pink salmon..

# Note that POP_ID is the same for even and odd pink, but indicator designation may be different
# Create new POP_ID_pk where POP_ID + 0.1 is even and POP_ID + 0.2 is odd
lgl <- lgl %>%
	mutate(POP_ID_pk = case_when(
		SPP == "PKE" ~ POP_ID + 0.1,
		SPP == "PKO" ~ POP_ID + 0.2,
		.default = POP_ID
	))

spawner_surveys.all <- spawner_surveys.all %>%
	mutate(POP_ID_pk = case_when(
		species_qualified == "PKE" ~ POP_ID + 0.1,
		species_qualified == "PKO" ~ POP_ID + 0.2,
		.default = POP_ID
	))

# Add LGL indicator field
indicator_comparisons <- spawner_surveys.all %>%
	filter(POP_ID %in% lgl$POP_ID) %>%
	select(region, species_name, species_qualified, streamid, POP_ID, POP_ID_pk, stream_name_pse, GFE_ID, indicator_nuseds, latitude, longitude) %>%
	distinct(POP_ID_pk, .keep_all = TRUE) %>%
	left_join(lgl %>%
							rename("indicator_lgl" = Indicator) %>%
							select(POP_ID_pk, indicator_lgl)
						) %>%
	select(region, species_name, species_qualified, streamid, POP_ID, POP_ID_pk, stream_name_pse, GFE_ID, indicator_nuseds, indicator_lgl)

# Which indicator designations are different?
indicator_comparisons %>% filter(indicator_nuseds != indicator_lgl) %>%
	select(region, species_qualified, streamid, stream_name_pse, indicator_nuseds, indicator_lgl)

indicator_comparisons %>% filter(indicator_nuseds != indicator_lgl) %>%
	summarise(YtoN = length(which(indicator_nuseds == "Y" & indicator_lgl == "N")),
						NtoY = length(which(indicator_nuseds == "N" & indicator_lgl == "Y")))

# Chinook and chum; the LGL changes seem to make sense...
# Majority were changes from N to Y, so more indicator streams in LGL data

# Create new master indicator variable in spawner_surveys.all
spawner_surveys.all <- spawner_surveys.all %>%
	left_join(lgl %>%
							rename("indicator_lgl" = Indicator) %>%
							select(POP_ID_pk, indicator_lgl)
	) %>%
	mutate(indicator = case_when(
		is.na(indicator_lgl) ~ indicator_nuseds, # if there is no indicator_lgl, use nuseds
		!is.na(indicator_lgl) ~ indicator_lgl
	))

# Need to set pink salmon as indicators if EITHER even or odd is an indicator
pink_popids <- unique(spawner_surveys.all$POP_ID[spawner_surveys.all$species_name == "Pink"])
length(unique(pink_popids))

for(i in 1:length(unique(pink_popids))){
	ss.i <- spawner_surveys.all %>% filter(POP_ID == pink_popids[i])
	if("Y" %in% unique(ss.i$indicator)){
		spawner_surveys.all$indicator[which(spawner_surveys.all$POP_ID == pink_popids[i])] <- "Y"
	} else if("N" %in% unique(ss.i$indicator)){
		spawner_surveys.all$indicator[which(spawner_surveys.all$POP_ID == pink_popids[i])] <- "N"
	}
}

z <- tapply(spawner_surveys.all$indicator, spawner_surveys.all$POP_ID, function(x){length(unique(x))})
which(z == 2)

# Fill in missing indicator designations using same criteria as WVI pinks: 10 years of data in the past 20 years
start.year <- max(spawner_surveys.all$year) - 20 + 1

ind <- which(is.na(spawner_surveys.all$indicator))
for(i in 1:length(ind)){
	popid.i <- spawner_surveys.all$POP_ID[ind[i]]
	if("N" %in% unique(spawner_surveys.all$indicator[which(spawner_surveys.all$POP_ID == popid.i)])){
		spawner_surveys.all$indicator[ind[i]] <- "N"
	} else if("Y" %in% unique(spawner_surveys.all$indicator[which(spawner_surveys.all$POP_ID == popid.i)])){
		spawner_surveys.all$indicator[ind[i]] <- "Y"
	} else {
		recent.yrs <- length(which(spawner_surveys.all$year[which(spawner_surveys.all$POP_ID == popid.i)] >= start.year))
		if(recent.yrs >= 10){
			spawner_surveys.all$indicator[ind[i]] <- "Y"
			print(popid.i)
		} else {
			spawner_surveys.all$indicator[ind[i]] <- "N"
		}
	}
}

# Check no duplicates
z <- tapply(spawner_surveys.all$indicator, spawner_surveys.all$POP_ID, function(x){length(unique(x))})
which(z == 2)

# Are there any NAs remaining?
which(is.na(spawner_surveys.all$indicator)) # No

###############################################################################
# Remove 2024 Atnarko sockeye for expansions; it is not reasonable to expand from this single stream
###############################################################################

spawner_surveys.all <- spawner_surveys.all[-which(spawner_surveys.all$year == 2024 & spawner_surveys.all$stream_name_pse == "ATNARKO RIVER"), ]

###############################################################################
# Add 2025 WVI estimated and re-visit indicator designation
###############################################################################

species_vec  <- sort(unique(spawner_surveys.all$species_name))

# Set Malksope coho as an indicator (McHugh & King 2018)
spawner_surveys.all$indicator[which(spawner_surveys.all$species_name == "Coho" & spawner_surveys.all$stream_name_pse == "MALKSOPE RIVER")] <- "Y"

# More recent data for WVI
wvi2025 <- read.csv("data/2025_WVI_Esc_Bulletin_7_Oct_24.csv")
for(s in 1:5){ # No steelhead
	wvi2025.s <- wvi2025 %>% filter(Species == species_vec[s])
	spawner_surveys.s <- spawner_surveys.all %>% filter(species_name == species_vec[s], region == "West Vancouver Island")
	
	for(i in 1:dim(wvi2025.s)[1]){
		if(wvi2025.s$System[i] %in% unique(spawner_surveys.s$stream_name_pse)){ 
			spawner_surveys.si <- spawner_surveys.s %>% 
				filter(stream_name_pse == wvi2025.s$System[i]) %>%
				arrange(year)
			
			spawner_surveys.add <- spawner_surveys.si %>% tail(1)
			spawner_surveys.add$year <- 2025 
			spawner_surveys.add$stream_observed_count <- wvi2025.s$Count[i]
			spawner_surveys.add$source_id <- "McHugh_20261024" 
			
			spawner_surveys.all <- spawner_surveys.all %>% bind_rows(spawner_surveys.add)
			
			# plot(spawner_surveys.si$year,
			# 		 spawner_surveys.si$stream_observed_count, "o", xlim = c(min(spawner_surveys.si$year), 2025), xlab = "", ylab = "Count", main = paste(wvi2025.s$System[i], unique(spawner_surveys.si$indicator), sep = " - "))
			# points(2025, wvi2025.s$Count[i], col = 2, pch = 19)
			# 
			# readline("Press return to advance to next stream.")
			# 
		}
	} # end i
} #end s

# For Pink salmon, declare indicators as those with >10 years of data in the past 20 years
start.year <- max(spawner_surveys.all$year) - 20 + 1
pink_nyrs <- spawner_surveys.all %>% 
	filter(species_name == "Pink", 
				 region %in% c("East Vancouver Island & Mainland Inlets",
												"West Vancouver Island"),
				 year > start.year) %>%
	group_by(stream_name_pse) %>%
	summarise(species_name = unique(species_name),
						nyrs_20 = length(unique(year)),
						most_recent_year = max(year),
						indicator = paste(unique(indicator), collapse = ", ")) %>%
	arrange(20-nyrs_20, stream_name_pse) %>%
	data.frame()

# Designate streams with > 10 years as indicators
spawner_surveys.all$indicator[which(spawner_surveys.all$species_name == "Pink" & 
																			spawner_surveys.all$region %in% c("East Vancouver Island & Mainland Inlets",
																																				"West Vancouver Island") & 
																			spawner_surveys.all$stream_name_pse %in% pink_nyrs$stream_name_pse[pink_nyrs$nyrs_20 > 10])] <- "Y"

# Designate streams with <= 10 years as non-indicators
spawner_surveys.all$indicator[which(spawner_surveys.all$species_name == "Pink" & 
																			spawner_surveys.all$region %in% c("East Vancouver Island & Mainland Inlets",
																																				"West Vancouver Island") & 
																			spawner_surveys.all$stream_name_pse %in% pink_nyrs$stream_name_pse[pink_nyrs$nyrs_20 <= 10])] <- "N"

# Keep a couple of indicators that have < 10 years but recent monitoring and were designated indicators
pink_nyrs %>% filter(nyrs_20 <= 10, grepl("Y", indicator))
# Most have no recent data

spawner_surveys.all$indicator[which(spawner_surveys.all$species_name == "Pink" & 
																			spawner_surveys.all$region %in% c("East Vancouver Island & Mainland Inlets",
																																				"West Vancouver Island") & 
																			spawner_surveys.all$stream_name_pse %in% c("HEYDON CREEK", "WAKEMAN RIVER", "EMBLEY CREEK" ))] <- "Y"

# z <- spawner_surveys.all %>%
# 	filter(species_name == "Pink", region == "Vancouver Island & Mainland Inlets", stream_name_pse == "GRASSY CREEK") %>%
# 	arrange(year)
# plot(z$year, z$stream_observed_count, "o", main = paste(unique(z$stream_name_pse), unique(z$species_name), sep = " - "))

spawner_surveys.all %>% filter(species_name == "Pink" & region %in% c("East Vancouver Island & Mainland Inlets",
																																			"West Vancouver Island")) %>%
	dplyr::select(stream_name_pse, indicator) %>%
	distinct(.keep_all = TRUE) %>% arrange(indicator)

###############################################################################
# Add/update updated steelhead data
###############################################################################
# - Add newer data
# - Update lat/lon of Little Campbell

# steelhead_update <- read.csv("https://github.com/salmonwatersheds/steelhead-data/blob/main/output/dataset2_spawner-surveys_steelhead.csv")
steelhead_update <- read.csv(paste0(X_Drive, "1_PROJECTS/1_Active/Population Methods and Analysis/population-data/steelhead-data/output/archive/dataset2_spawner-surveys_Steelhead_2026-08-12.csv"))

names(steelhead_update)
names(spawner_surveys.all)

spawner_surveys.all <- spawner_surveys.all %>%
	select(region, species_name, species_qualified, streamid, stream_name_pse, latitude, longitude, indicator, year, stream_observed_count, source_id) %>% 
	filter(streamid %in% steelhead_update$streamid == FALSE) %>% # Remove old steelhead data
	bind_rows(steelhead_update %>% 
							select(region, species_name, species_qualified, streamid, stream_name_pse, latitude, longitude, indicator, year, stream_observed_count, source_id)) %>%
	arrange(region, species_name, stream_name_pse)
	

###############################################################################
# Write .csv of revised spawner survey data
###############################################################################

write.csv(spawner_surveys.all, file = "data/spawner_surveys_revised_2026-08-14.csv")
	
