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

# Read in spawner survey data from PSE

spawner_surveys.all <- read.csv("data/dataset2_spawner-surveys_2026-03-16.csv") %>%
	dplyr::filter(year >= 1950, !is.na(stream_observed_count)) %>% # Use only data from 1950 to present
	dplyr::select(region, species_name, species_qualified, streamid, stream_name_pse, GFE_ID, indicator, latitude, longitude, year, stream_observed_count, source_id)

###############################################################################
# Change indicator/non-indicator designations from NuSEDS
###############################################################################

# Check indicator designation from LGL - does it match NuSEDS?
lgl <- read.csv("data/ignore/OUTPUT_NCCStreams_2017.csv") %>%
	dplyr::select(POP_ID, Indicator, SPP, GFE_ID, SYS_NM)#, CU_findex, CU_name, CU_index)
species_qualified <- sort(unique(lgl$SPP))


for(s in 1:length(species_qualified)){ # For each species
	
	spq <- species_qualified[s]
	
	lgl.s <- lgl %>% filter(SPP == spq)
	
	ss.s <- spawner_surveys.all %>% filter(
		region %in% c("Haida Gwaii", "Nass", "Skeena", "Central Coast"),
		species_qualified == spq) %>% 
		group_by(streamid) %>%
		distinct(streamid, .keep_all = TRUE) %>%
		left_join(lgl.s, by = "GFE_ID") 
	
	# Warning: many-to-many relationship - Bella Coola chum, dealt with below
	
	print(unique(ss.s$species_name))
	
	ss.s_different <- ss.s %>%
		dplyr::filter(indicator != Indicator) %>%
		dplyr::select(region, species_qualified, streamid, stream_name_pse, indicator, SYS_NM, Indicator)
	
	if(s == 1){
		indicator_change <- ss.s_different
	} else {
		indicator_change <- indicator_change %>%
			bind_rows(ss.s_different)
	}
	
	if(s == 2){ # Temporary fix for doubled GFE_ID in Chum causing misassinged indicator stream - Nuseds indicator designation is good
		indicator_change <- indicator_change %>% 
			filter(stream_name_pse != "BELLA COOLA RIVER") 
	}
	
	# Replace NuSEDS indicator with LGL's indicator where different
	spawner_surveys.all$indicator[match(ss.s_different$streamid, spawner_surveys.all$streamid)]<- ss.s_different$Indicator

	rm(spq, ss.s, lgl.s, ss.s_different)
	}

write.csv(indicator_change, paste0("data/indicator_changed_", Sys.Date(), ".csv"))

###############################################################################
# Remove 2024 Atnarko sockeye for expansions; it is not reasonable to expand from this single stream
###############################################################################

spawner_surveys.all <- spawner_surveys.all[-which(spawner_surveys.all$year == 2024 & spawner_surveys.all$stream_name_pse == "ATNARKO RIVER"), ]

###############################################################################
# Add 2024 WVI estimated and re-visit indicator designation
###############################################################################

species_vec  <- sort(unique(spawner_surveys.all$species_name))

# Set Malksope coho as an indicator (McHugh & King 2018)
spawner_surveys.all$indicator[which(spawner_surveys.all$species_name == "Coho" & spawner_surveys.all$stream_name_pse == "MALKSOPE RIVER")] <- "Y"

# More recent data for WVI
wvi2025 <- read.csv("data/2025_WVI_Esc_Bulletin_7_Oct_24.csv")
for(s in 1:5){
	wvi2025.s <- wvi2025 %>% filter(Species == species_vec[s])
	spawner_surveys.s <- spawner_surveys.all %>% filter(species_name == species_vec[s], region == "East Vancouver Island & Mainland Inlets")
	
	for(i in 1:dim(wvi2025.s)[1]){
		if(wvi2025.s$System[i] %in% unique(spawner_surveys.s$stream_name_pse)){ 
			spawner_surveys.si <- spawner_surveys.s %>% 
				filter(stream_name_pse == wvi2025.s$System[i]) %>%
				arrange(year)
			
			spawner_surveys.add <- spawner_surveys.si %>% tail(1)
			spawner_surveys.add$year <- 2025 # Update?
			spawner_surveys.add$stream_observed_count <- wvi2025.s$Count[i]
			spawner_surveys.add$source_id <- "McHugh_20251115" # Hannah find correct source ID and update for 2025
			
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
pink_nyrs <- spawner_surveys.all %>% 
	filter(species_name == "Pink", region == "East Vancouver Island & Mainland Inlets", year > 2005) %>%
	group_by(stream_name_pse) %>%
	summarise(species_name = unique(species_name),
						nyrs_20 = length(unique(year)),
						most_recent_year = max(year),
						indicator = paste(unique(indicator), collapse = ", ")) %>%
	arrange(20-nyrs_20, stream_name_pse) %>%
	data.frame()

# Designate streams with > 10 years as indicators
spawner_surveys.all$indicator[which(spawner_surveys.all$species_name == "Pink" & 
																			spawner_surveys.all$region == "East Vancouver Island & Mainland Inlets" & 
																			spawner_surveys.all$stream_name_pse %in% pink_nyrs$stream_name_pse[pink_nyrs$nyrs_20 > 10])] <- "Y"

# Designate streams with <= 10 years as non-indicators
spawner_surveys.all$indicator[which(spawner_surveys.all$species_name == "Pink" & 
																			spawner_surveys.all$region == "East Vancouver Island & Mainland Inlets" & 
																			spawner_surveys.all$stream_name_pse %in% pink_nyrs$stream_name_pse[pink_nyrs$nyrs_20 <= 10])] <- "N"

# Keep a couple of indicators that have < 10 years but recent monitoring and were designated indicators
pink_nyrs %>% filter(nyrs_20 <= 10, grepl("Y", indicator))
# Most have no recent data

spawner_surveys.all$indicator[which(spawner_surveys.all$species_name == "Pink" & 
																			spawner_surveys.all$region == "East Vancouver Island & Mainland Inlets" & 
																			spawner_surveys.all$stream_name_pse %in% c("HEYDON CREEK", "WAKEMAN RIVER", "EMBLEY CREEK" ))] <- "Y"

# z <- spawner_surveys.all %>%
# 	filter(species_name == "Pink", region == "Vancouver Island & Mainland Inlets", stream_name_pse == "GRASSY CREEK") %>%
# 	arrange(year)
# plot(z$year, z$stream_observed_count, "o", main = paste(unique(z$stream_name_pse), unique(z$species_name), sep = " - "))

spawner_surveys.all %>% filter(species_name == "Pink" & region == "East Vancouver Island & Mainland Inlets") %>%
	dplyr::select(stream_name_pse, indicator) %>%
	distinct(.keep_all = TRUE) %>% arrange(indicator)

###############################################################################
# Write .csv of revised spawner survey data
###############################################################################

write.csv(spawner_surveys.all, file = "data/spawner_surveys_revised2025.csv")
	
