##############################################################################
# This code compiles the region-specific data for (1) spawner abundance and 
# (2) run size (where available) from EITHER region-specific datasets 
# (preferable) or expansions from spawner surveys.
# 
# Author: Steph Peacock
# Date: Oct 25, 2023
###############################################################################

library(dplyr)
library(abind)

source("code/functions.R")

regions <- c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")

species <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")

# Generation length by species and region (for smoothing)
genLength <- read.csv("data/gen_length_regions.csv") 

###############################################################################
###############################################################################
# Go through each region and add data
###############################################################################
###############################################################################

pdf(file = paste0("output/ignore/figures/spawners_and_runsize_ALL_", Sys.Date(), ".pdf"),
		width = 6, height = 4, pointsize = 10)

###############################################################################
# Yukon
###############################################################################

#------------------------------------------------------------------------------
# Yukon: Chinook
#------------------------------------------------------------------------------
# ytck0 <- read.csv('data/yukon_chinook_appendixB11.csv') # Pre-2023
ytck <- read.csv('data/JTC 2023 B11.csv') # 2023 update

# # Compare RR estimates that started to be reported in 2024 for 2023 and historical
# # Outcome: RR estimates tend to be slightly higher historically; may lead to more significant declines in trend metrics?
# plot(ytck$Year, ytck$RR.Spawning.escapement.estimate, "o")
# points(ytck0$Year, ytck0$Spawning.escapement.estimate, "o", col = grey(0.8), cex = 0.8, pch = 19)
# 
# plot(ytck$Year, ytck$RR.Canadian.origin.total.run.size.estimate, "o")
# points(ytck$Year, ytck$Historic.Canadian.origin.total.run.size.estimate, col = 2, pch = 19, cex = 0.5)
# points(ytck0$Year, ytck0$Canadian.origin.total.run.size.estimate, "o", col = grey(0.8), cex = 0.8, pch = 19)

# Check if years are continuous
unique(diff(ytck$Year)) # Yes, all one year apart

# Reformat data for SPS
ytck_sps <- data.frame(
	region = rep("Yukon", dim(ytck)[1]),
	species = rep("Chinook", dim(ytck)[1]),
	year = ytck$Year,
	spawners = ytck$RR.Spawning.escapement.estimate, # ytck$Spawning.escapement.estimate, 
	smoothedSpawners = NA,
	runsize = ytck$RR.Canadian.origin.total.run.size.estimate, #ytck$Canadian.origin.total.run.size.estimate,
	smoothedRunsize = NA
) 

# Smoothing
ytck_sps$smoothedSpawners <- genSmooth(
	abund = ytck_sps$spawners,
	years = ytck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Yukon" & genLength$species == "Chinook"]
	)

ytck_sps$smoothedRunsize <- genSmooth(
	abund = ytck_sps$runsize,
	years = ytck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Yukon" & genLength$species == "Chinook"]
)

plot_abund(ytck_sps)

# First region/species, so initiate sps dataframe
sps_data <- ytck_sps

#------------------------------------------------------------------------------
# Yukon: Chum
#------------------------------------------------------------------------------
ytcm_spawners <- read.csv('data/yukon_chum_appendixB16.csv')
ytcm_runsize <- read.csv('data/yukon_chum_appendixB20.csv')

# Check if years are continuous
unique(diff(ytcm_spawners$Date)) # Yes, all one year apart
unique(diff(ytcm_runsize$Year)) # Yes, all one year apart
# Note run size available for fewer years

# Reformat data for SPS
ytcm_sps <- data.frame(
	region = rep("Yukon", dim(ytcm_spawners)[1]),
	species = rep("Chum", dim(ytcm_spawners)[1]),
	year = ytcm_spawners$Date,
	spawners = ytcm_spawners$Spawning.escapement.estimate, 
	smoothedSpawners = NA,
	runsize = ytcm_runsize$Total.estimated.Canadian.origin.run.size[match(ytcm_spawners$Date, ytcm_runsize$Year)],
	smoothedRunsize = NA
) 

# Smoothing
ytcm_sps$smoothedSpawners <- genSmooth(
	abund = ytcm_sps$spawners,
	years = ytcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Yukon" & genLength$species == "Chum"]
)

ytcm_sps$smoothedRunsize <- genSmooth(
	abund = ytcm_sps$runsize,
	years = ytcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Yukon" & genLength$species == "Chum"]
)

plot_abund(ytcm_sps)

# Add to SPS data
sps_data <- rbind(sps_data, ytcm_sps)

###############################################################################
# Transboundary
###############################################################################

#------------------------------------------------------------------------------
# Transboundary: Chinook
#------------------------------------------------------------------------------

# # (1) CTC data
tbrck <- read.csv('data/tbr_chinook.csv')

# Remove 1975 -> missing Alsek data
tbrck <- tbrck[!is.na(tbrck$Alsek_Escapement), ]
# Check if years are continuous
unique(diff(tbrck$Year)) # Yes, all one year apart

# tbrck2 <- readxl::read_xlsx("data/TCCHINOOK-23-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B2", range = "A4:G52")

# (2) TTC data
ttcck <- read.csv("data/TTC_MERGED.csv") %>%
	filter(SPECIES == "Chinook", TableSource %in% c("E.7_full", "B.12", "D.7"))

ttcck_totals <- ttcck %>%
	filter(Series %in% c("Alsek River - Escapement", "Escapement", "Spawning Escapement")) %>%
	group_by(Year) %>%
	summarise(Spawners = sum(Value))
ttcck_totals <- ttcck_totals %>% left_join(
	ttcck %>%
		filter(Series %in% c("Alsek River - Canada harvest", "Alsek River - Harvest Dry Bay", "Canadian Harvest", "US Harvest", "Canadian Catch", "US Harvest")) %>%
		group_by(Year) %>%
		summarise(Catch = sum(Value))
)


# Check if years are continuous
unique(diff(ttcck_totals$Year)) # Yes, all one year apart

# # Compare
# par(mfrow = c(2,1))
# plot(ttcck_totals$Year, ttcck_totals$Spawners*10^-3, "o", col = 2, bty = "l", xlab = "", ylab = "Spawners (thousands)", las = 1)
# points(tbrck$Year, apply(tbrck[, c("Taku_Escapement", "Stikine_Escapement", "Alsek_Escapement")], 1, sum)*10^-3, "o")
# points(tbrck2$...1, apply(tbrck2[, grep("Esc", names(tbrck2))], 1, sum)*10^-3, "o", col = 4, cex = 0.3)
# legend("topright", pch = 1, lty = 1, col = c(1,2,4), c("CTC data request to E. Hertz", "TTC data", "TCCHINOOK Table B2"))
# 
# plot(ttcck_totals$Year, (ttcck_totals$Spawners + ttcck_totals$Catch)*10^-3, "o", col = 2, bty = "l", xlab = "", ylab = "Run size (thousands)", las = 1)
# points(tbrck$Year, apply(tbrck[, c("Taku_Escapement", "Stikine_Escapement", "Alsek_Escapement")]/(1 - tbrck[, c("Taku_Rate", "Stikine_Rate", "Alsek_Rate")]), 1, sum)*10^-3, "o")
# mtext(side =3, outer = TRUE, line = -1, "Transboundary Chinook")

# Use TCCHINOOK, but add most recent year from TTC for now
tbrck_yrs <- c(tbrck$Year, 2023)
tbrck_spawners <- c(apply(tbrck[, c("Taku_Escapement", "Stikine_Escapement", "Alsek_Escapement")], 1, sum), ttcck_totals$Spawners[ttcck_totals$Year == 2023])
tbrck_run <- c(apply(tbrck[, c("Taku_Escapement", "Stikine_Escapement", "Alsek_Escapement")]/(1 - tbrck[, c("Taku_Rate", "Stikine_Rate", "Alsek_Rate")]), 1, sum), sum(ttcck_totals[ttcck_totals$Year == 2023, c("Spawners", "Catch")]))

# Reformat data for SPS
tbrck_sps <- data.frame(
	region = rep("Transboundary", length(tbrck_yrs)),
	species = rep("Chinook", length(tbrck_yrs)),
	year = tbrck_yrs,
	spawners = tbrck_spawners, 
	smoothedSpawners = NA,
	runsize = tbrck_run,
	smoothedRunsize = NA
) 

# # Catch (for comparison to TCCHINOOL Table A4)
# plot(tbrck$Year, apply( tbrck[, c("Taku_Rate", "Stikine_Rate", "Alsek_Rate")] * tbrck[, c("Taku_Escapement", "Stikine_Escapement", "Alsek_Escapement")]/(1 - tbrck[, c("Taku_Rate", "Stikine_Rate", "Alsek_Rate")]), 1, sum) * 10^-3, 'o', ylab = "Transboudnary CK Catch (thousands)", xlab = "", las = 1)
# lines(1975:2022, c(1257,1584,856,1210,3946,3193,2386,3430,3442,1986,2608,4140,4021,4615,5198,6088,5378,4771,5237,5866,6829,8407,9151,5051,6600,6428,4277,5160,6447,10343,22610,19128,16364,12641,12390,11433,10037,11116,6996,7822,7210,5121,1379,202,1815,3966,713,650)*10^-3, "o", col = 2)
# legend("topleft", pch = 1, lwd = 1, col = c(1,2), c("CTC data request (esc + expl rate)", "CTC Table A4 (Total LC, Rel. IM)"))

# Smoothing
tbrck_sps$smoothedSpawners <- genSmooth(
	abund = tbrck_sps$spawners,
	years = tbrck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Chinook"]
)

tbrck_sps$smoothedRunsize <- genSmooth(
	abund = tbrck_sps$runsize,
	years = tbrck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Chinook"]
)

plot_abund(tbrck_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrck_sps)

#------------------------------------------------------------------------------
# Transboundary: Sockeye
#------------------------------------------------------------------------------

# TBR Sockeye data are the sum of the Stikine (Appendix B21) and Taku (Appendix D15) 

tbrse_stik <- read.csv('data/tbr_sockeye_appendixB21.csv')
tbrse_taku <- read.csv('data/tbr_sockeye_appendixD15.csv')

# What is the earliest common year?
min(tbrse_stik$Year) # 1979
min(tbrse_taku$Year) # 1984

max(tbrse_stik$Year) # 2023
max(tbrse_taku$Year) # 2023

# Remove Stikine data prior to 1984
tbrse_stik <- subset(tbrse_stik, tbrse_stik$Year >= 1984)

# Reformat data for SPS
tbrse_sps <- data.frame(
	region = rep("Transboundary", dim(tbrse_stik)[1]),
	species = rep("Sockeye", dim(tbrse_stik)[1]),
	year = tbrse_stik$Year,
	spawners = tbrse_stik$Escapement.broodstock + tbrse_taku$Escapement, 
	smoothedSpawners = NA,
	runsize = tbrse_stik$Terminal.Run + tbrse_taku$Terminal.Run,
	smoothedRunsize = NA
) 

# Smoothing
tbrse_sps$smoothedSpawners <- genSmooth(
	abund = tbrse_sps$spawners,
	years = tbrse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Sockeye"]
)

tbrse_sps$smoothedRunsize <- genSmooth(
	abund = tbrse_sps$runsize,
	years = tbrse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Sockeye"]
)

plot_abund(tbrse_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrse_sps)

#------------------------------------------------------------------------------
# Transboundary: Coho
#------------------------------------------------------------------------------
tbrco <- read.csv('data/tbr_coho_appendixD20.csv')

# Check if years are continuous
unique(diff(tbrco$Year)) # Yes, all one year apart

# Reformat data for SPS
tbrco_sps <- data.frame(
	region = rep("Transboundary", dim(tbrco)[1]),
	species = rep("Coho", dim(tbrco)[1]),
	year = tbrco$Year,
	spawners = tbrco$Escape., 
	smoothedSpawners = NA,
	runsize = tbrco$Run,
	smoothedRunsize = NA
) 

# Smoothing
tbrco_sps$smoothedSpawners <- genSmooth(
	abund = tbrco_sps$spawners,
	years = tbrco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Coho"]
)

tbrco_sps$smoothedRunsize <- genSmooth(
	abund = tbrco_sps$runsize,
	years = tbrco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Coho"]
)

plot_abund(tbrco_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrco_sps)

#------------------------------------------------------------------------------
# Transboundary: Pink and chum
#------------------------------------------------------------------------------

# Monitoring of pink and chum salmon in the Transboundary region is limited, and the 
# best available information is the Canyon Island fish wheel. 

# tbrpkcm <- read.csv("data/spawner_surveys.csv", na.strings = c(-989898)) %>%
# 	subset(!is.na(stream_observed_count)) %>% # Remove stream with no data
# 	subset(stream_name_pse == "CANYON ISLAND") %>% # Subset Canyon Island data
# 	subset(species_name %in% c("Pink (even)", "Pink (odd)", "Chum")) # Use only data from 1950 to present
# 
# tbrpkcm$species_pooled <- tbrpkcm$species_name
# tbrpkcm$species_pooled[tbrpkcm$species_name %in% c("Pink (even)", "Pink (odd)")] <- "Pink"

# --- 
# Pink
# --- 

tbrpk <- read.csv("data/TTC_ManualExtract_Taku_Pink.csv") %>%
	filter(!is.na(Value))

# Check if years are continuous
unique(diff(tbrpk$Year)) # Yes, all one year apart

# Reformat data for SPS
tbrpk_sps <- data.frame(
	region = rep("Transboundary", nrow(tbrpk)), #rep("Transboundary", length(which(tbrpkcm$species_pooled == "Pink"))),
	species = rep("Pink", nrow(tbrpk)), #length(which(tbrpkcm$species_pooled == "Pink"))),
	year = tbrpk$Year, #sort(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"]),
	spawners = tbrpk$Value, # tbrpkcm$stream_observed_count[tbrpkcm$species_pooled == "Pink"][order(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"])], 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 
# Smoothing
tbrpk_sps$smoothedSpawners <- genSmooth(
	abund = tbrpk_sps$spawners,
	years = tbrpk_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Pink"]
)

plot_abund(tbrpk_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrpk_sps)

# --- 
# Chum
# --- 
tbrcm <- read.csv("data/TTC_ManualExtract_Taku_Chum.csv") %>%
	filter(!is.na(Value))

# Reformat data for SPS
tbrcm_sps <- data.frame(
	region = rep("Transboundary", nrow(tbrcm)), #length(which(tbrpkcm$species_pooled == "Chum"))),
	species = rep("Chum", nrow(tbrcm)), #length(which(tbrpkcm$species_pooled == "Chum"))),
	year = tbrcm$Year, #sort(tbrpkcm$year[tbrpkcm$species_pooled == "Chum"]),
	spawners = tbrcm$Value, #tbrpkcm$stream_observed_count[tbrpkcm$species_pooled == "Chum"][order(tbrpkcm$year[tbrpkcm$species_pooled == "Chum"])], 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 
# Smoothing
tbrcm_sps$smoothedSpawners <- genSmooth(
	abund = tbrcm_sps$spawners,
	years = tbrcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Chum"]
)

plot_abund(tbrcm_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrcm_sps)

# # --- 
# # Steelhead - do not use for now
# # --- 
# 
# tbrsh <- read.csv("data/TTC_ManualExtract_Taku_Steelhead.csv")
# 
# # Check if years are continuous
# unique(diff(tbrsh$Year)) # Yes, all one year apart
# 
# # Start timeseries at first non-NA
# tbrsh <- tbrsh[which(!is.na(tbrsh$Value))[1]:nrow(tbrsh), ]
# 
# # Reformat data for SPS
# tbrsh_sps <- data.frame(
# 	region = rep("Transboundary", nrow(tbrsh)), #rep("Transboundary", length(which(tbrpkcm$species_pooled == "Pink"))),
# 	species = rep("Steelhead", nrow(tbrsh)), #length(which(tbrpkcm$species_pooled == "Pink"))),
# 	year = tbrsh$Year, #sort(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"]),
# 	spawners = tbrsh$Value, # tbrpkcm$stream_observed_count[tbrpkcm$species_pooled == "Pink"][order(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"])], 
# 	smoothedSpawners = NA,
# 	runsize = NA,
# 	smoothedRunsize = NA
# ) 
# # Smoothing
# tbrsh_sps$smoothedSpawners <- genSmooth(
# 	abund = tbrsh_sps$spawners,
# 	years = tbrsh_sps$year,
# 	genLength = genLength$gen_length[genLength$region == "Transboundary" & genLength$species == "Steelhead"]
# )
# 
# plot_abund(tbrsh_sps)
# 
# # Add to master sps dataframe
# sps_data <- rbind(sps_data, tbrsh_sps)
###############################################################################
# Haida Gwaii
###############################################################################

# PSC Northern Boundary data for comparison
hg_table30 <- read.csv("data/TCNB-23-01_Table30_Area1escapement.csv")
names(hg_table30) <- c("year", "Sockeye", "Coho", "Pink", "Chum", "Chinook") # Change names to be consistent with species

#------------------------------------------------------------------------------
# Haida Gwaii: Chinook
#------------------------------------------------------------------------------
# No recent data, but at least show what we have for Yakoun
# These are consistent with the TCNB escapement data for Area 1 (Table 30)

hgck <- read.csv("data/spawner_surveys.csv", na.strings = c(-989898)) %>%
	subset(species_name == "Chinook" & region == "Haida Gwaii") %>%
	subset(stream_name_pse == "YAKOUN RIVER")

# There are some missing years of data; want to impute in between when smoothing
diff(hgck$year)
hgck_years <- min(hgck$year):max(hgck$year)

# Reformat data for SPS
hgck_sps <- data.frame(
	region = rep("Haida Gwaii", length(hgck_years)),
	species = rep("Chinook", length(hgck_years)),
	year = hgck_years,
	spawners = hgck$stream_observed_count[match(hgck_years, hgck$year)], 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
hgck_sps$smoothedSpawners <- genSmooth(
	abund = hgck_sps$spawners,
	years = hgck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Haida Gwaii" & genLength$species == "Chinook"]
)

plot_abund(hgck_sps)
# lines(hg_table30$year, hg_table30$Chinook*10^-3, lty = 2, col = 2)
# legend("topleft", lty = 2, col = 2, "NBTC Table 30 (Area 1) escapement")

# Add to master sps dataframe
sps_data <- rbind(sps_data, hgck_sps)


#------------------------------------------------------------------------------
# Haida Gwaii: Chum, Coho, Pink, Sockeye
#------------------------------------------------------------------------------
# Expansion from spawner surveys only; no steelhead data

# Use expanded spawner abundance -> NO, not super accurate
sp <- readRDS("output/expanded-spawners/Haida Gwaii-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in 2:5){
	hg.s <- sp[2, species[s], ]
	# hg.s <- hg.s[!is.na(hg.s)]

	# Reformat data for SPS
	hg.s_sps <- data.frame(
		region = rep("Haida Gwaii", length(hg.s)),
		species = rep(species[s], length(hg.s)),
		year = as.numeric(names(hg.s)),
		spawners = as.numeric(hg.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA
	) 
	
	# Smoothing
	hg.s_sps$smoothedSpawners <- genSmooth(
		abund = hg.s_sps$spawners,
		years = hg.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "Haida Gwaii" & genLength$species == species[s]]
	)
	
	plot_abund(hg.s_sps)
	# lines(hg_table30$year, hg_table30[, species[s]]*10^-3, col = 2)
	# legend("topleft", lty = 1, col = 2, "NBTC Table 30 (Area 1) escapement", bty = "n")
	
	# Add to master sps dataframe
	sps_data <- rbind(sps_data, hg.s_sps)
	
}	# end species

###############################################################################
# Nass
###############################################################################

#------------------------------------------------------------------------------
# Nass: Chinook
#------------------------------------------------------------------------------

# TCCHINOOK Table B3
nass_ctc <- readxl::read_xlsx("data/TCCHINOOK-24-01-Appendix-B-Escapement-Detailed.xlsx", sheet = "B3", range = "A4:D53", col_types = "numeric")

# Reformat data for SPS
nassck_sps <- data.frame(
	region = rep("Nass", dim(nass_ctc)[1]),
	species = rep("Chinook", dim(nass_ctc)[1]),
	year = nass_ctc$...1,
	spawners = nass_ctc$Esc, 
	smoothedSpawners = NA,
	runsize = nass_ctc$t.run,
	smoothedRunsize = NA
) 

# Smoothing
nassck_sps$smoothedSpawners <- genSmooth(
	abund = nassck_sps$spawners,
	years = nassck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Chinook"]
)

nassck_sps$smoothedRunsize <- genSmooth(
	abund = nassck_sps$runsize,
	years = nassck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Chinook"]
)

plot_abund(nassck_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, nassck_sps)

#------------------------------------------------------------------------------
# Nass: Sockeye
#------------------------------------------------------------------------------
# # From Fig. 1 of PSC (2023) Assessment and management frameworks of the Pacific Salmon Treaty and their robustness to environmental change
# nassse <- read.csv('data/nass_sockeye.csv')
# names(nassse) <- c("Year", "Total.Run", "TE") # Rename to be consistent with NCCDBV2 output for easy switching

# # Compare to escapement from the TCNB (23)-01
# nassse2 <- read.csv("data/TCNB-23-01_Table31_Area3escapement.csv")
# plot(nassse$year, nassse$escapement,"o")
# lines(nassse2$YEAR, nassse2$SOCKEYE, col = 2)

# Compare to NCC run reconstruction: https://github.com/LGLLimited/nccdbv2/tree/master/run/2022-nass-update/data/KarlEnglish-2023-05-29
nassse <- read.csv("data/nassskeena_sockeye_lgl.csv") %>%
subset(Region == "Nass")
# par(mfrow = c(2, 1), mar = c(3,4,2,1))
# plot(nassse$year, nassse$escapement,"o", xlim = c(1980, 2023), bty = "l", ylab = "Escapement")
# lines(nassse3$Year[nassse3$Region == "Nass"], nassse3$TE[nassse3$Region == "Nass"], "o", col = 2, pch = 19, cex = 0.6)
# legend("topright", pch = c(1, 19), col = c(1,2), pt.cex = c(1, 0.8), legend = c("SCSC figure", "NCCDB_v2 GitHub"), lty = 1, bty = "n")
# 
# plot(nassse$year, nassse$runsize,"o", xlim = c(1980, 2023), bty = "l", ylab = "Run Size")
# lines(nassse3$Year[nassse3$Region == "Nass"], nassse3$Total.Run[nassse3$Region == "Nass"], "o", col = 2, pch = 19, cex = 0.6)
# mtext(side = 3, line = -1, outer= TRUE, 'Nass sockeye')

# Check if years are continuous
# unique(diff(nassse$Year)) # Yes, all one year apart

# Reformat data for SPS
nassse_sps <- data.frame(
	region = rep("Nass", dim(nassse)[1]),
	species = rep("Sockeye", dim(nassse)[1]),
	year = nassse$Year,
	spawners = nassse$TE, 
	smoothedSpawners = NA,
	runsize = nassse$Total.Run,
	smoothedRunsize = NA
) 

# Smoothing
nassse_sps$smoothedSpawners <- genSmooth(
	abund = nassse_sps$spawners,
	years = nassse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Sockeye"]
)

nassse_sps$smoothedRunsize <- genSmooth(
	abund = nassse_sps$runsize,
	years = nassse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Sockeye"]
)

plot_abund(nassse_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, nassse_sps)

#------------------------------------------------------------------------------
# Nass: Steelhead
#------------------------------------------------------------------------------
# Use CU-level Nass summer abundance which is a better region-scale indicator
nasssh <- read.csv("data/spawner_abundance.csv") %>% 
	filter(region == "Nass", species_name == "Steelhead", cu_name_pse == "Nass Summer", !is.na(estimated_count))

# Reformat data for SPS
nasssh_sps <- data.frame(
	region = rep("Nass", dim(nasssh)[1]),
	species = rep("Steelhead", dim(nasssh)[1]),
	year = nasssh$year,
	spawners = as.numeric(nasssh$estimated_count), 
	smoothedSpawners = NA,
	runsize = as.numeric(nasssh$total_run),
	smoothedRunsize = NA
) 

# Smoothing
nasssh_sps$smoothedSpawners <- genSmooth(
	abund = nasssh_sps$spawners,
	years = nasssh_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Steelhead"]
)

nasssh_sps$smoothedRunsize <- genSmooth(
	abund = nasssh_sps$runsize,
	years = nasssh_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Steelhead"]
)

plot_abund(nasssh_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, nasssh_sps)


#------------------------------------------------------------------------------
# Nass: Coho
#------------------------------------------------------------------------------

# Read in LGL data
nassco_lgl <- readxl::read_xlsx("data/LGL_nass_update_TRTC_age.xlsx", sheet = "ConservationUnit TRTC") %>%
	filter(SpeciesId == "CO" & Year >= 1992)

nassco <- nassco_lgl %>%
	group_by(Year) %>%
	summarise(escapement = sum(TE), runsize = sum(`Total Run`))

# CHeck year difference is 1
if(unique(diff(nassco$Year)) != 1){
	stop("Nass coho years not continuous.")
}

# Reformat data for SPS
nassco_sps <- data.frame(
	region = rep("Nass", dim(nassco)[1]),
	species = rep("Coho", dim(nassco)[1]),
	year = nassco$Year,
	spawners = nassco$escapement, 
	smoothedSpawners = NA,
	runsize = nassco$runsize,
	smoothedRunsize = NA
) 

# Smoothing
nassco_sps$smoothedSpawners <- genSmooth(
	abund = nassco_sps$spawners,
	years = nassco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Coho"]
)

nassco_sps$smoothedRunsize <- genSmooth(
	abund = nassco_sps$runsize,
	years = nassco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Coho"]
)

plot_abund(nassco_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, nassco_sps)

#------------------------------------------------------------------------------
# Nass: Pink, Chum
#------------------------------------------------------------------------------

# Use expanded spawner abundance
sp <- readRDS("output/expanded-spawners/Nass-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in c(2,4)){
	nass.s <- sp[2, species[s], ]
	
	# Reformat data for SPS
	nass.s_sps <- data.frame(
		region = rep("Nass", length(nass.s)),
		species = rep(species[s], length(nass.s)),
		year = as.numeric(names(nass.s)),
		spawners = as.numeric(nass.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA
	) 
	
	# Smoothing
	nass.s_sps$smoothedSpawners <- genSmooth(
		abund = nass.s_sps$spawners,
		years = nass.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == species[s]]
	)
	
	plot_abund(nass.s_sps)

	# Add to master sps dataframe
	sps_data <- rbind(sps_data, nass.s_sps)
	
}	

###############################################################################
# Skeena
###############################################################################

#------------------------------------------------------------------------------
# Skeena: Chinook
#------------------------------------------------------------------------------

# TCCHINOOK Table B3
sk_ctc <- readxl::read_xlsx("data/TCCHINOOK-24-01-Appendix-B-Escapement-Detailed.xlsx", sheet = "B3", range = "A4:G53", col_types = "numeric")

# # TCNB Table 32
# tcnb_32 <-read.csv("data/TCNB-23-01_Table32_Area4escapement.csv")
# 
# # There is Total Esc and GSI esc...
# plot(as.numeric(names(skeena.s)), as.numeric(skeena.s)*10^-3, "o", las = 1, ylab = "Spawner abundance (thousands)", xlab = "", ylim = c(0, 100), bty = "l", xpd = NA, cex = 0.5, pch = 19)
# lines(sk_ctc$...1, sk_ctc$`Total Esc`*10^-3, "o", col = 2, cex = 0.8, pch = 19, lwd = 1.5)
# lines(sk_ctc$...1, sk_ctc$`GSI3 esc`*10^-3, "o", col = 4, cex = 0.5, pch = 19)
# lines(tcnb_32$YEAR, tcnb_32$CHINOOK*10^-3, "o", col = 3, pch = 19, cex = 0.5)
# legend("topleft", pch = 19, pt.cex = c(0.5, 0.8, 0.5, 0.5), lwd = c(1, 1.5, 1, 1), col = c(1,2,4, 3), c("Expanded", "Table B3: Total Esc", "Table B3: GSI esc", "TCNB Table 31 Area 4 escapement"))

# Reformat data for SPS
skck_sps <- data.frame(
	region = rep("Skeena", dim(sk_ctc)[1]),
	species = rep("Chinook", dim(sk_ctc)[1]),
	year = sk_ctc$...1,
	spawners = sk_ctc$`GSI esc3`, 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
skck_sps$smoothedSpawners <- genSmooth(
	abund = skck_sps$spawners,
	years = skck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == "Chinook"]
)

plot_abund(skck_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, skck_sps)

#------------------------------------------------------------------------------
# Skeena: Sockeye
#------------------------------------------------------------------------------
# From Fig. 1 of PSC (2023) Assessment and management frameworks of the Pacific Salmon Treaty and their robustness to environmental change
# skse <- read.csv('data/skeena_sockeye.csv')

# Compare to NCC run reconstruction: https://github.com/LGLLimited/nccdbv2/tree/master/run/2022-nass-update/data/KarlEnglish-2023-05-29
skse <- read.csv("data/nassskeena_sockeye_lgl.csv") %>%
	subset(Region == "Skeena")
# par(mfrow = c(2, 1), mar = c(3,4,2,1))
# plot(skse$year, skse$escapement,"o", xlim = c(1960, 2023), bty = "l", ylab = "Escapement", ylim = c(0, max(nassse3$TE[nassse3$Region == "Skeena"])))
# lines(nassse3$Year[nassse3$Region == "Skeena"], nassse3$TE[nassse3$Region == "Skeena"], "o", col = 2, pch = 19, cex = 0.6)
# legend("topleft", pch = c(1, 19), col = c(1,2), pt.cex = c(1, 0.8), legend = c("SCSC figure", "NCCDB_v2 GitHub"), lty = 1, bty = "n")
# 
# plot(skse$year, skse$runsize,"o", xlim = c(1960, 2023), bty = "l", ylab = "Run Size")
# lines(nassse3$Year[nassse3$Region == "Skeena"], nassse3$Total.Run[nassse3$Region == "Skeena"], "o", col = 2, pch = 19, cex = 0.6)
# mtext(side = 3, line = -1, outer= TRUE, 'Skeena sockeye')

# Check if years are continuous
unique(diff(skse$Year)) # Yes, all one year apart

# Reformat data for SPS
skse_sps <- data.frame(
	region = rep("Skeena", dim(skse)[1]),
	species = rep("Sockeye", dim(skse)[1]),
	year = skse$Year,
	spawners = skse$TE, 
	smoothedSpawners = NA,
	runsize = skse$Total.Run,
	smoothedRunsize = NA
) 

# Smoothing
skse_sps$smoothedSpawners <- genSmooth(
	abund = skse_sps$spawners,
	years = skse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == "Sockeye"]
)

skse_sps$smoothedRunsize <- genSmooth(
	abund = skse_sps$runsize,
	years = skse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == "Sockeye"]
)

plot_abund(skse_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, skse_sps)

#------------------------------------------------------------------------------
# Skeena: Steelhead
#------------------------------------------------------------------------------

# Use Skeena steelhead run size from BC Updates
# sksh <- read.csv("data/Steelhead_total_runsize.csv")
sksh <- read.csv("data/SkeenaSteelhead_1956-2023.csv")


# Reformat data for SPS
sksh_sps <- data.frame(
	region = rep("Skeena", dim(sksh)[1]),
	species = rep("Steelhead", dim(sksh)[1]),
	year = sksh$Year,
	spawners = sksh$TyeeEscapement, 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
sksh_sps$smoothedSpawners <- genSmooth(
	abund = sksh_sps$spawners,
	years = sksh_sps$year,
	genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == "Steelhead"]
)

plot_abund(sksh_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, sksh_sps)

#------------------------------------------------------------------------------
# Skeena: Chum
#------------------------------------------------------------------------------

sk_table32 <- read.csv("data/TCNB-23-01_Table32_Area4escapement.csv")

# Reformat data for SPS
skcm_sps <- data.frame(
	region = rep("Skeena", dim(sk_table32)[1]),
	species = rep("Chum", dim(sk_table32)[1]),
	year = sk_table32$YEAR,
	spawners = sk_table32$CHUM, 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
skcm_sps$smoothedSpawners <- genSmooth(
	abund = skcm_sps$spawners,
	years = skcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == "Chum"]
)

plot_abund(skcm_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, skcm_sps)

#------------------------------------------------------------------------------
# Skeena: Pink and Coho
#------------------------------------------------------------------------------

# Use expanded spawner abundance -> NO, not super accurate
sp <- readRDS("output/expanded-spawners/Skeena-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in c(3, 4)){
	skeena.s <- sp[2, species[s], ]
	skeena.s <- skeena.s[!is.na(skeena.s)]
	
	# Reformat data for SPS
	skeena.s_sps <- data.frame(
		region = rep("Skeena", length(skeena.s)),
		species = rep(species[s], length(skeena.s)),
		year = as.numeric(names(skeena.s)),
		spawners = as.numeric(skeena.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA
	) 
	
	# Smoothing
	skeena.s_sps$smoothedSpawners <- genSmooth(
		abund = skeena.s_sps$spawners,
		years = skeena.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == species[s]]
	)
	
	plot_abund(skeena.s_sps)
	
	# Add to master sps dataframe
	sps_data <- rbind(sps_data, skeena.s_sps)
	
}	# end species

###############################################################################
# Central Coast
###############################################################################

# Expansion from spawner surveys only; no steelhead data
sp <- readRDS("output/expanded-spawners/Central Coast-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in 1:5){
	cc.s <- sp[2, species[s], ]
	cc.s <- cc.s[!is.na(cc.s)]
	
	# Reformat data for SPS
	cc.s_sps <- data.frame(
		region = rep("Central Coast", length(cc.s)),
		species = rep(species[s], length(cc.s)),
		year = as.numeric(names(cc.s)),
		spawners = as.numeric(cc.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA
	) 
	
	# Smoothing
	cc.s_sps$smoothedSpawners <- genSmooth(
		abund = cc.s_sps$spawners,
		years = cc.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "Central Coast" & genLength$species == species[s]]
	)
	
	plot_abund(cc.s_sps)
	
	# Add to master sps dataframe
	sps_data <- rbind(sps_data, cc.s_sps)
	
}	# end species

###############################################################################
# Vancouver Island & Mainland Inlets
###############################################################################

# #------------------------------------------------------------------------------
# # Chinook - leave as expansion for now Apr 9, 2024
# #------------------------------------------------------------------------------
# 
# # (1) CTC data
# vimi_ctc_esc1 <- readxl::read_xlsx("data/TCCHINOOK-23-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B4", range = "A4:G52", col_types = "numeric") # Southern BC
# vimi_ctc_esc2 <- readxl::read_xlsx("data/TCCHINOOK-23-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B5", range = "A3:K51", col_types = "numeric") # Vancouver Island
# 
# vimi_ctc_esc <- data.frame(
# 	year = vimi_ctc_esc1$...1,
# 	esc_SBC = apply(vimi_ctc_esc1[, grep("Esc", names(vimi_ctc_esc1))], 1, sum),
# 	esc_VI = apply(vimi_ctc_esc2[, grep("Index", names(vimi_ctc_esc2))], 1, sum, na.rm = TRUE) # Ignore missing data; small  contribution to overall sum
# 	)
# 
# # Remove 1975-1978, when SBC wasn't monitored
# vimi_ctc_esc <- vimi_ctc_esc[!is.na(vimi_ctc_esc$esc_SBC), ]
# 
# # Catch	
# vimi_ctc_catch <-  readxl::read_xlsx("data/TCCHINOOK-23-02-Appendix-A-Catch-Detailed.xlsx", sheet = "A14", range = "K4:M52") %>%
# 	mutate(year = 1975:2022)
# 
# ctc_catch_sum <- apply(ctc_catch[, 1:3], 1, sum)
# ctc_run <- ctc_esc_sum + ctc_catch_sum
# 
# plot(yrs, vimi.s*10^-6, "o", las = 1, ylab = "Spawners (millions)", ylim = c(0, 0.3))
# lines(vimi_ctc_esc$year, apply(vimi_ctc_esc[, 2:3], 1, sum)*10^-6, "o", col = 2)
# abline(v = seq(1950, 2023, 2), lty = 3, col = grey(0.6))
# legend("topleft", pch = 1, lwd = 1, col = c(1,2), c("Expansion", "Sum of index from TCCHINOOK Table B4,B5"))
#------------------------------------------------------------------------------
# Other (including steelhead)
#------------------------------------------------------------------------------

# Expansion from spawner surveys; including steelhead (incl. Cheakamus)
sp <- readRDS("output/expanded-spawners/Vancouver Island & Mainland Inlets-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in 1:6){
	vimi.s <- sp[2, species[s], ]
	# Get rid of NAs (esp in 2022)
	vimi.s <- vimi.s[1:max(which(!is.na(vimi.s)))]
	
	# For all VIMI species, start in 1953 - earlier expansion factors are huge
	vimi.s	<- vimi.s[which(as.numeric(names(vimi.s)) >= 1953)]
	
	# Reformat data for SPS
	vimi.s_sps <- data.frame(
		region = rep("Vancouver Island & Mainland Inlets", length(vimi.s)),
		species = rep(species[s], length(vimi.s)),
		year = as.numeric(names(vimi.s)),
		spawners = as.numeric(vimi.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA
	) 
	
	# Smoothing
	vimi.s_sps$smoothedSpawners <- genSmooth(
		abund = vimi.s_sps$spawners,
		years = vimi.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "Vancouver Island & Mainland Inlets" & genLength$species == species[s]]
	)
	
	plot_abund(vimi.s_sps)
	
	# Add to master sps dataframe
	sps_data <- rbind(sps_data, vimi.s_sps)
	
}	# end species


###############################################################################
# Fraser
###############################################################################

#------------------------------------------------------------------------------
# Chinook
#------------------------------------------------------------------------------
# # (1) Use expanded spawner surveys from 1984 onward
# sp <- readRDS("output/expanded-spawners/Fraser-spawners.rds")
# yrs <- c(1984:max(as.numeric(dimnames(sp)[[3]])))
# 
# # (2) Use Atlas et al. (2023) Fraser populations for run size
# frck_run0 <- read.csv("data/Atlas2023/CK_TotalRun_FINAL.csv") %>% 
# 	subset(group == "salish") %>% # select Salish group
# 	subset(grepl("skagit", population) == FALSE & grepl("cowichan", population) == FALSE) # Remove VIMI populations
# 
# frck_run <- tapply(frck_run0$tot_run, frck_run0$year, sum)

# (3) CTC data
ctc_esc <- readxl::read_xlsx("data/TCCHINOOK-24-01-Appendix-B-Escapement-Detailed.xlsx", sheet = "B6", range = "A3:P52", col_types = "numeric")
ctc_esc_sum <- apply(ctc_esc[, grep("Esc", names(ctc_esc))], 1, sum)

# ctc_catch <-  readxl::read_xlsx("data/TCCHINOOK-23-02-Appendix-A-Catch-Detailed.xlsx", sheet = "A14", range = "K4:M52") %>%
# 	mutate(year = 1975:2022)
# 
# ctc_catch_sum <- apply(ctc_catch[, 1:3], 1, sum)
# ctc_run <- ctc_esc_sum + ctc_catch_sum
	
# # Compare to CTC data# Compare to CTC datasum
# plot(as.numeric(names(frck_run)), frck_run*10^-6, "o", pch = 19, las = 1, ylim = c(0, 1), xlim = c(1980, 2023), lwd = 1.5, ylab = "Abundance (millions)", xlab = "")
# abline(v = seq(1980, 2023, 2), lty = 3, col = grey(0.6))
# points(1975:2022, ctc_run*10^-6, "o", col = 2, lwd = 1.5, pch = 19)
# points(yrs, sp[2, "Chinook", match(yrs, as.numeric(dimnames(sp)[[3]]))]*10^-6, "o",cex = 0.6)
# points(1975:2022, ctc_esc_sum*10^-6, "o", col = 2, cex = 0.6)
# legend("topleft", ncol = 2, pch = c(19, 1, 19, 1), pt.cex = c(1, 0.6, 1, 0.6), col = c(1,1,2,2), c("Atlas (2023)", "Expansion", "TCCHINOOK Table A14 - sum + Esc from below", "TCCHINOOK Table B6 - sum of Esc."))

yrs <- ctc_esc$Year[!is.na(ctc_esc_sum)]
# Put in SPS format
frck_sps <- data.frame(
	region = rep("Fraser", length(yrs)),
	species = rep("Chinook", length(yrs)),
	year = yrs,
	spawners = ctc_esc_sum[!is.na(ctc_esc_sum)], # sp[2, "Chinook", match(yrs, as.numeric(dimnames(sp)[[3]]))], 
	smoothedSpawners = NA,
	runsize = NA, #ctc_run[!is.na(ctc_run)], # frck_run[match(yrs, as.numeric(names(frck_run)))],
	smoothedRunsize = NA
) 
	
	# Smoothing
frck_sps$smoothedSpawners <- genSmooth(
		abund = frck_sps$spawners,
		years = frck_sps$year,
		genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Chinook"]
	)

# frck_sps$smoothedRunsize <- genSmooth(
# 	abund = frck_sps$runsize,
# 	years = frck_sps$year,
# 	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Chinook"]
# )

plot_abund(frck_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frck_sps)

#------------------------------------------------------------------------------
# Chum
#------------------------------------------------------------------------------
# # (1) Use expanded spawner surveys from 1953 onward
# sp <- readRDS("output/expanded-spawners/Fraser-spawners.rds")
# yrs <- as.numeric(dimnames(sp)[[3]])
# yrs <- 1953:max(yrs[!is.na(sp[2, "Chum", ])])

# (2) Use TCCHUM Fraser data
frcm_esc <- readxl::read_xlsx("data/ChumTC Report Tables - full time series up to 2022.xlsx", sheet = "3-11", range = "A4:B29") %>%
	rename(year = "...1", esc = "Fraser River")

frcm_catch1 <- readxl::read_xlsx("data/ChumTC Report Tables - full time series up to 2022.xlsx", sheet = "3-8", range = "A3:G30") %>%
	rename(year = "Year", catch = "Total") %>%
	select(year, catch)


frcm_catch2 <- readxl::read_xlsx("data/ChumTC Report Tables - full time series up to 2022.xlsx", sheet = "3-9", range = "A4:G31") %>%
	rename(year = "...1", catch = "Total") %>%
	select(year, catch)

# Find years when catch and escapement are reported
yrs_tcchum <- c(max(c(min(frcm_esc$year), min(frcm_catch1$year), min(frcm_catch2$year))):min(c(max(frcm_esc$year), max(frcm_catch1$year), max(frcm_catch2$year)))) #1998:2022

frcm_run <- frcm_esc$esc[match(yrs_tcchum, frcm_esc$year)] + frcm_catch1$catch[match(yrs_tcchum, frcm_catch1$year)] + frcm_catch2$catch[match(yrs_tcchum, frcm_catch2$year)] 
	
# # Compare
# plot(yrs, sp[2, "Chum", match(yrs, as.numeric(dimnames(sp)[[3]]))]*10^-6, "o", pch = 19, bty = "l", xlab = "", ylab = "Escapement (millions)", las = 1, main = "Escapement of Fraser Chum")
# abline(v = seq(1950, 2020, 5), col = "#00000030")
# points(yrs_tcchum, frcm_esc$esc*10^-6, "o", col = 2, pch = 19, cex = 0.8, xpd = NA)
# # polygon(x = c(yrs_tcchum, rev(yrs_tcchum)),
# # 				y = c(frcm_esc$esc, rev(frcm_run))*10^-6,
# # 				col = "#FF000040", 
# # 				border = NA)
# legend("topleft", pch = c(19, 19, 21), pt.cex = c(1, 0.8, 0.8), col = c(1,2,2), pt.bg = c(NA, NA, "white"), c("PSF-expanded from NuSEDS indicator streams", "TCCHUM (23)-01 Table 3-11", "TCCHUM esc + catch"), lwd = 1)
# 
# points(yrs_tcchum, frcm_run*10^-6, "o", col = 2, pch = 21, bg = "white", cex = 0.8, xpd = NA)
# 
# (frcm_run - frcm_esc)/frcm_run

# # Plot catch
# plot(frcm_catch1$year, frcm_catch1$catch*10^-6, "o", pch = 21, bg = "white", xlab = "", ylab = "Commercial and FN catch (millions)", main = "TCCHUM Table 3-8 Fraser Catch", las = 1)
# lines(frcm_catch2$year, frcm_catch1$catch + frcm_catch2$catch , lty = 2)


# Put in SPS format
frcm_sps <- data.frame(
	region = rep("Fraser", length(yrs_tcchum)), 
	species = rep("Chum", length(yrs_tcchum)), 
	year = yrs_tcchum, 
	spawners = frcm_esc$esc, #sp[2, "Chum", match(yrs, as.numeric(dimnames(sp)[[3]]))], 
	smoothedSpawners = NA,
	runsize = frcm_run, #frcm_run,
	smoothedRunsize = NA
) 

# Smoothing
frcm_sps$smoothedSpawners <- genSmooth(
	abund = frcm_sps$spawners,
	years = frcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Chum"]
)

frcm_sps$smoothedRunsize <- genSmooth(
	abund = frcm_sps$runsize,
	years = frcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Chum"]
)

plot_abund(frcm_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frcm_sps)


#------------------------------------------------------------------------------
# Coho
#------------------------------------------------------------------------------

# Interior Fraser Coho data shared by Marissa.Glavas@dfo-mpo.gc.ca on data request
frco <- read.csv("data/E. Hertz - IFC Data.csv")
yrs <- sort(unique(frco$ReturnYear))

# Put in SPS format
frco_sps <- data.frame(
	region = rep("Fraser", length(yrs)),
	species = rep("Coho", length(yrs)),
	year = yrs,
	spawners = tapply(frco$Total.Return, frco$ReturnYear, sum), # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = tapply(frco$Total.Prefishery.Abundance, frco$ReturnYear, sum),
	smoothedRunsize = NA
) 

# Smoothing
frco_sps$smoothedSpawners <- genSmooth(
	abund = frco_sps$spawners,
	years = frco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Coho"]
)

frco_sps$smoothedRunsize <- genSmooth(
	abund = frco_sps$runsize,
	years = frco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Coho"]
)

plot_abund(frco_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frco_sps)

#------------------------------------------------------------------------------
# Pink
#------------------------------------------------------------------------------

# From PSC (updated to include)
frpk <- read.csv("data/pink_run_size_2024_04_05.csv")

# Put in SPS format
frpk_sps <- data.frame(
	region = rep("Fraser", length(frpk$Year)),
	species = rep("Pink", length(frpk$Year)),
	year = frpk$Year,
	spawners = frpk$Escapement, # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = frpk$Run.Size,
	smoothedRunsize = NA
) 

# Smoothing - don't do this for pink salmon in the Fraser - no even year data!
frpk_sps$smoothedSpawners <- frpk_sps$spawners
frpk_sps$smoothedRunsize <- frpk_sps$runsize

plot_abund(frpk_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frpk_sps)

#------------------------------------------------------------------------------
# Sockeye
#------------------------------------------------------------------------------

# From PSC
frse <- read.csv("data/Total Fraser_run_size_2024_04_05.csv")

# Put in SPS format
frse_sps <- data.frame(
	region = rep("Fraser", length(frse$Year)),
	species = rep("Sockeye", length(frse$Year)),
	year = frse$Year,
	spawners = frse$Spawning.Escapement, # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = frse$Run.Size,
	smoothedRunsize = NA
) 

# Fill in run size with in-season estimates if available
if(sum(is.na(frse_sps$runsize)) > 0){
	if(!is.na(frse$In.season.Run.Size[which(frse$Year %in% frse_sps$year[is.na(frse_sps$runsize)])])){
		frse_sps$runsize[which(frse$Year %in% frse_sps$year[is.na(frse_sps$runsize)])] <- frse$In.season.Run.Size[which(frse$Year %in% frse_sps$year[is.na(frse_sps$runsize)])]
	}
}

# Smoothing
frse_sps$smoothedSpawners <- genSmooth(
	abund = frse_sps$spawners,
	years = frse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Sockeye"]
)

frse_sps$smoothedRunsize <- genSmooth(
	abund = frse_sps$runsize,
	years = frse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Sockeye"]
)

plot_abund(frse_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frse_sps)

#------------------------------------------------------------------------------
# Steelhead
#------------------------------------------------------------------------------

# Use sum of two main Fraser steelhead CUs
frsh <- read.csv("data/spawner_abundance.csv") %>% 
	filter(region == "Fraser", species_name == "Steelhead") %>%
	filter(cu_name_pse %in% c("Thompson Summer", "Mid Fraser Summer")) %>%
	filter(!is.na(estimated_count))

frsh_sum <- tapply(as.numeric(frsh$estimated_count), frsh$year, sum)
frsh_sum <- frsh_sum[min(which(!is.na(frsh_sum))): max(which(!is.na(frsh_sum)))]

# Put in SPS format
frsh_sps <- data.frame(
	region = rep("Fraser", length(frsh_sum)),
	species = rep("Steelhead", length(frsh_sum)),
	year = as.numeric(names(frsh_sum)),
	spawners = frsh_sum, # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
frsh_sps$smoothedSpawners <- genSmooth(
	abund = frsh_sps$spawners,
	years = frsh_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Steelhead"]
)


plot_abund(frsh_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, frsh_sps)

###############################################################################
# Columbia
###############################################################################

#------------------------------------------------------------------------------
# Chinook
#------------------------------------------------------------------------------
colck <- read.csv("data/spawner_abundance.csv") %>% 
	filter(region == "Columbia", species_name == "Chinook") %>%
	filter(!is.na(estimated_count))

# Put in SPS format
colck_sps <- data.frame(
	region = rep("Columbia", length(colck$year)),
	species = rep("Chinook", length(colck$year)),
	year = colck$year,
	spawners = as.numeric(colck$estimated_count), # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
colck_sps$smoothedSpawners <- genSmooth(
	abund = colck_sps$spawners,
	years = colck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Columbia" & genLength$species == "Chinook"]
)


plot_abund(colck_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, colck_sps)

#------------------------------------------------------------------------------
# Sockeye
#------------------------------------------------------------------------------
colse <- read.csv("data/spawner_abundance.csv") %>% 
	subset(region == "Columbia" &  species_name == "Lake sockeye") %>%
	subset(!is.na(estimated_count))

# Put in SPS format
colse_sps <- data.frame(
	region = rep("Columbia", length(colse$year)),
	species = rep("Sockeye", length(colse$year)),
	year = colse$year,
	spawners = as.numeric(colse$estimated_count), # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
colse_sps$smoothedSpawners <- genSmooth(
	abund = colse_sps$spawners,
	years = colse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Columbia" & genLength$species == "Sockeye"]
)


plot_abund(colse_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, colse_sps)

#------------------------------------------------------------------------------
# Columbia: Steelhead
#------------------------------------------------------------------------------
colsh <- read.csv("data/Columbia_Steelhead_OBMEP.csv")

# Put in SPS format
colsh_sps <- data.frame(
	region = rep("Columbia", length(colsh$year)),
	species = rep("Steelhead", length(colsh$year)),
	year = colsh$year,
	spawners = colsh$natural_spawners, # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA
) 

# Smoothing
colsh_sps$smoothedSpawners <- genSmooth(
	abund = colsh_sps$spawners,
	years = colsh_sps$year,
	genLength = genLength$gen_length[genLength$region == "Columbia" & genLength$species == "Steelhead"]
)


plot_abund(colsh_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, colsh_sps)


dev.off()


###############################################################################
###############################################################################
# Write to .csv
###############################################################################
###############################################################################

write.csv(sps_data, "output/sps-data.csv", row.names = FALSE) # Always have most recent
write.csv(sps_data, paste0("output/archive/sps-data_", Sys.Date(), ".csv"), row.names = FALSE) # Archive with date
