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
library(readxl)

source("code/functions.R")

regions <- c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "East Vancouver Island & Mainland Inlets", "West Vancouver Island", "Fraser", "Columbia")

species <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")

# Generation length by species and region (for smoothing)
genLength <- read.csv("data/gen_length_regions.csv") 

# Set version of NuSEDS for source_id
nuseds_id <- "NuSEDS_20250221"
ctc_id <- "CTC_20250714"

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
ytck <- read.csv('data/yukon_chinook_appendixB11.csv')

# Note: Use Run Reconstruction (RR) estimates of spawning escapement and total
# run size, not "Historical" or Mark-Recapture estimates

# Check if years are continuous
unique(diff(ytck$Year)) # Yes, all one year apart

# Reformat data for SPS
ytck_sps <- data.frame(
	region = rep("Yukon", dim(ytck)[1]),
	species = rep("Chinook", dim(ytck)[1]),
	year = ytck$Year,
	spawners = ytck$RR.Spawning.escapement.estimate, 
	smoothedSpawners = NA,
	runsize = ytck$RR.Canadian.origin.total.run.size.estimate,
	smoothedRunsize = NA,
	source_id = rep("JTC_202503", dim(ytck)[1])
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

ytcm_B16 <- read.csv('data/yukon_chum_appendixB16.csv') # Historical data
names(ytcm_B16)[1] <- "Year"
ytcm_B20 <- read.csv('data/yukon_chum_appendixB20.csv') # Most recent estimates


# Compare data from two appendices; note that spawning escapement is sometimes different
plot(ytcm_B16$Year, ytcm_B16$Spawning.escapement.estimate*10^-3, "o", ylab = "Spawning escapement (thousands)", las = 1, bty = "l", xlab = "", pch = 21, bg = "white", main = "Canadian-origin chum salmon")
abline(v = seq(1970, 2024, 2), col = "#00000060", lwd = 0.8, lty = 3)
points(ytcm_B20$Year, ytcm_B20$Spawning.escapement*10^-3, col = 2, pch = 19, cex = 0.6)
legend("topleft", pch= c(1, 19), pt.cex = c(1, 0.6), col = c(1,2), c("Appendix B16", "Appendix B20"))

# Check if years are continuous
unique(diff(ytcm_B16$Year)) # Yes, all one year apart
unique(diff(ytcm_B20$Year)) # Yes, all one year apart

# Reformat data for SPS
ytcm_sps <- data.frame(
	region = rep("Yukon", dim(ytcm_B16)[1]),
	species = rep("Chum", dim(ytcm_B16)[1]),
	year = ytcm_B16$Year,
	spawners = ytcm_B16$Spawning.escapement.estimate, 
	smoothedSpawners = NA,
	runsize = ytcm_B20$Total.estimated.Canadian.origin.run.size[match(ytcm_B16$Year, ytcm_B20$Year)],
	smoothedRunsize = NA,
	source_id = rep("JTC_202503", dim(ytcm_B16)[1])
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
# tbrck <- read.csv('data/tbr_chinook.csv')
# # Remove 1975 -> missing Alsek data
# tbrck <- tbrck[!is.na(tbrck$Alsek_Escapement), ]

tbrck_raw <- read.csv("data/CTC_Synoptic_evaluation_data_all_2025-04-16.csv") %>%
	filter(Year > 1975) %>% # Remove 1975 -> missing Alsek data
	filter(Stock %in% c("Alsek", "Taku", "Stikine")) %>%
	dplyr::select(Stock, Year, Escapement, RateType, Rate) %>%
	mutate(TotalRun = Escapement/(1 - Rate))

# average exploitation rate in last 5 years for Alsek
mean(tbrck_raw$Rate[tbrck_raw$Stock == "Alsek" & tbrck_raw$Year %in% c(2019:2023)])

# Fill in 2024 from Post-Season review
tbrck_raw %>% filter(Year >= 2020) %>%
	mutate(catch = TotalRun * Rate)


tbrck_esc <- readxl::read_xlsx("data/TCCHINOOK-25-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B2", range = "A5:G53", col_names = c("year", "Alsek_esc", "Alsek_cv", "Taku_esc", "Taku_cv", "Stikine_esc", "Stikine_cv"))
# Ignore 1975 because not available for Alsek

# # Spawner abundance
# plot(tbrck_esc$year, tbrck_esc$Alsek_esc, col = 2, ylim = c(0, max(tbrck_raw$Escapement, na.rm = TRUE)), "o", pch = 21, bg = "white", cex = 0.8)
# points(tbrck_esc$year, tbrck_esc$Taku_esc, "o", col = 3, pch = 21, bg = "white", cex = 0.8)
# points(tbrck_esc$year, tbrck_esc$Stikine_esc, "o", col = 4, pch = 21, bg = "white", cex = 0.8)
# for(i in 1:3) points(tbrck_raw$Year[tbrck_raw$Stock == c("Alsek", "Taku", "Stikine")[i]], tbrck_raw$Escapement[tbrck_raw$Stock == c("Alsek", "Taku", "Stikine")[i]], col = i+1, pch = 4)

#Catch - WAIT! Appendix A does not include US catch of TBR stocks...this is under the TTC
# tbrck_catch <- readxl::read_xlsx("data/TCCHINOOK-25-02-Appendix-A-Catch-Detailed.xlsx", sheet = "A4", range = "K5:M54", col_names = c("landed_catch", "releases", "incidental_mortality")) %>%
	# mutate(year = 1975:2024) %>%
	# mutate(total_mortality = landed_catch + incidental_mortality)

# For 2024, take harvest from TTC B12, D8, Alsek harvest provided by email (Teresa Wallace, DFO)
tbrck <- tbrck_raw %>%
	group_by(Year) %>%
	summarise(Escapement_all = sum(Escapement),
						TotalRun_all = sum(TotalRun), 
						source_id = "PSC_20250416") %>%
	bind_rows(data.frame(Year = 2024,
						Escapement_all = (tbrck_esc$Alsek_esc + tbrck_esc$Taku_esc + tbrck_esc$Stikine_esc)[tbrck_esc$year == 2024],
						TotalRun_all = 717 + (tbrck_esc$Alsek_esc + tbrck_esc$Taku_esc + tbrck_esc$Stikine_esc)[tbrck_esc$year == 2024], source_id = "Foos_20250415"))  

# plot(tbrck$Year, tbrck$TotalRun_all*10^-3, "o", pch = 19, bty= "l", xlab = "", ylab = "Total abundance (thousands)", ylim = c(0, 200))
# for(i in 1:3) points(tbrck_raw$Year[tbrck_raw$Stock == c("Alsek", "Taku", "Stikine")[i]], tbrck_raw$TotalRun[tbrck_raw$Stock == c("Alsek", "Taku", "Stikine")[i]]*10^-3, col = i+1, pch = 4)
# points(tbrck$Year, tbrck$Escapement_all*10^-3, "o", pch = 21, bg = "white")
# 

# # (2) TTC data
# Prelimnary from aaron Foos with some assumptions suggests 2024 catch for all TBR Rivers is ~693


# stikine terminal run 9,921
# ttcck <- read.csv("data/TTC_MERGED.csv") %>%
# 	filter(SPECIES == "Chinook", TableSource %in% c("E.7_full", "B.12", "D.7"))
# 
# ttcck_totals <- ttcck %>%
# 	filter(Series %in% c("Alsek River - Escapement", "Escapement", "Spawning Escapement")) %>%
# 	group_by(Year) %>%
# 	summarise(Spawners = sum(Value))
# ttcck_totals <- ttcck_totals %>% left_join(
# 	ttcck %>%
# 		filter(Series %in% c("Alsek River - Canada harvest", "Alsek River - Harvest Dry Bay", "Canadian Harvest", "US Harvest", "Canadian Catch", "US Harvest")) %>%
# 		group_by(Year) %>%
# 		summarise(Catch = sum(Value))
# )
# 
# 
# # Check if years are continuous
# unique(diff(ttcck_totals$Year)) # Yes, all one year apart
# 
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
# 
# # Use TCCHINOOK, but add most recent year from TTC for now
# tbrck_yrs <- c(tbrck$Year, 2023)
# tbrck_spawners <- c(apply(tbrck[, c("Taku_Escapement", "Stikine_Escapement", "Alsek_Escapement")], 1, sum), ttcck_totals$Spawners[ttcck_totals$Year == 2023])
# tbrck_run <- c(apply(tbrck[, c("Taku_Escapement", "Stikine_Escapement", "Alsek_Escapement")]/(1 - tbrck[, c("Taku_Rate", "Stikine_Rate", "Alsek_Rate")]), 1, sum), sum(ttcck_totals[ttcck_totals$Year == 2023, c("Spawners", "Catch")]))

# Reformat data for SPS
tbrck_sps <- data.frame(
	region = rep("Northern Transboundary", length(tbrck$Year)),
	species = rep("Chinook", length(tbrck$Year)),
	year = tbrck$Year,
	spawners = tbrck$Escapement_all, 
	smoothedSpawners = NA,
	runsize = tbrck$TotalRun_all,
	smoothedRunsize = NA,
	source_id = tbrck$source_id
) 

# # Catch (for comparison to TCCHINOOL Table A4)
# plot(tbrck$Year, apply( tbrck[, c("Taku_Rate", "Stikine_Rate", "Alsek_Rate")] * tbrck[, c("Taku_Escapement", "Stikine_Escapement", "Alsek_Escapement")]/(1 - tbrck[, c("Taku_Rate", "Stikine_Rate", "Alsek_Rate")]), 1, sum) * 10^-3, 'o', ylab = "Transboudnary CK Catch (thousands)", xlab = "", las = 1)
# lines(1975:2022, c(1257,1584,856,1210,3946,3193,2386,3430,3442,1986,2608,4140,4021,4615,5198,6088,5378,4771,5237,5866,6829,8407,9151,5051,6600,6428,4277,5160,6447,10343,22610,19128,16364,12641,12390,11433,10037,11116,6996,7822,7210,5121,1379,202,1815,3966,713,650)*10^-3, "o", col = 2)
# legend("topleft", pch = 1, lwd = 1, col = c(1,2), c("CTC data request (esc + expl rate)", "CTC Table A4 (Total LC, Rel. IM)"))

# Smoothing
tbrck_sps$smoothedSpawners <- genSmooth(
	abund = tbrck_sps$spawners,
	years = tbrck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Northern Transboundary" & genLength$species == "Chinook"]
)

tbrck_sps$smoothedRunsize <- genSmooth(
	abund = tbrck_sps$runsize,
	years = tbrck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Northern Transboundary" & genLength$species == "Chinook"]
)

plot_abund(tbrck_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrck_sps)

#------------------------------------------------------------------------------
# Transboundary: Sockeye
#------------------------------------------------------------------------------

# TBR Sockeye data are the sum of the Stikine (Appendix B26) and Taku (Appendix D17) 
# In 2024, the Appendix numbers given were B21 and D15 respectively, but that's changed.
tbrse_stik <- read.csv('data/tbr_sockeye_appendixB26.csv')
tbrse_taku <- read.csv('data/tbr_sockeye_appendixD17.csv')

# What is the earliest common year?
min(tbrse_stik$Year) # 1979
min(tbrse_taku$Year) # 1984

max(tbrse_stik$Year) # 2024
max(tbrse_taku$Year) # 2024

# Remove Stikine data prior to 1984
tbrse_stik <- subset(tbrse_stik, tbrse_stik$Year >= 1984)

# plot(tbrse_taku$Year, tbrse_taku$Escapement*10^-3, "o", col = 2, pch = 21, bg = "white", ylim = c(0, 400), xlab = "", bty = "l")
# points(tbrse_taku$Year, tbrse_taku$Run*10^-3, "o", col = 2, pch = 19)
# points(tbrse_stik$Year, tbrse_stik$StikineRiver_EscapementBroodstock*10^-3, "o", col = 4, pch = 21, bg = "white")
# points(tbrse_stik$Year, tbrse_stik$StikineRiver_TerminalRun*10^-3, "o", col = 4, pch = 19)
# legend("topright", fill = c(2,4), c("Taku", "Stikine"))
# 
# Reformat data for SPS
tbrse_sps <- data.frame(
	region = rep("Northern Transboundary", dim(tbrse_stik)[1]),
	species = rep("Sockeye", dim(tbrse_stik)[1]),
	year = tbrse_stik$Year,
	spawners = tbrse_stik$StikineRiver_EscapementBroodstock + tbrse_taku$Escapement, 
	smoothedSpawners = NA,
	runsize = tbrse_stik$StikineRiver_TerminalRun + tbrse_taku$Run,
	smoothedRunsize = NA, 
	source_id = rep("Foos_20250415", dim(tbrse_stik)[1])
) 

# Smoothing
tbrse_sps$smoothedSpawners <- genSmooth(
	abund = tbrse_sps$spawners,
	years = tbrse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Northern Transboundary" & genLength$species == "Sockeye"]
)

tbrse_sps$smoothedRunsize <- genSmooth(
	abund = tbrse_sps$runsize,
	years = tbrse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Northern Transboundary" & genLength$species == "Sockeye"]
)

plot_abund(tbrse_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, tbrse_sps)

#------------------------------------------------------------------------------
# Transboundary: Coho
#------------------------------------------------------------------------------
tbrco <- read.csv('data/tbr_coho_appendixD22.csv')

# Check if years are continuous
unique(diff(tbrco$Year)) # Yes, all one year apart

# Reformat data for SPS
tbrco_sps <- data.frame(
	region = rep("Northern Transboundary", dim(tbrco)[1]),
	species = rep("Coho", dim(tbrco)[1]),
	year = tbrco$Year,
	spawners = tbrco$Escapement, 
	smoothedSpawners = NA,
	runsize = tbrco$TerminalRun,
	smoothedRunsize = NA,
	source_id = rep("Foos_20250415", dim(tbrco)[1])
) 

# Smoothing
tbrco_sps$smoothedSpawners <- genSmooth(
	abund = tbrco_sps$spawners,
	years = tbrco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Northern Transboundary" & genLength$species == "Coho"]
)

tbrco_sps$smoothedRunsize <- genSmooth(
	abund = tbrco_sps$runsize,
	years = tbrco_sps$year,
	genLength = genLength$gen_length[genLength$region == "Northern Transboundary" & genLength$species == "Coho"]
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
	region = rep("Northern Transboundary", nrow(tbrpk)), #rep("Transboundary", length(which(tbrpkcm$species_pooled == "Pink"))),
	species = rep("Pink", nrow(tbrpk)), #length(which(tbrpkcm$species_pooled == "Pink"))),
	year = tbrpk$Year, #sort(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"]),
	spawners = tbrpk$Value, # tbrpkcm$stream_observed_count[tbrpkcm$species_pooled == "Pink"][order(tbrpkcm$year[tbrpkcm$species_pooled == "Pink"])], 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA,
	source_id = rep("Foos_20250415", nrow(tbrpk))
) 
# Smoothing
tbrpk_sps$smoothedSpawners <- genSmooth(
	abund = tbrpk_sps$spawners,
	years = tbrpk_sps$year,
	genLength = genLength$gen_length[genLength$region == "Northern Transboundary" & genLength$species == "Pink"]
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
	region = rep("Northern Transboundary", nrow(tbrcm)), #length(which(tbrpkcm$species_pooled == "Chum"))),
	species = rep("Chum", nrow(tbrcm)), #length(which(tbrpkcm$species_pooled == "Chum"))),
	year = tbrcm$Year, #sort(tbrpkcm$year[tbrpkcm$species_pooled == "Chum"]),
	spawners = tbrcm$Value, #tbrpkcm$stream_observed_count[tbrpkcm$species_pooled == "Chum"][order(tbrpkcm$year[tbrpkcm$species_pooled == "Chum"])], 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA,
	source_id = rep("Foos_20250415", nrow(tbrcm))
) 
# Smoothing
tbrcm_sps$smoothedSpawners <- genSmooth(
	abund = tbrcm_sps$spawners,
	years = tbrcm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Northern Transboundary" & genLength$species == "Chum"]
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
	smoothedRunsize = NA,
	source_id = rep(nuseds_id, length(hgck_years))
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

sp[, "Coho",]

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
		smoothedRunsize = NA,
		source_id = rep(nuseds_id, length(hg.s))
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

# nass_nisgaa <- readxl::read_xlsx("data/2024NassStockAssessment_Table18_TRun.xlsx", sheet = "Table 1", range = "A1:P26", col_types = "numeric")

nass_nisgaa <- read.csv("data/2024NassStockAssessment_combinedTables19-21.csv")


#------------------------------------------------------------------------------
# Nass: Chinook
#------------------------------------------------------------------------------

# TCCHINOOK Table B3
nass_ctc <- readxl::read_xlsx("data/TCCHINOOK-25-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B3", range = "A5:D54", col_types = "numeric", col_names = c("Year", "Above_Gitwinksihlkw2", "Esc", "t.run"))

# Reformat data for SPS
nassck_sps <- data.frame(
	region = rep("Nass", length(nass_ctc$Year)),
	species = rep("Chinook", length(nass_ctc$Year)),
	year = nass_ctc$Year,
	spawners = nass_ctc$Esc, 
	smoothedSpawners = NA,
	runsize = nass_ctc$t.run,
	smoothedRunsize = NA,
	source_id = rep(ctc_id, length(nass_ctc$Year))
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

# use NBSRR data provided by LGL up to 2022 and Nisga'a Post-season for 2023, 2024
nassse <- read.csv("data/nassskeena_sockeye_lgl.csv") %>%
	subset(Region == "Nass") %>%
	mutate(source_id = "English_20230930") %>%
	bind_rows(
		data.frame(Year = c(2023, 2024),
							 TE = nass_nisgaa$ESC_Sockeye[nass_nisgaa$Year %in% c(2023, 2024)],
							 Total.Run = nass_nisgaa$TOTAL_RUN_Sockeye[nass_nisgaa$Year %in% c(2023, 2024)],
							 source_id = rep("Nisgaa_20241203", 2)
		)
	)


# Reformat data for SPS
nassse_sps <- data.frame(
	region = rep("Nass", dim(nassse)[1]),
	species = rep("Sockeye", dim(nassse)[1]),
	year = nassse$Year,
	spawners = nassse$TE, 
	smoothedSpawners = NA,
	runsize = nassse$Total.Run,
	smoothedRunsize = NA, 
	source_id = nassse$source_id
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
# # Use CU-level Nass summer abundance which is a better region-scale indicator
# nasssh <- read.csv("data/spawner_abundance.csv") %>% 
# 	filter(region == "Nass", species_name == "Steelhead", cu_name_pse == "Nass Summer", !is.na(estimated_count))


# Reformat data for SPS
nasssh_sps <- data.frame(
	region = rep("Nass", length(nass_nisgaa$Year)),
	species = rep("Steelhead", length(nass_nisgaa$Year)),
	year = nass_nisgaa$Year,
	spawners = nass_nisgaa$ESC_Steelhead, 
	smoothedSpawners = NA,
	runsize = nass_nisgaa$TOTAL_RUN_Steelhead,
	smoothedRunsize = NA,
	source_id = rep("Nisgaa_20241203",length(nass_nisgaa$Year))
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

# Just use Nisga'a data, which goes back to 1992 and matches English et al. (2023)
# nassco <- nassco_lgl %>%
# 	group_by(Year) %>%
# 	summarise(escapement = sum(TE), runsize = sum(`Total Run`)) %>%
# 	bind_rows(
# 		data.frame(Year = c(2023, 2024),
# 							 escapement = nass_nisgaa$TOTAL_RUN_Coho[nass_nisgaa$Year %in% c(2023, 2024)] * (1 - nass_nisgaa$EXP_RATE_Coho[nass_nisgaa$Year %in% c(2023, 2024)]),
# 							 runsize = nass_nisgaa$TOTAL_RUN_Coho[nass_nisgaa$Year %in% c(2023, 2024)]
# 							 )
# 	)


# Reformat data for SPS
nassco_sps <- data.frame(
	region = rep("Nass", length(nass_nisgaa$Year)),
	species = rep("Coho", length(nass_nisgaa$Year)),
	year = nass_nisgaa$Year,
	spawners = nass_nisgaa$ESC_Coho, 
	smoothedSpawners = NA,
	runsize = nass_nisgaa$TOTAL_RUN_Coho,
	smoothedRunsize = NA,
	source_id = rep("Nisgaa_20241203",length(nass_nisgaa$Year))
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
# Nass: Pink
#------------------------------------------------------------------------------

# Use expanded spawner abundance to 1991 then Nisga'a final report data
sp <- readRDS("output/expanded-spawners/Nass-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

nasspk <- data.frame(
	year = c(yrs, 2024),
	esc = c(round(sp["expanded", "Pink", which(yrs %in% c(1950:1991))]), nass_nisgaa$ESC_Pink),
	run = c(rep(NA, length(c(1950:1991))), nass_nisgaa$TOTAL_RUN_Pink),
	source_id = c(rep(nuseds_id, length(c(1950:1991))), rep("Nisgaa_20241203", length(nass_nisgaa$TOTAL_RUN_Pink)))
)

# Reformat data for SPS
nasspk_sps <- data.frame(
	region = rep("Nass", length(nasspk$year)),
	species = rep("Pink", length(nasspk$year)),
	year = nasspk$year,
	spawners = nasspk$esc,
	smoothedSpawners = NA,
	runsize = nasspk$run,
	smoothedRunsize = NA,
	source_id = nasspk$source_id
)

# Smoothing
nasspk_sps$smoothedSpawners <- genSmooth(
	abund = nasspk_sps$spawners,
	years = nasspk_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Pink"]
)

nasspk_sps$smoothedRunsize <- genSmooth(
	abund = nasspk_sps$runsize,
	years = nasspk_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Pink"]
)

plot_abund(nasspk_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, nasspk_sps)
	
	
#------------------------------------------------------------------------------
# Nass: Chum
#------------------------------------------------------------------------------

nasscm <- data.frame(
	year = c(yrs, 2024),
	esc = c(round(sp["expanded", "Chum", which(yrs %in% c(1950:1991))]), nass_nisgaa$ESC_Chum),
	run = c(rep(NA, length(c(1950:1991))), nass_nisgaa$TOTAL_RUN_Chum),
	source_id = c(rep(nuseds_id, length(c(1950:1991))), rep("Nisgaa_20241203", length(nass_nisgaa$TOTAL_RUN_Chum)))
)

# Reformat data for SPS
nasscm_sps <- data.frame(
	region = rep("Nass", length(nasscm$year)),
	species = rep("Chum", length(nasscm$year)),
	year = nasscm$year,
	spawners = nasscm$esc,
	smoothedSpawners = NA,
	runsize = nasscm$run,
	smoothedRunsize = NA,
	source_id = nasscm$source_id
)

# Smoothing
nasscm_sps$smoothedSpawners <- genSmooth(
	abund = nasscm_sps$spawners,
	years = nasscm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Chum"]
)

nasscm_sps$smoothedRunsize <- genSmooth(
	abund = nasscm_sps$runsize,
	years = nasscm_sps$year,
	genLength = genLength$gen_length[genLength$region == "Nass" & genLength$species == "Chum"]
)

plot_abund(nasscm_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, nasscm_sps)
	

###############################################################################
# Skeena
###############################################################################

#------------------------------------------------------------------------------
# Skeena: Chinook
#------------------------------------------------------------------------------

# TCCHINOOK Table B3
sk_ctc <- readxl::read_xlsx("data/TCCHINOOK-25-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B3", range = "A5:G54", col_types = "numeric", col_names = c("Year", "Nass_Above_Gitwinksihlkw2", "Nass_Esc", "Nass_t.run", "Skeena_Total esc.", "Skeena_GSI esc", "Skeena_GSI SD"))

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
	region = rep("Skeena", length(sk_ctc$Year)),
	species = rep("Chinook", length(sk_ctc$Year)),
	year = sk_ctc$Year,
	spawners = sk_ctc$`Skeena_GSI esc`, 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA,
	source_id = rep(ctc_id, length(sk_ctc$Year))
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
# skse_EnvChg <- read.csv('data/skeena_sockeye.csv')

# Compare to NCC run reconstruction: https://github.com/LGLLimited/nccdbv2/tree/master/run/2022-nass-update/data/KarlEnglish-2023-05-29
skse_LGL <- read.csv("data/nassskeena_sockeye_lgl.csv") %>%
	subset(Region == "Skeena") %>%
	mutate(source_id = "English_20230930")

# From PSC Run Size app:
skse_ch2 <- read.csv("data/Chapter2_Data_2025_07_22.csv") %>%
	filter(Region == "Skeena River") %>%
	mutate(source_id = "PSC_20250722")

# Add recent year's data from Charmaine (pers. comm. July 24 2025)
skse_2023 <- data.frame(
	Year = 2023,
	TE = 1437788,
	catch = 656984
) %>%
	mutate(Total.Run = TE + catch,
	source_id = "CarrHarris_20250724")

# par(mfrow = c(1, 1), mar = c(3,4,2,1))
# plot(skse_LGL$Year, skse_LGL$Total.Run * 10^-6,"n", xlim = c(1960, 2024), bty = "l", ylab = "Abundance (millions)", ylim = c(0, 7.5), xpd = NA, las = 1, yaxs = "i")
# abline(v = seq(1950, 2025, 2), lty = 3, col = grey(0.8))
# abline(v = seq(1950, 2025, 10), col = grey(0.8))
# abline(h = seq(0, 7, 1), lty = 3, col = grey(0.8))
# 
# 
# points(skse_LGL$Year, skse_LGL$Total.Run * 10^-6,"o", pch = 19, xpd = NA)
# points(skse_LGL$Year, skse_LGL$TE * 10^-6, "o", pch =21, bg = "white")
# # points(skse_EnvChg$year, skse_EnvChg$runsize * 10^-3, pch = 19, col = 2)
# # points(skse_EnvChg$year, skse_EnvChg$escapement * 10^-3, col = 2)
# 
# points(skse_ch2$Year, skse_ch2$Spawners * 10^-6, col = 4, cex = 0.8, xpd = NA)
# points(skse_ch2$Year, skse_ch2$Returns * 10^-6, col = 4, pch = 19, cex = 0.8, xpd = NA)
# 
# points(skse_2023$Year, skse_2023$TE * 10^-6, col = 2)
# points(skse_2023$Year, skse_2023$Total.Run * 10^-6, col = 2, pch = 19)
# 
# legend("topleft", col = c(1,4, 2), pch = 19, lwd = c(1, 1, NA), c("English et al. (2023)", "PSC run size app", "2023 update"))

# # Look at Total Return to Canada
# plot(skse$Year, skse$TRTC * 10^-3,"o", xlim = c(1960, 2024), bty = "l", ylab = "TRTC", pch = 19, ylim = c(0, 7000), xpd = NA)
# points(2023, 1.87*10^6*10^-3)
# points(2024, 2*10^6*10^-3)
# 
# plot(skse$TRTC, skse$Total.Run, xlab = "TRTC", ylab = "Total Run")
# abline(v = c(1.87, 2)*10^6)
# Check if years are continuous

# combine sources
skse <- skse_ch2[ , c("Year", "Spawners", "Returns", "source_id")] %>%
	rename(TE = Spawners,
				 Total.Run  = Returns) %>%
	bind_rows(skse_LGL[which(skse_LGL$Year < 1982), c("Year", "TE", "Total.Run", "source_id")]) %>%
	bind_rows(skse_2023[ c("Year", "TE", "Total.Run", "source_id")]) %>%
	arrange(Year)

	
unique(diff(skse$Year)) # Yes, all one year apart

# Reformat data for SPS
skse_sps <- data.frame(
	region = rep("Skeena", dim(skse)[1]),
	species = rep("Sockeye", dim(skse)[1]),
	year = skse$Year,
	spawners = skse$TE, 
	smoothedSpawners = NA,
	runsize = skse$Total.Run,
	smoothedRunsize = NA,
	source_id = skse$source_id
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
sksh <- read.csv("data/SkeenaSteelhead_1956-2024.csv")


# Reformat data for SPS
sksh_sps <- data.frame(
	region = rep("Skeena", dim(sksh)[1]),
	species = rep("Steelhead", dim(sksh)[1]),
	year = sksh$Year,
	spawners = sksh$TyeeEscapement, 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA,
	source_id = "ProvBC_20240929"
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

# # TCNB report not updated; using expanded spawenrs for 2023 
# sk_table32 <- read.csv("data/TCNB-23-01_Table32_Area4escapement.csv")
# 
# # Compare
# plot(yrs, sp[2, "Chum" ,]*10^-3, "o")
# points(sk_table32$YEAR, sk_table32$CHUM, pch =19, cex = 0.8, col = 2)
# 
# # Reformat data for SPS
# skcm_sps <- data.frame(
# 	region = rep("Skeena", dim(sk_table32)[1]),
# 	species = rep("Chum", dim(sk_table32)[1]),
# 	year = sk_table32$YEAR,
# 	spawners = sk_table32$CHUM, 
# 	smoothedSpawners = NA,
# 	runsize = NA,
# 	smoothedRunsize = NA
# ) 
# 
# # Smoothing
# skcm_sps$smoothedSpawners <- genSmooth(
# 	abund = skcm_sps$spawners,
# 	years = skcm_sps$year,
# 	genLength = genLength$gen_length[genLength$region == "Skeena" & genLength$species == "Chum"]
# )
# 
# plot_abund(skcm_sps)
# 
# # Add to master sps dataframe
# sps_data <- rbind(sps_data, skcm_sps)

#------------------------------------------------------------------------------
# Skeena: Chum, Pink and Coho
#------------------------------------------------------------------------------

# Use expanded spawner abundance 
sp <- readRDS("output/expanded-spawners/Skeena-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in c(2, 3, 4)){
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
		smoothedRunsize = NA,
		source_id = rep(nuseds_id, length(skeena.s))
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

#------------------------------------------------------------------------------
# Central Coast Chinook
#------------------------------------------------------------------------------
 # Combo of CTC indicator stocks to Atnarko R. and Rivers Inlet from Table B3

# TCCHINOOK Table B3
cc_ctc <- readxl::read_xlsx("data/TCCHINOOK-25-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B3", range = "H5:K54", col_types = "numeric", col_names = c("Atnarko_Total_esc", "Atnarko_CV", "Atnarko_Wild", "Rivers_Inlet")) %>%
	mutate(Year = c(1975:2024))

# Reformat data for SPS
ccck_sps <- data.frame(
	region = rep("Central Coast", length(cc_ctc$Year)),
	species = rep("Chinook", length(cc_ctc$Year)),
	year = cc_ctc$Year,
	spawners = cc_ctc$Atnarko_Total_esc + cc_ctc$Rivers_Inlet, 
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA,
	source_id = rep(ctc_id, length(cc_ctc$Year))
) 

# Smoothing
ccck_sps$smoothedSpawners <- genSmooth(
	abund = ccck_sps$spawners,
	years = ccck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Central Coast" & genLength$species == "Chinook"]
)

plot_abund(ccck_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, ccck_sps)


#------------------------------------------------------------------------------
# Central Coast chum, coho, pink, and sockeye 
#------------------------------------------------------------------------------

# Expansion from spawner surveys only; no steelhead data
sp <- readRDS("output/expanded-spawners/Central Coast-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in 2:5){
	cc.s <- sp[2, species[s], ]
	# cc.s <- cc.s[!is.na(cc.s)] # There is a missing year for Chinook in 2016...
	
	# Reformat data for SPS
	cc.s_sps <- data.frame(
		region = rep("Central Coast", length(cc.s)),
		species = rep(species[s], length(cc.s)),
		year = as.numeric(names(cc.s)),
		spawners = as.numeric(cc.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA,
		source_id = rep(nuseds_id, length(cc.s))
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
# East Vancouver Island & Mainland Inlets
###############################################################################

#------------------------------------------------------------------------------
# Chinook - Use CTC data from App B4
#------------------------------------------------------------------------------

# (1) CTC data
evimi_ctc <- readxl::read_xlsx("data/TCCHINOOK-25-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B4", range = "A5:G54", col_types = "numeric", col_names = c("Year", "Nanaimo_esc", "Nanaimo_trun", "Cowichan_esc", "Cowichan_trun", "Phillips_esc", "Phillips_trun")) # Southern BC

evimi_ctc_summed <- evimi_ctc %>%
	group_by(Year) %>%
	summarise(esc_sum = sum(Nanaimo_esc, Cowichan_esc, Phillips_esc),
						trun_sum = sum(Nanaimo_trun, Cowichan_trun, Phillips_trun))

# Put in SPS format
evimick_sps <- data.frame(
	region = rep("East Vancouver Island & Mainland Inlets", dim(evimi_ctc_summed)[1]),
	species = rep("Chinook", dim(evimi_ctc_summed)[1]),
	year = evimi_ctc_summed$Year,
	spawners = evimi_ctc_summed$esc_sum, 
	smoothedSpawners = NA,
	runsize = evimi_ctc_summed$trun_sum,
	smoothedRunsize = NA,
	source_id = rep(ctc_id, dim(evimi_ctc_summed)[1])
) 

# Smoothing
evimick_sps$smoothedSpawners <- genSmooth(
	abund = evimick_sps$spawners,
	years = evimick_sps$year,
	genLength = genLength$gen_length[genLength$region == "East Vancouver Island & Mainland Inlets" & genLength$species == "Chinook"]
)

evimick_sps$smoothedRunsize <- genSmooth(
	abund = evimick_sps$runsize,
	years = evimick_sps$year,
	genLength = genLength$gen_length[genLength$region == "East Vancouver Island & Mainland Inlets" & genLength$species == "Chinook"]
)

plot_abund(evimick_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, evimick_sps)

#------------------------------------------------------------------------------
# Other (including steelhead)
#------------------------------------------------------------------------------

# Expansion from spawner surveys; including steelhead (incl. Cheakamus)
sp <- readRDS("output/expanded-spawners/East Vancouver Island & Mainland Inlets-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in 2:6){
	evimi.s <- sp[2, species[s], ]
	
	# Get rid of NAs (esp in 2024)
	evimi.s <- evimi.s[1:max(which(!is.na(evimi.s)))]
	
	# For all VIMI species, start in 1953 - earlier expansion factors are large
	evimi.s	<- evimi.s[which(as.numeric(names(evimi.s)) >= 1953)]
	
	# Reformat data for SPS
	evimi.s_sps <- data.frame(
		region = rep("East Vancouver Island & Mainland Inlets", length(evimi.s)),
		species = rep(species[s], length(evimi.s)),
		year = as.numeric(names(evimi.s)),
		spawners = as.numeric(evimi.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA,
		source_id = rep(nuseds_id, length(evimi.s))
	) 
	
	# Smoothing
	evimi.s_sps$smoothedSpawners <- genSmooth(
		abund = evimi.s_sps$spawners,
		years = evimi.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "East Vancouver Island & Mainland Inlets" & genLength$species == species[s]]
	)
	
	plot_abund(evimi.s_sps)
	
	# Add to master sps dataframe
	sps_data <- rbind(sps_data, evimi.s_sps)
	
}	# end species

###############################################################################
# West Vancouver Island
###############################################################################

#------------------------------------------------------------------------------
# Chinook
#------------------------------------------------------------------------------

wvi_ck_raw <- readxl::read_xlsx("data/TCCHINOOK-25-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B5", range = "K24:K53", col_names = "WCVI_14Stream_Index") %>%
	mutate(Year = c(1995:2024))
# Escapement methods changed in 1995; only use earlier data for assessments

# West Vancouver Island from Brown et al. 2025
wvi_ck_tot_raw <- read_xlsx("data/WCVI_Chinook_total_abundance.xlsx")

wvi_ck_tot_raw$total_abundance <-  wvi_ck_tot_raw$`Hatchery origin Terminal Return` + wvi_ck_tot_raw$`Natural origin Terminal Return` +  wvi_ck_tot_raw$`non-terminal Ocean catch`

yrs <- sort(unique(c(wvi_ck_tot_raw$`Return year`, wvi_ck_raw$Year)))

# Put in SPS format
wvick_sps <- data.frame(
	region = rep("West Vancouver Island", length(yrs)),
	species = rep("Chinook", length(yrs)),
	year = yrs,
	spawners = wvi_ck_raw$WCVI_14Stream_Index[match(yrs, wvi_ck_raw$Year)], 
	smoothedSpawners = NA,
	runsize = wvi_ck_tot_raw$total_abundance[match(yrs, wvi_ck_tot_raw$`Return year`)],
	smoothedRunsize = NA,
	source_id = rep(paste(ctc_id, "Brown_20250703", sep = ", "), length(yrs))
) 

# Smoothing
wvick_sps$smoothedSpawners <- genSmooth(
	abund = wvick_sps$spawners,
	years = wvick_sps$year,
	genLength = genLength$gen_length[genLength$region == "West Vancouver Island" & genLength$species == "Chinook"]
)

wvick_sps$smoothedRunsize <- genSmooth(
	abund = wvick_sps$runsize,
	years = wvick_sps$year,
	genLength = genLength$gen_length[genLength$region == "West Vancouver Island" & genLength$species == "Chinook"]
)

plot_abund(wvick_sps)

if(1 == 2){
	avg <- c(spawners = exp(mean(log(wvick_sps$spawners), na.rm = TRUE)),
	total = exp(mean(log(wvick_sps$runsize), na.rm = TRUE)))

	plot(wvick_sps$year, (wvick_sps$smoothedRunsize - avg[2])/avg[2], "o", pch = 19)
	abline(h= avg[2])
	points(wvick_sps$year, (wvick_sps$smoothedSpawners - avg[1])/avg[1], "o", col = grey(0.6))

}
# Add to master sps dataframe
sps_data <- rbind(sps_data, wvick_sps)

#------------------------------------------------------------------------------
# Sockeye
#------------------------------------------------------------------------------
 # Sum of CU abundance from Sproat, Great Central, and Henderson

wvi_se_raw <- readxl::read_xlsx("data/Barkley_Sockeye_stock-recruit_infilled.xlsx", sheet = "S-R data")

wvi_se_summed <- wvi_se_raw %>%
	dplyr::select(year, stock, S, N) %>% # S = escapement, N = annual terminal run size
	filter(year >= 1977) %>% # Remove 1972 to 2076 for HUC because not available for other stocks
	group_by(year) %>%
	summarise(totalS = sum(S),
						totalN = sum(N))

# plot(wvi_se_summed$year, wvi_se_summed$totalN, "o", ylim = c(0, 2e6), main = "WVI Sockeye", xlab = "", ylab = "")
# points(wvi_se_summed$year, wvi_se_summed$totalS, "o",  col = 2)
# points(yrs, sp[2, "Sockeye", ], "o", col = 4)
# legend("topleft", col = c(1,2,4), c("3 CU - Total run", "3 CU - Escape.", "NuSEDS expanded"), pch = 1, lty = 1)

# Reformat data for SPS
wvise_sps <- data.frame(
	region = rep("West Vancouver Island", dim(wvi_se_summed)[1]),
	species = rep("Sockeye", dim(wvi_se_summed)[1]),
	year = wvi_se_summed$year,
	spawners = wvi_se_summed$totalS, 
	smoothedSpawners = NA,
	runsize = wvi_se_summed$totalN,
	smoothedRunsize = NA,
	source_id = rep("Brown_20250417", dim(wvi_se_summed)[1])
) 

# Smoothing
wvise_sps$smoothedSpawners <- genSmooth(
	abund = wvise_sps$spawners,
	years = wvise_sps$year,
	genLength = genLength$gen_length[genLength$region == "West Vancouver Island" & genLength$species == "Sockeye"]
)

wvise_sps$smoothedRunsize <- genSmooth(
	abund = wvise_sps$runsize,
	years = wvise_sps$year,
	genLength = genLength$gen_length[genLength$region == "West Vancouver Island" & genLength$species == "Sockeye"]
)

plot_abund(wvise_sps)

# Add to master sps dataframe
sps_data <- rbind(sps_data, wvise_sps)

#------------------------------------------------------------------------------
# Chum, coho, pink, steelhead
#------------------------------------------------------------------------------

# Expansion from spawner surveys 

sp <- readRDS("output/expanded-spawners/West Vancouver Island-spawners.rds")
yrs <- as.numeric(dimnames(sp)[[3]])

for(s in c(2,3,4,6)){ # Chum, coho, pink, steelhead
	wvi.s <- sp[2, species[s], ]
	# Get rid of NAs (esp in 2024)
	wvi.s <- wvi.s[1:max(which(!is.na(wvi.s)))]
	
	# For all VIMI species, start in 1953 - earlier expansion factors are large
	wvi.s	<- wvi.s[which(as.numeric(names(wvi.s)) >= 1953)]
	
	# Reformat data for SPS
	wvi.s_sps <- data.frame(
		region = rep("West Vancouver Island", length(wvi.s)),
		species = rep(species[s], length(wvi.s)),
		year = as.numeric(names(wvi.s)),
		spawners = as.numeric(wvi.s), 
		smoothedSpawners = NA,
		runsize = NA,
		smoothedRunsize = NA,
		source_id = rep(nuseds_id, length(wvi.s))
	) 
	
	# Smoothing
	wvi.s_sps$smoothedSpawners <- genSmooth(
		abund = wvi.s_sps$spawners,
		years = wvi.s_sps$year,
		genLength = genLength$gen_length[genLength$region == "West Vancouver Island" & genLength$species == species[s]]
	)
	
	plot_abund(wvi.s_sps)
	
	# Add to master sps dataframe
	sps_data <- rbind(sps_data, wvi.s_sps)
	
}	# end species


###############################################################################
# Fraser
###############################################################################

#------------------------------------------------------------------------------
# Fraser Chinook
#------------------------------------------------------------------------------
# # (1) Use expanded spawner surveys from 1984 onward
# sp <- readRDS("output/expanded-spawners/Fraser-spawners.rds")
# yrs <- c(1984:max(as.numeric(dimnames(sp)[[3]])))
# 
# # (2) Use Atlas et al. (2023) Fraser populations for run size
# frck_run0 <- read.csv("data/Atlas2023/CK_TotalRun_FINAL.csv") %>% 
# 	subset(group == "salish") %>% # dplyr::select Salish group
# 	subset(grepl("skagit", population) == FALSE & grepl("cowichan", population) == FALSE) # Remove VIMI populations
# 
# frck_run <- tapply(frck_run0$tot_run, frck_run0$year, sum)

# (3) CTC data
# ctc_fr <- readxl::read_xlsx("data/TCCHINOOK-25-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B6", range = "A3:P53", col_types = "numeric")
# names(ctc_fr) <- c("Year", "Fraser Spring Age 1.2 Esc", "Fraser Spring Age 1.3 Esc", "Fraser Summer Age 0.3 Esc", "Fraser Summer Age 1.3 Esc", "Fraser Spring/Summer t.run", "Harrison Esc", "Harrison CV", "Lower Shuswap Esc", "Lower Shuswap CV", "Nicola Esc", "Nicola CV", "Lower Chilcotin Esc", "Lower Chilcotin CV", "Chilko Esc", "Chilko CV")
# ctc_esc <- ctc_fr %>% dplyr::select(c("Year", "Fraser Spring Age 1.2 Esc", "Fraser Spring Age 1.3 Esc", "Fraser Summer Age 0.3 Esc", "Fraser Summer Age 1.3 Esc", "Harrison Esc"))
# ctc_esc_sum <- apply(ctc_esc[, c("Fraser Spring Age 1.2 Esc", "Fraser Spring Age 1.3 Esc", "Fraser Summer Age 0.3 Esc", "Fraser Summer Age 1.3 Esc", "Harrison Esc")], 1, sum)
# 
# ctc_trun_sum <- ctc_fr$`Fraser Spring/Summer t.run`

# Data from Chuck Parken (shared 23-Oct-2025 via email)
frck_cp <- readxl::read_xlsx("data/Fraser Chinook escapement and terminal run by SMU.xlsx", range = "A3:K52", col_names = c("year", "spr12_esc", "spr12_run", "spr13_esc", "spr13_run", "sum13_esc", "sum13_run", "sum03_esc", "sum03_run", "harrisonfall03_esc", "harrisonfall03_run"))

# Cut data to only 1984 onward when Harrison data were available.
frck_cp <- frck_cp %>% dplyr::filter(year >= 1984)

# frck_cp$spr12_esc - ctc_fr$`Fraser Spring Age 1.2 Esc`
# range(frck_cp$sum03_esc - ctc_fr$`Fraser Summer Age 0.3 Esc`)

# Numbers match CTC except that t.run from CTC doesn't include Harrison fall. So need to add that in
frck_cp_esc <- round(apply(frck_cp[, grep("esc", names(frck_cp))], 1, sum))
frck_cp_run <- round(apply(frck_cp[, grep("run", names(frck_cp))], 1, sum))


# par(mfrow = c(2,1))
# plot(frck_cp$year, frck_cp_run*10^-3, "o", bty = "l", las = 1, ylab = "Total abundance (thousands)", ylim = c(0, 800), pch = 19, cex = 0.7, col = col_frck[1], xpd = NA)
# points(ctc_esc$Year, ctc_trun_sum*10^-3, col = col_frck[3], "o", pch = 19, cex = 0.7)
# points(ctc_esc$Year, ctc_esc_sum*10^-3, col = col_frck[3], "o", pch = 21, bg = 'white', cex = 0.7)
# legend("topleft")
# #----------------------
# # Plot
# col_frck <- viridisLite::viridis(n = 5)
# 
# par(mfrow = c(2,1))
# plot(ctc_esc$Year, ctc_esc_sum*10^-3, "n", bty = "l", las = 1, ylab = "Spawner abundance (thousands)", ylim = c(0, 700))
# for(i in c(1:5)){
# 	# lines(ctc_esc$Year, data.frame(ctc_esc)[,i] * 10^-3, "l", col = col_frck[i])
# 	if(i == 1){
# 		y1 <- rep(0, length(ctc_esc$Year))
# 		y2 <- data.frame(ctc_esc)[,2]
# 	} else if(i == 2){
# 		y1 <-  data.frame(ctc_esc)[,2]
# 		y2 <- apply(data.frame(ctc_esc)[,2:3], 1, sum)
# 	} else {
# 		y1 <-  apply(data.frame(ctc_esc)[,2:i], 1, sum)
# 		y2 <- apply(data.frame(ctc_esc)[,2:(i+1)], 1, sum)
# 	}
# 
# 	yr <- ctc_esc$Year
# 	if(i == 5){
# 		yr <- ctc_esc$Year[!is.na(ctc_esc$`Harrison Esc`)]
# 		y1 <- y1[!is.na(ctc_esc$`Harrison Esc`)]
# 		y2 <- y2[!is.na(ctc_esc$`Harrison Esc`)]
# 	}
# 	polygon(x = c(yr, rev(yr)),
# 					y = c(y1, rev(y2))*10^-3,
# 					col = col_frck[i], border = NA)
# }
# points(ctc_esc$Year, ctc_esc_sum*10^-3, "o", lwd = 1.5, pch = 21, bg = "white", xpd = NA)
# legend("topleft", fill = col_frck, border = NA, bty = "n", names(ctc_esc)[2:6])
# mtext(side = 3, line = 0.5, "a) Escapement to five Fraser Chinook stocks", adj = 0)
# 
# # Proportion
# plot(ctc_esc$Year, ctc_esc_sum, "n", bty = "l", las = 1, ylab = "Proportion of spawner abundance", ylim = c(0, 1))
# for(i in c(1:5)){
# 	# lines(ctc_esc$Year, data.frame(ctc_esc)[,i] * 10^-3, "l", col = col_frck[i])
# 	if(i == 1){
# 		y1 <- rep(0, length(ctc_esc$Year))
# 		y2 <- data.frame(ctc_esc)[,2]
# 	} else if(i == 2){
# 		y1 <-  data.frame(ctc_esc)[,2]
# 		y2 <- apply(data.frame(ctc_esc)[,2:3], 1, sum)
# 	} else {
# 		y1 <-  apply(data.frame(ctc_esc)[,2:i], 1, sum)
# 		y2 <- apply(data.frame(ctc_esc)[,2:(i+1)], 1, sum)
# 	}
# 
# 
# 	polygon(x = c(ctc_esc$Year, rev(ctc_esc$Year)),
# 					y = c(y1/ctc_esc_sum, rev(y2/ctc_esc_sum)),
# 					col = col_frck[i], border = NA)
# }
# mtext(side = 3, line = 0.5, "b) Proportional contribution of five stocks to aggregate escapement", adj = 0)


yrs <- frck_cp$year
# Put in SPS format
frck_sps <- data.frame(
	region = rep("Fraser", length(yrs)),
	species = rep("Chinook", length(yrs)),
	year = yrs,
	spawners = frck_cp_esc,
	smoothedSpawners = NA,
	runsize = frck_cp_run,
	smoothedRunsize = NA,
	source_id = rep("Parken_20251023", length(yrs))
) 
	
	# Smoothing
frck_sps$smoothedSpawners <- genSmooth(
		abund = frck_sps$spawners,
		years = frck_sps$year,
		genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Chinook"]
	)

frck_sps$smoothedRunsize <- genSmooth(
	abund = frck_sps$runsize,
	years = frck_sps$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Chinook"]
)

plot_abund(frck_sps)
(tail(frck_sps$smoothedRunsize, 1) - exp(mean(log(frck_sps$runsize), na.rm = TRUE)))/exp(mean(log(frck_sps$runsize), na.rm = TRUE))

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
	dplyr::select(year, catch)


frcm_catch2 <- readxl::read_xlsx("data/ChumTC Report Tables - full time series up to 2022.xlsx", sheet = "3-9", range = "A4:G31") %>%
	rename(year = "...1", catch = "Total") %>%
	dplyr::select(year, catch)

# Find years when catch and escapement are reported
yrs_tcchum <- c(max(c(min(frcm_esc$year), min(frcm_catch1$year), min(frcm_catch2$year))):min(c(max(frcm_esc$year), max(frcm_catch1$year), max(frcm_catch2$year)))) #1998:2022

frcm_run <- frcm_esc$esc[match(yrs_tcchum, frcm_esc$year)] + frcm_catch1$catch[match(yrs_tcchum, frcm_catch1$year)] + frcm_catch2$catch[match(yrs_tcchum, frcm_catch2$year)] 
	
# Data extracted from Fraser Management council post-season review presentation
# https://frasersalmon.ca/wp-content/uploads/2025/01/2024-Fraser-Coho-and-Chum-Post-season-Review-2025-01-28-FINAL-1.pdf
frcm_FMC <- read.csv("data/FraserChum_FMC.csv")

# Data from PSC post-season (Brittany Jenewin)
frcm_PSC <- read.csv("data/FraserChum_PSC.csv")

# # Compare
# plot(yrs, sp[2, "Chum", match(yrs, as.numeric(dimnames(sp)[[3]]))]*10^-6, "o", pch = 19, bty = "l", xlab = "", ylab = "Abundance (millions)", las = 1, main = "
# 		 Fraser Chum", xlim = c(1980, 2025), cex = 0.6, ylim = c(0, 3))
# abline(v = seq(1950, 2025, 5), col = "#00000030")
# points(yrs_tcchum, frcm_esc$esc*10^-6, "o", col = 2, pch = 19, cex = 0.8, xpd = NA)
# polygon(x = c(yrs_tcchum, rev(yrs_tcchum)),
# 				y = c(frcm_esc$esc, rev(frcm_run))*10^-6,
# 				col = "#FF000040",
# 				border = NA)
# points(yrs_tcchum, frcm_run*10^-6, "o", col = 2, pch = 21, bg = "white", cex = 0.8, xpd = NA)
# 
# # PSC - updated from post-season review
# points(frcm_PSC$year, frcm_PSC$escapement*10^-6, pch = 3, col = 2)
# points(frcm_PSC$year, frcm_PSC$total_run*10^-6, pch = 4, col = 2)
# 
# # FMC - updated from post-season review
# points(frcm_FMC$year, frcm_FMC$escapement*10^-6, pch = 3, col = 4)
# points(frcm_FMC$year, frcm_FMC$runsize*10^-6, pch = 4, col = 4)
# 
# abline(v = 2024, lty = 2)
# 
# legend("topleft", pch = c(19, 19, 21, 3, 4, 3, 4), pt.cex = c(1, 0.8, 0.8, 1, 1, 1, 1), col = c(1,2,2, 2, 2, 4, 4), pt.bg = c(NA, NA, "white", NA, NA, NA, NA), c("PSF-expanded from NuSEDS indicator streams", "TCCHUM (23)-01 Table 3-11", "TCCHUM esc + catch", "PSC post-season esc.", "PSC post-season run", "FMC esc.", "FMC run"), lwd = c(1, 1, 1, NA, NA, NA, NA), cex = 0.7)


# Add recent PSC data to previous tables up to 2022
# Note: these data are very close to the FMC presentation; can be considered the same
frcm <- frcm_esc %>%
	mutate(runsize = frcm_run) %>%
	bind_rows(data.frame(
		year = c(2023, 2024),
		esc = frcm_PSC$escapement[match(c(2023, 2024), frcm_PSC$year)],
		runsize = frcm_PSC$total_run[match(c(2023, 2024), frcm_PSC$year)]
	))


# Put in SPS format
frcm_sps <- data.frame(
	region = rep("Fraser", length(frcm$year)), 
	species = rep("Chum", length(frcm$year)), 
	year = frcm$year, 
	spawners = frcm$esc,  
	smoothedSpawners = NA,
	runsize = frcm$runsize,
	smoothedRunsize = NA,
	source_id = c(rep("Jenewin_20240416", nrow(frcm_esc)), rep("DFO_20250128", 2))
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
# to 2022

# # Expansion from spawner surveys only; no steelhead data
# sp <- readRDS("output/expanded-spawners/Fraser-spawners.rds")
# # yrs <- as.numeric(dimnames(sp)[[3]])
# expansion_factors <- readRDS("output/expanded-spawners/Fraser-expansion-factors.rds")

# Compiled data
frco <- read.csv("data/IFC_data_compiled.csv")

# plot(as.numeric(dimnames(sp)[[3]]), sp[2, 3, ]*10^-3, "o", pch = 19,  cex = 0.6, ylab = "Abundance (thousands)", bty = "l", col = grey(0.6))
# abline(v = 2022, lty = 3)
# points(frco$year, frco$runsize*10^-3, "o", cex = 0.8, pch =19)
# points(frco$year, frco$spawners*10^-3, "o", cex = 0.8, pch =21, bg = 'white')
# points(frco$year, frco$spawners_naturalOrigin*10^-3, col = 4)
# legend("topleft", pch = c(19, 19, 1, 1), col = c(grey(0.8), 1, 1, 4), pt.cex = c(0.6, 0.8, 0.8, 1), c("NuSEDS expansion", "IFC runsize", "IFC total spawners", "IFC natural spawners"))
# 
# par(new = TRUE)
# plot(as.numeric(dimnames(sp)[[3]]), expansion_factors[[3]]$exp1, col = 4, pch = 4, yaxt = "n", xaxt = "n", bty = "n", xlab = "", ylab = "")
# axis(side = 4, las = 1)
# mtext(side = 4, "Expansion factor", line = 3)
# 
# plot(c(1980, 2025), c(0, 400), "n", bty = "n", ylab = "Abundance (thousands)", xlab = "", yaxs = "i")
# abline(v = seq(1980, 2025, 2), lty = 3, col = grey(0.8))
# abline(v = seq(1980, 2025, 10), col = grey(0.8))
# segments(x0 = yrs, x1 = yrs, y0 = 0, y1 = tapply(frco$Total.Prefishery.Abundance, frco$ReturnYear, sum)*10^-3, lwd = 8, lend = 2, col = grey(0.8))
# segments(x0 = yrs, x1 = yrs, y0 = 0, y1 = tapply(frco$Total.Return, frco$ReturnYear, sum)*10^-3, lwd = 8, lend = 2)
# segments(x0 = yrs, x1 = yrs, y0 = 0, y1 = tapply(frco$Natural.Returns, frco$ReturnYear, sum)*10^-3, lwd = 8, lend = 2, col = 2)
# abline(h = seq(0, 400, 50))
# 
# write.csv(data.frame(
# 	year = yrs,
# 	spawners_naturalOrigin = tapply(frco$Natural.Returns, frco$ReturnYear, sum),
# 	spawners_hatcheryOrigin = tapply(frco$Hatchery.Returns, frco$ReturnYear, sum),
# 	spawners = tapply(frco$Total.Return, frco$ReturnYear, sum), # Note this is the fish that returned to spawn, NOT a mistake!
# 	runsize = tapply(frco$Total.Prefishery.Abundance, frco$ReturnYear, sum)),
# 	file = "data/IFC_data_compiled.csv")

# Put in SPS format
frco_sps <- data.frame(
	region = rep("Fraser", length(frco$year)),
	species = rep("Coho", length(frco$year)),
	year = frco$year,
	spawners = frco$spawners, # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = frco$runsize,
	smoothedRunsize = NA,
	source_id = c(rep("Glavas_20231006", length(frco$year) - 2), rep("DFO_20250225", 2))
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
# Note: Only odd year data, so no change 2024 -> 2025 (data still to 2023)
# From PSC 
frpk <- read.csv("data/pink_run_size_2025-04-24.csv") # No changes from Apr 24, 2025 version to Sept 18, 2025, but cite Sept because fraser sockeye in-season estimate for 2024 DID change (and don't want to change that number at this point)

# Put in SPS format
frpk_sps <- data.frame(
	region = rep("Fraser", length(frpk$Year)),
	species = rep("Pink", length(frpk$Year)),
	year = frpk$Year,
	spawners = frpk$Escapement, # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = frpk$Run.Size,
	smoothedRunsize = NA, 
	source_id = rep("PSC_20250424", length(frpk$Year))
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
frse <- read.csv("data/Fraser Sockeye Run Size_2025-04-24.csv") %>%
	filter(Management.Group == "Total Fraser")

# frse2 <- read.csv("~/Downloads/Data 13/Fraser Sockeye Run Size_2025-09-18.csv") %>%
# 	filter(Management.Group == "Total Fraser")
# cbind(frse$Run.Size, frse2$Run.Size)
# cbind(frse$In.season.Run.Size, frse2$In.season.Run.Size) # CHANGED, stick with April data until next year
# cbind(frse$Spawning.Escapement, frse2$Spawning.Escapement)

# Put in SPS format
frse_sps <- data.frame(
	region = rep("Fraser", length(frse$Year)),
	species = rep("Sockeye", length(frse$Year)),
	year = frse$Year,
	spawners = frse$Spawning.Escapement, # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = frse$Run.Size,
	smoothedRunsize = NA,
	source_id = rep("PSC_20250424", length(frse$Year))
) 

# Fill in run size with in-season estimates if available
if(sum(is.na(frse_sps$runsize)) > 0){
	if(sum(!is.na(frse$In.season.Run.Size[which(frse$Year %in% frse_sps$year[is.na(frse_sps$runsize)])])) > 0){
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

# plot_abund(frse_sps[frse$Year >= 1980,]); abline(v = 2024)
plot_abund(frse_sps)
# Add to master sps dataframe
sps_data <- rbind(sps_data, frse_sps)

#------------------------------------------------------------------------------
# Steelhead
#------------------------------------------------------------------------------

# Use sum of two main Fraser steelhead CUs
frsh <- read.csv("data/dataset1cu_steelhead_20250117.csv") %>% 
	filter(region == "Fraser", species_name == "Steelhead") %>%
	filter(cu_name_pse %in% c("Thompson Summer", "Mid Fraser Summer")) %>%
	filter(!is.na(estimated_count) & estimated_count != -989898)

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
	smoothedRunsize = NA,
	source_id = rep("Bison_20241129", length(frsh_sum))
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
# colck <- read.csv("data/spawner_abundance.csv") %>% 
# 	filter(region == "Columbia", species_name == "Chinook") %>%
# 	filter(!is.na(estimated_count))

colck <- read.csv("data/Chinook_Okanagan_NEW.csv") %>% filter(Year >= 2006)
# Downloaded from https://github.com/SOLV-Code/Scanner-Data-Processing/blame/main/DATA_IN/Chinook_Okanagan_NEW.csv
# on March 18, 2025

# Put in SPS format
colck_sps <- data.frame(
	region = rep("Columbia", length(colck$Year)),
	species = rep("Chinook", length(colck$Year)),
	year = colck$Year,
	spawners = as.numeric(colck$NatOrigSpn + colck$HatchOrigSpn), # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = NA,
	smoothedRunsize = NA,
	source_id = rep("DFO_20250318", length(colck$Year))
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
colse_old <- read.csv("data/spawner_abundance.csv") %>% 
	subset(region == "Columbia" &  species_name == "Lake sockeye") %>%
	subset(!is.na(estimated_count))

colse <- read.csv("data/OkanaganSockeye.csv")

colse2 <- readxl::read_xlsx("data/Okanagan Sockeye Esc for KR_revJn2025.xlsx", sheet = "CAN Escapement", range = "A1:E65")

# # Plot tpo reproduce fig 27A of Ogden et al, (2025)
# bp <- barplot(rbind(colse$spawners, colse$harvest, colse$enroute_loss)*10^-3, beside = FALSE, col = c('lightblue', grey(0.8), "white"), las = 1, ylab = "year", yaxs = "i")
# axis(side = 1, at = bp[seq(1, length(bp), 10)], labels = seq(1980, 2025, 10))
# legend("topleft", fill = c('lightblue', grey(0.8), "white"), c("Spawners", "Harvest", "Enroute Loss"), bty = "n")

# # Compare
# plot(c(1960,2025), c(0,600), "n", ylab = "Abundance (thousands)", xlab = "", las = 1, bty = "l")
# segments(x0 = colse$year, x1 = colse$year, y0 = rep(0, length(colse$year)), y1 = colse$runsize*10^-3, lend = 2, lwd = 8, col = grey(0.8))
# segments(x0 = colse$year, x1 = colse$year, y0 = rep(0, length(colse$year)), y1 = colse$spawners*10^-3, lend = 2, lwd = 8, col = 2)
# 
# lines(colse2$Year, colse2$`Total spawners`*10^-3, "o", pch =21, bg = "white", col = 4)
# points(colse2$Year, colse2$Wells*10^-3, pch = 19, col = 3)
# 
# abline(v = 2003.5); text(c(1995, 2015), c(600,600), c("Not used", "Used"))
# legend("topleft", fill = c(grey(0.8), 2, NA, NA), border = NA, pch = c(NA, NA, 21, 19), pt.bg = c(NA, NA, "white", NA), col = c(NA, NA, 4, 3), lwd= c(NA, NA, 1, NA), legend = c("WSP Run Size", "WSP Spanwers", "Ogden total spawners", "Ogden Wells"))
# plot(colse$year, colse$spawners*10^-3, "o", bty = "l", xlab = "", ylab = "Spawners (thousands)", las = 1, ylim = c(0, 210))
# points(colse_old$year, colse_old$estimated_count*10^-3, col = 2, pch = 19, cex = 0.6)
# legend("topleft", pch = c(1,19), col = c(1,2), pt.cex = c(1,0.6), legend = c("DFO (2024)", "PSE"))
# abline(v = 2003.5); text(c(1995, 2015), c(210, 210), c("Not used", "Used"))


# Put in SPS format
yrs <- sort(colse2$Year) # don't use pre-2004 (not comparable; DFO 2024)
colse_sps <- data.frame(
	region = rep("Columbia", length(yrs)),
	species = rep("Sockeye", length(yrs)),
	year = yrs,
	spawners = round(colse2$`Total spawners`[match(yrs, colse2$Year)]), 
	smoothedSpawners = NA,
	runsize = colse$runsize[match(yrs, colse$year)],
	smoothedRunsize = NA, 
	source_id = c(rep("Ogden_20250627, Ogden_20250901", length(yrs) - 1), "Ogden_20250627")
) 

# Smoothing
colse_sps$smoothedSpawners <- genSmooth(
	abund = colse_sps$spawners,
	years = colse_sps$year,
	genLength = genLength$gen_length[genLength$region == "Columbia" & genLength$species == "Sockeye"]
)
colse_sps$smoothedRunsize <- genSmooth(
	abund = colse_sps$runsize,
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
	smoothedRunsize = NA,
	source_id = rep("OBMEP_20250228", length(colsh$year))
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
