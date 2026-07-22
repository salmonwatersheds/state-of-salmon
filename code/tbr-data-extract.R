# Extract 2024 data
library(readxl)
library(dplyr)

run_CK <- FALSE # Do you want to get Chinook data? Usually better to wait for CTC as it is incomplete.

###############################################################################
# Sockeye: Sum of stikine and taku
###############################################################################

#------------------------------------------------------------------------------
# Stikine
#------------------------------------------------------------------------------

stikine_sockeye <- read_xlsx("data/2025 PSF Pacific Salmon Explorer Data.xlsx", sheet = "Stik Appen B26", range = "A5:P51")
names(stikine_sockeye) <- c("Year", 
														"AllTahltan_AboveBorderRun", 
														"AllTahltan_CanHarvest",
														"AllTahltan_EscapementBroodstock",
														"AllTahltan_USHarvest",
														"AllTahltan_TerminalRun",
														"StikineMainstem_AboveBorderRun", 
														"StikineMainstem_CanHarvest",
														"StikineMainstem_Escapement",
														"StikineMainstem_USHarvest",
														"StikineMainstem_TerminalRun",
														"AllTahltanPlusMainstem_AboveBorderRun", 
														"AllTahltanPlusMainstem_CanHarvest",
														"AllTahltanPlusMainstem_EscapementBroodstock",
														"AllTahltanPlusMainstem_USHarvest",
														"AllTahltanPlusMainstem_TerminalRun")
head(stikine_sockeye)

# Wait! There's a new table with "Stikine Sockeye", use that?
stikine_sockeye_true <- read_xlsx("data/2025 PSF Pacific Salmon Explorer Data.xlsx", sheet = "Stik Appen B26", range = "A58:F105")
names(stikine_sockeye_true) <- c("Year", 
																 "StikineRiver_AboveBorderRun", 
																 "StikineRiver_CanHarvest",
																 "StikineRiver_EscapementBroodstock",
																 "StikineRiver_USHarvest",
																 "StikineRiver_TerminalRun") 

# Compare to 2024 SOS data
#stikine_sockeye24 <- read.csv("data/tbr_sockeye_appendixB21.csv")
windows(width = 6, height = 6, pointsize = 10)
par(mfrow = c(2,1), mar = c(4,4,2,1), oma = c(0,0,1,0))
plot(stikine_sockeye$Year, stikine_sockeye$AllTahltanPlusMainstem_TerminalRun, "o", ylab = "Terminal run", xlab = "", bty = "l")
#points(stikine_sockeye24$Year, stikine_sockeye24$Terminal.Run, col = 2, pch = 19, cex = 0.8) # Last few years adjusted, but otherwise good
points(stikine_sockeye_true$Year, stikine_sockeye_true$StikineRiver_TerminalRun, col = 4, lwd = 2) 
mtext(side =3, line = 1, adj = 0, "a) Total abundance")

mtext(side =3, outer = TRUE, line = 0, "Stikine Sockeye (App B26)", cex = 1.2)
#legend("topright", pch = c(1, 1, 19), col = c(1,4, 2), c("2025 AllT+Main.", "2025 Stikine River", "2024 data"))

# Escapement
plot(stikine_sockeye$Year, stikine_sockeye$AllTahltanPlusMainstem_EscapementBroodstock, "o", ylab = "Escapement/Broodstock", xlab = "", bty = "l")
points(stikine_sockeye_true$Year, stikine_sockeye_true$StikineRiver_EscapementBroodstock, col = 4, lwd = 2) 
#points(stikine_sockeye24$Year, stikine_sockeye24$Escapement.broodstock, col = 2, pch = 19, cex = 0.8) # Last few years adjusted, but otherwise good
mtext(side =3, line = 1, adj = 0, "c) Spawner abundance")

# DECISION: use "Stikine River" table instead of "Tahltan + Mainstem" (estimates tend to be similar or slightly higher)

# Write 2026 data
stikine_sockeye_sos <- stikine_sockeye_true %>%
	select(Year, StikineRiver_EscapementBroodstock, StikineRiver_TerminalRun)
write.csv(stikine_sockeye_sos, "data/tbr_sockeye_appendixB26.csv", row.names = FALSE)

#------------------------------------------------------------------------------
# Taku
#------------------------------------------------------------------------------

taku_sockeye <- read_xlsx("data/2025 PSF Pacific Salmon Explorer Data.xlsx", sheet = "TAL Appen D.17", range = "A6:J48")

taku_sockeye2024 <- read.csv(here(Dropbox_dir, "data/archive/2024/tbr_sockeye_appendixD15.csv"))
taku_sockeye2025 <- read.csv(here(Dropbox_dir, "data/archive/2025/tbr_sockeye_appendixD17.csv"))


# Plot changes
windows(width = 6, height = 6, pointsize = 10)
par(mfrow = c(2,1), mar = c(4,4,2,1), oma = c(0,0,1,0))
plot(taku_sockeye$Year, taku_sockeye$Run, "o", ylab = "Terminal run", xlab = "", bty = "l")
points(taku_sockeye2025$Year, taku_sockeye2025$Run, col = 3, pch = 19, cex = 0.8) # tiny or no adjustments after 2025
points(taku_sockeye2024$Year, taku_sockeye2024$Terminal.Run, col = 2, pch = 19, cex = 0.8) # Small adjustments happened after 2024
mtext(side =3, line = 1, adj = 0, "a) Total abundance")

mtext(side =3, outer = TRUE, line = 0, "Taku Sockeye (App D17)", cex = 1.2)
legend("topright", pch = c(1, 19), col = c(1, 2, 3), c("2026 report data", "2025 report data", "2024 report data"))

# Escapement
plot(taku_sockeye$Year, taku_sockeye$Escapement, "o", ylab = "Escapement", xlab = "", bty = "l")
points(taku_sockeye2025$Year, taku_sockeye2025$Escapement, col = 3, pch = 19, cex = 0.8, xpd = NA) # tiny (or none?) adjustments after 2025
points(taku_sockeye2024$Year, taku_sockeye2024$Escapement, col = 2, pch = 19, cex = 0.8, xpd = NA) # Small adjustments after 2024
mtext(side =3, line = 1, adj = 0, "c) Spawner abundance")

# Write 2026 data
taku_sockeye_sos <- taku_sockeye %>%
	select(Year, Escapement, Run)
write.csv(taku_sockeye_sos, "data/tbr_sockeye_appendixD17.csv", row.names = FALSE)

###############################################################################
# Coho: Just Taku
###############################################################################

taku_coho <- read_xlsx("data/2025 PSF Pacific Salmon Explorer Data.xlsx", sheet = "TAK Appen D.22", range = "A6:L45")
# Note still missing 2025 total run, but I think we want to use Terminal run anyway. # Do we??

names(taku_coho) <- c("Year",
											"AboveBorderMR_RunEstimate",
											"AboveBorderMR_EndDate",
											"ExpansionMethod",
											"ExpansionFactor",
											"ExpandedEstimate",
											"CanadianHarvest",
											"Escapement",
											"TerminalRun_USHarvest",
											"TerminalRun",
											"TerminalRun_HarvestRate",
											"TotalRun" # Includes US spawners and catch of US spawners?
)

taku_coho2025 <- read.csv(here(Dropbox_dir, "data/archive/2025/tbr_coho_appendixD20.csv"))

# Plot changes
windows(width = 6, height = 6, pointsize = 10)
par(mfrow = c(2,1), mar = c(4,4,2,1), oma = c(0,0,1,0))
plot(taku_coho$Year, taku_coho$TerminalRun, "o", ylab = "Terminal run", xlab = "", bty = "l")
points(taku_coho2025$Year, taku_coho2025$Run, col = 2, pch = 19, cex = 0.8) # Small adjustments throughout time series
mtext(side =3, line = 1, adj = 0, "a) Total abundance")

mtext(side =3, outer = TRUE, line = 0, "Taku coho (App D22)", cex = 1.2)
legend("topright", pch = c(1, 19), col = c(1, 2), c("2026 report data", "2025 report data"))

# Escapement
plot(taku_coho$Year, taku_coho$Escapement, "o", ylab = "Escapement", xlab = "", bty = "l")
points(taku_coho2025$Year, taku_coho2025$Escape., col = 2, pch = 19, cex = 0.8, xpd = NA) # Small adjustments throughout time series
mtext(side =3, line = 1, adj = 0, "c) Spawner abundance")

# Write 2026 data
write.csv(taku_coho, "data/tbr_coho_appendixD22.csv", row.names = FALSE)


###############################################################################
# Chinook: Alsek, Stikine, and Taku
# Compare to CTC
###############################################################################

# Better to just wait for CTC...
if(run_CK){
	# CTC data
	tbr_ctc <- read.csv("../../../State of Salmon/2_Data & Analysis/state-of-salmon/data/CTC_Synoptic_evaluation_data_all_2025-04-16.csv") %>%
		filter(Year > 1975) %>% # Remove 1975 -> missing Alsek data
		filter(Stock %in% c("Alsek", "Taku", "Stikine")) %>%
		select(Stock, Year, Escapement, RateType, Rate)
	
	# TTC data used in 2024
	# (2) TTC data
	ttcck <- read.csv("../../../State of Salmon/2_Data & Analysis/state-of-salmon/data/TTC_MERGED.csv") %>%
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
	
	# TTC update April 2025
	stikck <- read_xlsx("data/1_raw-data/2024 PSF Pacific Salmon Explorer Data.xlsx", sheet = "Stik Appen B12-13", range = "A5:G51")
	names(stikck) <- c("Year",
										 "AboveBorderRun_MR",
										 "AboveBorderRun_Method",
										 "CanadianHarvest",
										 "Escapement",
										 "USHarvest",
										 "TerminalRun")
	# Remove 'a' from 2014
	stikck$Year[stikck$Year == "2014a"] <- 2014
	
	# Check: is Terminal run sum of harvest?
	stikck$TerminalRun - (stikck$CanadianHarvest + stikck$Escapement + stikck$USHarvest)
	
	# Proposal: Use CTC through 2022, update with TTC for 2023 and 2024
	tbrck_25 <- data.frame(
		Year = c(1976:2024),
		StikineEsc = c(tbr_ctc$Escapement[tbr_ctc$Stock == "Stikine"], stikck$Escapement[stikck$Year  == 2024]),
		StikineRun = c(round(tbr_ctc$Escapement[tbr_ctc$Stock == "Stikine"]/(1 - tbr_ctc$Rate[tbr_ctc$Stock == "Stikine"])), stikck$TerminalRun[stikck$Year == 2024])
	)
	
	# How does this compare to previous years?
	par(mfrow = c(2,1), mar = c(2,4,1,1), oma = c(0,0,1,0))
	plot(tbrck_25$Year, tbrck_25$StikineEsc, "o", xlim = c(1970, 2025), ylab = "Escapement", xlab = "")
	abline(v = 2024, lty =2)
	points(stikck$Year, stikck$Escapement, col = 2, pch = 19, cex = 0.6) # Recent two years match
	points(tbr_ctc$Year[tbr_ctc$Stock == "Stikine"], tbr_ctc$Escapement[tbr_ctc$Stock == "Stikine"], col = 4, pch = 1, cex = 1.4)
	legend("topright", pch = c(1,19,1), col = c(1,2,4), c("To use 2025", "Updated TTC", "CTC Synoptic Jul-2024"))
	
	
	plot(tbrck_25$Year, tbrck_25$StikineRun, "o", xlim = c(1970, 2025), xlab = "", ylab = "Terminal run")
	abline(v = 2024, lty =2)
	points(stikck$Year, stikck$TerminalRun, col = 2, pch = 19, cex = 0.6) # =
	points(tbr_ctc$Year[tbr_ctc$Stock == "Stikine"], tbr_ctc$Escapement[tbr_ctc$Stock == "Stikine"]/(1 - tbr_ctc$Rate[tbr_ctc$Stock == "Stikine"]), col = 4, pch = 1, cex = 1.2)
	mtext(side = 3, line = 0, outer = TRUE, "Stikine Chinook")
	
	
	#-----
	# Alsek
	# TTC update April 2025
	alsekck <- read_xlsx("data/1_raw-data/2024 PSF Pacific Salmon Explorer Data.xlsx", sheet = "Stik Appen B12-13", range = "A5:G51")
	names(stikck) <- c("Year",
										 "AboveBorderRun_MR",
										 "AboveBorderRun_Method",
										 "CanadianHarvest",
										 "Escapement",
										 "USHarvest",
										 "TerminalRun")
}

###############################################################################
# Pink & Chum & Steelhead- Taku (Canyon Island) Fish wheel
###############################################################################

taku_fishwheel <- read_xlsx("data/2025 PSF Pacific Salmon Explorer Data.xlsx", sheet = "TAK Appen D.24", range = "A8:P50") %>%
	select(Year, Pink...7, Chum...8, Steelhead) %>%
	rename(Pink = Pink...7, Chum = Chum...8)
	

write.csv(taku_fishwheel, "data/TTC_taku_fishwheel.csv", row.names = FALSE)


