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

###############################################################################
# Dive into results
###############################################################################

cc_exp <- readRDS("output/expanded-spawners/Central Coast-spawners.rds")

cc_ef <- readRDS("output/expanded-spawners/Central Coast-expansion-factors.rds")

plot(as.numeric(names(cc_ef[[2]]$exp1)), cc_ef[[2]]$exp1, "o") # chum
abline(h = mean(cc_ef[[2]]$exp1))
abline(h = 1.5, col = 2)
abline(v = seq(1950, 2025, 5), lty = 2)

spawner_surveys.all <- read.csv("data/spawner_surveys_revised_2026-08-14.csv")

s <- 4

ss <- spawner_surveys.all %>%
	filter(region == "Central Coast", species_name == species_vec[s])

ss_avg <- ss %>% filter(indicator == "Y") %>%
	group_by(stream_name_pse) %>%
	summarise(avg = exp(mean(log(stream_observed_count), na.rm = TRUE))) %>%
	arrange(1/avg)

ss_wide <- ss %>% filter(indicator == "Y") %>%
	group_by(year) %>%
	arrange(factor(stream_name_pse, levels = ss_avg$stream_name_pse)) %>%
	pivot_wider(id_cols = year, names_from = stream_name_pse, values_from = stream_observed_count) %>%
	arrange(year)

# ss_max <- max(ss_wide[,2:11], na.rm = TRUE)*10^-3
ss_max <- max(cc_exp[2, s, ], na.rm = TRUE)*10^-3

# quartz()
par(mar = c(3,4,2,15))
plot(range(ss_wide$year), c(0, ss_max), "n", las = 1, xlab = "", ylab = "Spawners (1000s)", bty = "l")
for(i in 1:10){
	points(ss_wide$year, data.frame(ss_wide)[, 1+i]*10^-3, "o", pch = 19, cex = 0.6, col = pnw_palette('Bay', n = 10)[i])
}
legend(2027, ss_max, pch = 19, pt.cex = 0.6, lwd = 1, col = pnw_palette('Bay', n = 10), legend = ss_avg$stream_name_pse[1:10], xpd = NA, bty = "n")

lines(as.numeric(names(cc_exp[1, s, ])), cc_exp[1, s, ]*10^-3, lwd = 2)
lines(as.numeric(names(cc_exp[2, s, ])), cc_exp[2, s, ]*10^-3, lwd = 2, lty = 2)
mtext(species_vec[s], side = 3, cex =2)
#-------------------------------------------------------------------------------
# EVIMI sockeye, why not as good?
#-------------------------------------------------------------------------------

evimi_exp <- readRDS("output/expanded-spawners/East Vancouver Island & Mainland Inlets-spawners.rds")

evimi_exp[, "Sockeye", ]

spawner_surveys.all <- read.csv("data/spawner_surveys_revised_2026-08-14.csv")

ss <- spawner_surveys.all %>%
	filter(region == "East Vancouver Island & Mainland Inlets", species_name == "Sockeye")
ss_wide <- ss %>% filter(indicator == "Y") %>%
	group_by(year) %>%
	pivot_wider(id_cols = year, names_from = stream_name_pse, values_from = stream_observed_count) %>%
	arrange(year)

exp(mean(log(ss_wide$`NIMPKISH RIVER`), na.rm = TRUE))

apply(ss_wide, 2, function(x){exp(mean(log(x), na.rm = TRUE))})

write.csv(ss_wide, 
					row.names = FALSE, file = "output/ignore/evimi_se_indicator_streams.csv")

#-------------------------------------------------------------------------------
# Skeena and CC coho
#-------------------------------------------------------------------------------
ss <- spawner_surveys.all %>%
	filter(region == "Skeena", species_name == "Coho")
ss_wide <- ss %>% filter(indicator == "Y") %>%
	group_by(year) %>%
	pivot_wider(id_cols = year, names_from = stream_name_pse, values_from = stream_observed_count) %>%
	arrange(year)
write.csv(ss_wide, 
					row.names = FALSE, file = "output/ignore/sk_co_indicator_streams.csv")

#-------------------------------------------------------------------------------
# Chinook blurb
#-------------------------------------------------------------------------------

# Yukon - averages by decade
sps_dat %>% filter(region == "Yukon", species == "Chinook") %>%
	select(year, spawners, runsize) %>%
	mutate(decade = floor(year/10)*10) %>%
	group_by(decade) %>%
	summarise(decade_spawners = exp(mean(log(spawners))),
						decade_runsize = exp(mean(log(runsize))))

sps_dat %>% filter(region == "Yukon", species == "Chinook") %>%
	select(year, spawners, runsize) %>%
	mutate(decade = floor(year/10)*10) %>%
	filter(year <= 2006) %>%
	summarise(exp(mean(log(spawners))),
						exp(mean(log(runsize))))

###############################################################################
# How has current state changed?
###############################################################################

sps_metrics_2024 <- read.csv(paste0(Dropbox_dir, "/output/archive/sps-metrics_2024-07-19.csv"))
sps_metrics_2025 <- read.csv(paste0(Dropbox_dir, "/output/archive/sps-metrics_2025-11-05.csv"))
sps_metrics_2026 <- read.csv(paste0(Dropbox_dir, "/output/archive/sps-metrics_2026-09-04.csv"))

species <- unique(sps_metrics_2026$species)
regions <- unique(sps_metrics_2026$region)
region_abbr <- c("YU", "TB", "HG", "NA", "SK", "CC", "EI", "WI", "FR", "CO")
type_name <- c("spawner", "total")


# Need to change TBR name and split VIMI in 2024
sps_metrics_2024$region[sps_metrics_2024$region == "Transboundary"] <- "Northern Transboundary"

sps_metrics_2024 <- sps_metrics_2024 %>%
	rbind(sps_metrics_2024 %>% 
					filter(region == "Vancouver Island & Mainland Inlets") %>%
					mutate(region = "East Vancouver Island & Mainland Inlets")) %>%
	mutate(region = if_else(region == "Vancouver Island & Mainland Inlets", "West Vancouver Island", region))

unique(sps_metrics_2024$region)

# Create combined dataset of current state 2024, 2025, 2026
sps_track <- sps_metrics_2026 %>%
	filter(type == "Spawners") %>%
	select(region, species, current_status) %>%
	mutate(year = 2026) %>%
	rbind(sps_metrics_2025 %>%
					filter(type == "Spawners") %>%
					select(region, species, current_status) %>%
					mutate(year = 2025)) %>%
	rbind(sps_metrics_2024 %>%
					filter(type == "Spawners") %>%
					select(region, species, current_status) %>%
					mutate(year = 2024)) %>%
	pivot_wider(names_from = year, values_from = current_status) %>%
	select(region, species, `2024`, `2025`, `2026`)

sps_track <- sps_track %>% 
	filter(paste(region, species, sep = "-") %in% c("Yukon-Pink", "Yukon-Sockeye", "Yukon-Steelhead", "Columbia-Chum", "Columbia-Coho", "Columbia-Pink") == FALSE)

# Plot
quartz(width = 7, height = 7, pointsize = 10, family = "Sofia Pro Bold")

par(mar = rep(0,4), family = "Sofia Pro Semi Bold")
plot(1,1,"n", xlab = "", ylab = "", bty = "n", xaxt  = "n", yaxt = "n", xlim = c(0.02,5.98), ylim = c(0.02, 9.98), xaxs = "i", yaxs = "i")

k <- 1
for(r in 1:10){
	for(s in 1:6){
		region.r <- rev(regions)[r]
		species.s <- species[s]
		sps_track.rs <- sps_track %>% filter(region == region.r & species == species.s)
		
		
		if(length(sps_track.rs$`2026`) > 0){ # If there is a 2026 status
			
			
			if(is.na(sps_track.rs$`2026`)){
				polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['dd'], border = NA)
			} else {
				
				if(sps_track.rs$`2026` == -999999){
				polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['crit'], border = NA)
			} else {
				polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['bg'], border = NA)
			}
		
			# Add sparkline
			if(sps_track.rs$`2026` == -999999){
				points(s - 0.2, r - 0.5, col = "#9d5857", pch = 19, cex = 2)
				text(s - 0.2, r - 0.5, pos = 2, "critical ", col = "#9d5857", cex = 1)
			} else {
				if(sum(is.na(sps_track.rs[, c("2024", "2025", "2026")]))  == 2){ # if just one year of data
					cs.norm <- ifelse(is.na(sps_track.rs[, c("2024", "2025", "2026")]), NA, r - 0.5)
					x.loc <- c(s - 0.8, s - 0.5, s - 0.2)
					points(x.loc, cs.norm, col = ifelse(sps_track.rs$`2026` > 0, "#89a67a", '#ad605f'), cex = 1.6)
				}else{
					cs.range <- range(sps_track.rs[, c("2024", "2025", "2026")], na.rm = TRUE)
					y.box <- c(r - 0.8, r - 0.2)
					cs.norm <- (sps_track.rs[, c("2024", "2025", "2026")] - cs.range[1])/(cs.range[2] - cs.range[1]) * 0.6 + y.box[1]
					x.loc <- c(s - 0.8, s - 0.5, s - 0.2)
				}
				lines(x.loc, cs.norm, col = ifelse(sps_track.rs$`2026` > 0, "#89a67a", '#ad605f'))
				points(x.loc, cs.norm, col = fishy_cols_func(as.numeric(sps_track.rs[, c("2024", "2025", "2026")]*100)), pch = 19, cex = 1.5)	
			}
				
			} # end if not data deficient
		} # end if species are present
		
	} # end s
} # end r

# combined Spawners ad runsize for saving
sps_track.all <- sps_metrics_2026 %>%
	select(region, species, type, current_status) %>%
	mutate(year = 2026) %>%
	rbind(sps_metrics_2025 %>%
					select(region, species, type, current_status) %>%
					mutate(year = 2025)) %>%
	rbind(sps_metrics_2024 %>%
						select(region, species, type, current_status) %>%
					mutate(year = 2024)) %>%
	pivot_wider(names_from = year, values_from = current_status) %>%
	select(region, species, type, `2026`, `2025`, `2024`) %>%
	mutate(region = factor(region, levels = regions)) %>%
	arrange(type, region, species)

	write.csv(sps_track.all, file = paste0(Dropbox_dir, "/output/ignore/SOS_compare_years_2026-09-10.csv"))
	