library(readxl)
library(dplyr)
library(ggplot2)
library(ragg)

source("code/functions.R")


theme_temp <-	theme_classic(base_family = "Sofia Pro", base_size = 14) +
		theme(axis.text  = element_text(size = 12),
					axis.title = element_text(size = 14),
					legend.position = "none")


# Preliminary pre-season number for 2026
frse_inseas <- 3016000


# Get raw and smoothed data
# the following is copied from 2_compile_regional_data, but adds 2026 preliminary info

## Load data ---
frse <- read.csv(here("data/Fraser Sockeye Run Size_2026-04-20.csv")) %>%
	filter(Management.Group == "Total Fraser")
frse[nrow(frse)+1,] <- c(2026, rep(NA, 8), frse_inseas)

# Load 2025 Near-final escapement estimates
frse_esc_2025 <- readxl::read_xlsx(here("data/2025 Fraser Sockeye Escapement Summary.xlsx"), sheet=2)

# Put in SPS format
frse_sps <- data.frame(
	region = rep("Fraser", length(frse$Year)),
	species = rep("Sockeye", length(frse$Year)),
	year = frse$Year,
	spawners = c(frse$Spawning.Escapement[which(frse$Year<2025)], 
							 frse_esc_2025$escapement[frse_esc_2025$area_cu=="Total"], NA), # Add 2025 near-final estimate # Note this is the fish that returned to spawn, NOT a mistake!
	smoothedSpawners = NA,
	runsize = frse$Run.Size,
	smoothedRunsize = NA,
	source_id = c(rep("PSC_20260417", length(frse$Year)-1), "Everitt_20260708")
) 

# Fill in run size with in-season estimates if available
if(sum(is.na(frse_sps$runsize)) > 0){
	if(sum(!is.na(frse$In.season.Run.Size[which(frse$Year %in% frse_sps$year[is.na(frse_sps$runsize)])])) > 0){
		frse_sps$runsize[which(frse$Year %in% frse_sps$year[is.na(frse_sps$runsize)])] <- frse$In.season.Run.Size[which(frse$Year %in% frse_sps$year[is.na(frse_sps$runsize)])]
	}
}

# Smoothing
genLength <- read.csv(here("data/gen_length_regions.csv"))

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


# Make series of plots for animation (TOTAL abundance)
ylims <- c(0, 41)
xlims <- c(min(frse_sps$year), 2029)
p0 <- frse_sps %>% ggplot(aes(x=year)) + 
	scale_x_continuous(limits=xlims, breaks=seq(1900, 2020, 20), labels=seq(1900, 2020, 20)) +
	lims(y=ylims) + labs(y= "Total abundance (millions)", x="") +
	theme_temp

# 1. Plot 2025 raw (total) abundance only
p1 <- frse_sps %>% filter(year == 2025) %>% ggplot(aes(x=year)) + 
	geom_point(aes(y=runsize*10^-6, shape=species), size=2, col="#74BDB8") + 
	scale_x_continuous(limits=xlims, breaks=seq(1900, 2020, 20), labels=seq(1900, 2020, 20)) +
	lims(y=ylims) + labs(y= "Total abundance (millions)", x="") +
	#annotate("text", x=2026, y=9, hjust=0, label="9.07M") +
	theme_temp + theme(legend.position = "none")

# 2. Add 2026 raw total abundance
p2 <- p1 + geom_point(aes(x=2026, y=frse_inseas*10^-6, shape=species), size=2, col="#74BDB8") #+ 
	#annotate("text", x=2027, y=3, hjust=0, label="3.02M") 

# 3.5. Add long-term average line
geo_mean_run <- exp(mean(log(frse_sps$runsize[frse_sps$year<2026]))) # Exclude 2026 data from this
p2.5 <- p2 + geom_segment(aes(y=geo_mean_run*10^-6, x=1893, xend=2026), lty="dashed", col="grey40")

# 3. Add raw data 
p3 <- p0 + geom_segment(aes(y=geo_mean_run*10^-6, x=1893, xend=2026), lty="dashed", col="grey40") + 
	geom_line(aes(y=runsize*10^-6), col="#74BDB8", linewidth=0.65)


# 4. Add smoothed total abundance to 2025
p4 <- p3 + geom_line(data=filter(frse_sps, year<2026), aes(y=smoothedRunsize*10^-6), col="#386662", linewidth=0.8) +
	geom_point(data=filter(frse_sps, year==2025), aes(y=smoothedRunsize*10^-6), size=2, col="#386662")

# 4.5. Show 4 years of data
p4.5 <- p1 + geom_line(data=filter(frse_sps, year %in% 2022:2025), aes(y=runsize*10^-6), col="#74BDB8", linewidth=0.65) +
	geom_point(data=filter(frse_sps, year %in% 2022:2025), aes(y=runsize*10^-6), col="#74BDB8")


# 5. Add smoothed total abundance for 2026

p5 <- p3 + geom_line(data=frse_sps, aes(y=smoothedRunsize*10^-6), col="#386662", linewidth=0.8) +
	geom_point(data=filter(frse_sps, year==2026), aes(y=smoothedRunsize*10^-6), col="#386662", size=2)

# 5.5. Smoothed and raw abundances to 2025 
p5.5 <- p0 + geom_segment(aes(y=geo_mean_run*10^-6, x=1893, xend=2026), lty="dashed", col="grey40") +
	geom_point(data=filter(frse_sps, year==2025), aes(y=runsize*10^-6), col="#74BDB8", size=2) +
	geom_line(data=filter(frse_sps, year<2026), aes(y=runsize*10^-6), col="#74BDB8", linewidth=0.65) +
	geom_line(data=filter(frse_sps, year<2026), aes(y=smoothedRunsize*10^-6), col="#386662", linewidth=0.8) +
	geom_point(data=filter(frse_sps, year==2025), aes(y=smoothedRunsize*10^-6), col="#386662", size=2) 


# 6. Add short-term trend line

# Load trend info
sps_data <- read.csv(here("output/sps-data.csv"))

frse_trends <- sps_data %>% filter(region=="Fraser", species == "Sockeye")

p6 <- p5 + geom_line(data=frse_trends, aes(x=year, y=runsize_short_trend*10^-6), col="orange", linewidth=1) + 
	geom_ribbon(data=frse_trends, aes(x=year, ymin=runsize_short_trend_lwr*10^-6, ymax=runsize_short_trend_upr*10^-6), fill="orange", alpha=0.4)


# 6.5. Long-term trend line only (+ data to 2025)
p6.5 <- p5.5 + geom_line(data=frse_trends, aes(x=year, y=runsize_long_trend*10^-6), col="navy", linewidth=1) +
	geom_ribbon(data=frse_trends, aes(x=year, ymin=runsize_long_trend_lwr*10^-6, ymax=runsize_long_trend_upr*10^-6), fill="navy", alpha=0.4)

# 7. Add long-term trend line
p7 <- p6 + geom_line(data=frse_trends, aes(x=year, y=runsize_long_trend*10^-6), col="navy", linewidth=1) +
	geom_ribbon(data=frse_trends, aes(x=year, ymin=runsize_long_trend_lwr*10^-6, ymax=runsize_long_trend_upr*10^-6), fill="navy", alpha=0.4)
	
# 7.5. Both trend lines, data to 2025
p7.5 <- p6.5 + geom_line(data=frse_trends, aes(x=year, y=runsize_short_trend*10^-6), col="orange", linewidth=1) + 
	geom_ribbon(data=frse_trends, aes(x=year, ymin=runsize_short_trend_lwr*10^-6, ymax=runsize_short_trend_upr*10^-6), fill="orange", alpha=0.4)

	
# Save them
agg_png(here("output/ignore/presentation/p0_2026.png"), height=10, width=23, units="cm", res=72*2); p0; dev.off()
agg_png(here("output/ignore/presentation/p1_2026.png"), height=10, width=23, units="cm", res=72*2); p1; dev.off()
agg_png(here("output/ignore/presentation/p2_2026.png"), height=10, width=23, units="cm", res=72*2); p2; dev.off()
agg_png(here("output/ignore/presentation/p2.5_2026.png"), height=10, width=23, units="cm", res=72*2); p2.5; dev.off()
agg_png(here("output/ignore/presentation/p3_2026.png"), height=10, width=23, units="cm", res=72*2); p3; dev.off()
agg_png(here("output/ignore/presentation/p4_2026.png"), height=10, width=23, units="cm", res=72*2); p4; dev.off()
agg_png(here("output/ignore/presentation/p5_2026.png"), height=10, width=23, units="cm", res=72*2); p5; dev.off()
agg_png(here("output/ignore/presentation/p5.5_2026.png"), height=10, width=23, units="cm", res=72*2); p5.5; dev.off()
agg_png(here("output/ignore/presentation/p6_2026.png"), height=10, width=23, units="cm", res=72*2); p6; dev.off()
agg_png(here("output/ignore/presentation/p6.5_2026.png"), height=10, width=23, units="cm", res=72*2); p6.5; dev.off()
agg_png(here("output/ignore/presentation/p7_2026.png"), height=10, width=23, units="cm", res=72*2); p7; dev.off()
agg_png(here("output/ignore/presentation/p7.5_2026.png"), height=10, width=23, units="cm", res=72*2); p7.5; dev.off()


# Make a series of plots for animation (SPAWNER abundance)
ylims <- c(0, 14)
xlims <- c(min(frse_sps$year), 2028)
s0 <- frse_sps %>% ggplot(aes(x=year)) + 
	scale_x_continuous(limits=xlims, breaks=seq(1900, 2020, 20), labels=seq(1900, 2020, 20)) +
	lims(y=ylims) + labs(y= "Spawner abundance (millions)", x="") +
	theme_temp

# 1. Plot 2025 raw (total) abundance only
s1 <- frse_sps %>% filter(year == 2025) %>% ggplot(aes(x=year)) + 
	geom_point(aes(y=spawners*10^-6), size=2, col="#74BDB8") + 
	scale_x_continuous(limits=xlims, breaks=seq(1900, 2020, 20), labels=seq(1900, 2020, 20)) +
	lims(y=ylims) + labs(y= "Spawner abundance (millions)", x="") +
	#annotate("text", x=2026, y=9, hjust=0, label="9.07M") +
	theme_temp + theme(legend.position = "none")

# 2. Add long-term average line
geo_mean_sp <- exp(mean(log(frse_sps$spawners[frse_sps$year<2026]))) # Exclude 2026 data from this
s2 <- s1 + geom_segment(aes(y=geo_mean_sp*10^-6, x=1893, xend=2026), lty="dashed", col="grey40")

# 3. Add historical data
s3 <- s0 + geom_segment(aes(y=geo_mean_sp*10^-6, x=1893, xend=2026), lty="dashed", col="grey40") + 
	geom_line(aes(y=spawners*10^-6), col="#74BDB8", linewidth=0.65)


# 4. Add smoothed total abundance to 2025
s4 <- s3 + geom_point(data=filter(frse_sps, year==2025), aes(y=spawners*10^-6), col="#74BDB8", size=2) +
	geom_line(data=filter(frse_sps, year<2026), aes(y=smoothedSpawners*10^-6), col="#386662", linewidth=0.8) +
	geom_point(data=filter(frse_sps, year==2025), aes(y=smoothedSpawners*10^-6), size=2, col="#386662")

# 5. Add short-term trend line
s5 <- s4 + geom_line(data=frse_trends, aes(x=year, y=spawners_short_trend*10^-6), col="orange", linewidth=1) + 
	geom_ribbon(data=frse_trends, aes(x=year, ymin=spawners_short_trend_lwr*10^-6, ymax=spawners_short_trend_upr*10^-6), fill="orange", alpha=0.4)

# 6. Add long-term trend line
s6 <- s5 + geom_line(data=frse_trends, aes(x=year, y=spawners_long_trend*10^-6), col="navy", linewidth=1) + 
	geom_ribbon(data=frse_trends, aes(x=year, ymin=spawners_long_trend_lwr*10^-6, ymax=spawners_long_trend_upr*10^-6), fill="navy", alpha=0.4)

# 6.5. Long-term trend line only
s6.5 <- s4 + geom_line(data=frse_trends, aes(x=year, y=spawners_long_trend*10^-6), col="navy", linewidth=1) + 
	geom_ribbon(data=frse_trends, aes(x=year, ymin=spawners_long_trend_lwr*10^-6, ymax=spawners_long_trend_upr*10^-6), fill="navy", alpha=0.4)

# Save them
agg_png(here("output/ignore/presentation/s1_2026.png"), height=10, width=23, units="cm", res=72*2); s1; dev.off()
agg_png(here("output/ignore/presentation/s2_2026.png"), height=10, width=23, units="cm", res=72*2); s2; dev.off()
agg_png(here("output/ignore/presentation/s3_2026.png"), height=10, width=23, units="cm", res=72*2); s3; dev.off()
agg_png(here("output/ignore/presentation/s4_2026.png"), height=10, width=23, units="cm", res=72*2); s4; dev.off()
agg_png(here("output/ignore/presentation/s5_2026.png"), height=10, width=23, units="cm", res=72*2); s5; dev.off()
agg_png(here("output/ignore/presentation/s6_2026.png"), height=10, width=23, units="cm", res=72*2); s6; dev.off()
agg_png(here("output/ignore/presentation/s6.5_2026.png"), height=10, width=23, units="cm", res=72*2); s6.5; dev.off()



