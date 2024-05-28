# Compiled spawner and run size
sps_dat <- read.csv("output/sps-data.csv")

sps_metrics <- read.csv("output/sps-summary.csv")

source("code/colours.R")
# Extra figures
# Apr 9, 2024
species <- sort(unique(sps_dat$species))
regions <- unique(sps_metrics$region)
regionsName <- regions
regionsName[regions == "Vancouver Island & Mainland Inlets"] <- "VIMI"
###############################################################################
# Fishy dot plot
###############################################################################

regionsCol <- PNWColors::pnw_palette("Bay", n = 9)

i <- 2
type <- c("Spawners", "Total return")[i]

# quartz(width = 8, height = 4.5, pointsize = 10)
yr <- range(sps_metrics$current_status[sps_metrics$type == type], na.rm = TRUE)
yr <- c(-100, 160)
par(mar = c(2, 5, 2, 8))
plot(c(0.5, 6.5), c(-120, 160), "n", yaxt = 'n', ylab = "Salmon Abundance", xaxt = "n", xlab = "", xaxs = "i", bty = "n")
abline(h = seq(-120, 160, 20), lty = 3, col = grey(0.6))
axis(side = 2, at = seq(-100, 150, 50), las = 1)
abline(h = 0, lwd = 5, col = SWP_cols['stone1'])
axis(side = 2, at = -115, "Unknown", las = 1)
for(s in 1:6){
	text(s, yr[2]+7, species[s], col = species_cols_dark[s], xpd = NA, pos = 3)
	polygon(x = s + c(-0.45, -0.45, 0.45, 0.45),
					y = c(-100, 200, 200, -100),
					col = "#00000020",
					#col = paste0(species_cols_light[s], 50),
					border = NA)
	polygon(x = s + c(-0.45, -0.45, 0.45, 0.45),
					y = c(-200, -105, -105, -200), 
					col = grey(0.8),
					border =NA)
	
	ind <- which(sps_metrics$species == species[s] & sps_metrics$type == type)
	# ind <- which(sps_metrics$species == species[s] & sps_metrics$type == "Total return")
	# ptCol <- ifelse(sps_metrics$current_status[ind] > 0, status_cols['green'], status_cols['red'])
	r <- match(sps_metrics$region[ind], regions)
	points(seq(s-0.3, s+0.3, length.out = 9)[r], sps_metrics$current_status[ind], pch = 19, cex = 2, col = regionsCol[r], xpd = NA)#col = ptCol)
	points(seq(s-0.3, s+0.3, length.out = 9)[r], ifelse(is.na(sps_metrics$current_status[ind]), -115, NA), cex = 2, pch = 19, col = regionsCol[r]) #, col = grey(0.5)
	
	# # Add run size
	# ind2 <- which(sps_metrics$species == species[s] & sps_metrics$type == "Run Size")
	# ptCol2 <- ifelse(sps_metrics$status[ind2] > 0, status_cols['green'], status_cols['red'])
	# points(seq(s-0.3, s+0.3, length.out = 9), sps_metrics$status[ind2]*100, pch = 1, cex = 2, col = ptCol2)
	# 
	# segments(x0 = seq(s-0.3, s+0.3, length.out = 9),
	# 				 x1 = seq(s-0.3, s+0.3, length.out = 9),
	# 				 y0 = sps_metrics$status[ind]*100,
	# 				 y1= sps_metrics$status[ind2]*100,
	# 				 col = ifelse(is.na(ptCol2), NA, paste0(ptCol2, 50)),
	# 				 lwd = 2)

}
mtext(side = 3, type, line = 1, cex = 1.2)
# mtext(side = 3, "Total return", line = 1, cex = 1.2)
# legend(-0.2, 180, pch = c(19, NA), pt.cex = 2, c("Spawners", ""), xpd = NA, bty = "n")
legend(6.5, 130, pch = 19, pt.cex = 2, col = regionsCol, legend = regionsName, xpd = NA, bty = "n")

# legend(-0.2, 180, pch = c(19, 1), pt.cex = 2, c("Spawners", "Run size"), xpd = NA, bty = "n")

#---- Just run size

yr <- range(sps_metrics$status[sps_metrics$type == "Run Size"], na.rm = TRUE)
par(mar = c(2, 5, 2, 1))
plot(c(0.5, 6.5), c(-120, 140), "n", yaxt = 'n', ylab = "Abundance", xaxt = "n", xlab = "", xaxs = "i")
axis(side = 2, at = seq(-100, 100, 50), las = 1)
abline(h = 0, lwd = 5, col = SWP_cols['stone1'])
axis(side = 2, at = -115, "Unknown", las = 1)
for(s in 1:6){
	text(s, 145, species[s], col = species_cols_dark[s], xpd = NA, pos = 3)
	polygon(x = s + c(-0.45, -0.45, 0.45, 0.45),
					y = c(-100, 200, 200, -100),
					col = paste0(species_cols_light[s], 50),
					border = NA)
	polygon(x = s + c(-0.45, -0.45, 0.45, 0.45),
					y = c(-200, -105, -105, -200), 
					col = grey(0.8),
					border =NA)
	
	ind <- which(sps_metrics$species == species[s] & sps_metrics$type == "Run Size")
	ptCol <- ifelse(sps_metrics$status[ind] > 0, status_cols['green'], status_cols['red'])
	points(seq(s-0.3, s+0.3, length.out = 9), sps_metrics$status[ind]*100, pch = 19, cex = 2, col = ptCol)
	points(seq(s-0.3, s+0.3, length.out = 9), ifelse(is.na(sps_metrics$status[ind]), -115, NA), cex = 2, col = grey(0.5), pch = 19)
	
}

legend(-0.2, 180, pch = c(19, 1), pt.cex = 2, c("Spawners", "Run size"), xpd = NA, bty = "n")

###############################################################################
# Trends
###############################################################################
regions <- unique(sps_metrics$region)

r <- "Skeena"
trend <- "long"
selected_species <- unique(sps_dat$species[sps_dat$region == r])
selected_species <- "Sockeye"
runsize <- FALSE
# pdf(file = "output/ignore/figures/trendMockPlots_2024-04-10.pdf", width = 8, height = 4.5, pointsize = 10)
# quartz(width = 8.5, height = 4.5, pointsize = 10)

# for(R in 1:9){
# 	r <- regions[R]
	
	# selected_species <- unique(sps_dat$species[sps_dat$region == r])
	
	
	
	par(mar = c(3, 4, 2, 8), bg = "white")
	plot(c(1960, 2024), c(-100, 200), "n", yaxt = 'n', ylab = "Salmon Abundance", xlab = "", xaxs = "i", bty = "l")
	axis(side = 2, at = seq(-100, 200, 50), labels = paste0(seq(-100, 200, 50), "%"), las = 1)
	abline(h = 0, lwd = 5, col = grey(0.8))
	abline(v = seq(1960, 2024, 10), col = grey(0.8))
	abline(h = seq(-100, 200, 50), col = grey(0.8))
	
	# mtext(side = 3, line = 1, r)
	for(s in which(species %in% selected_species)){
		dat <- sps_dat[which(sps_dat$species == species[s] & sps_dat$region == r),]	
		
		if(runsize == TRUE){
		met <- sps_metrics[which(sps_metrics$species == species[s] & sps_metrics$region == r & sps_metrics$type == "Run Size"),]
		lines(dat$year, (dat$smoothedRunsize - met$hist)/met$hist*100, col = species_cols_light[s])
		pt.bg = "white"
		} else {
			met <- sps_metrics[which(sps_metrics$species == species[s] & sps_metrics$region == r & sps_metrics$type == "Spawners"),]
			lines(dat$year, (dat$smoothedSpawners - met$hist)/met$hist*100, col = species_cols_light[s])
			pt.bg <- species_cols_light[s]
		}
		
		if(is.na(trend)){
			points(met$current_year, (met$current - met$hist)/met$hist*100, pch = 21, cex = 2, col = species_cols_dark[s], bg = pt.bg)
		
			text(2024, (met$current - met$hist)/met$hist*100, pos = 4, paste0(species[s], "  ", round(met$status*100), "%"), col = species_cols_dark[s], xpd = NA)	
			
		} else if(trend == "short"){ # Short-trend
			if(met$short_trend_cat == "arrows-left-right"){ # Non-significant
				# lines(dat$year, (dat$short_trend - met$hist)/met$hist*100, lwd = 3, col = line.col, lty = lty.rs)
				text(2024, tail((dat$short_trend - met$hist)/met$hist*100, 1), pos = 4, paste0(species[s], "  ", "(stable)"), col = species_cols_dark[s], xpd = NA, font = 2)	
			} else {
				lines(dat$year, (dat$short_trend - met$hist)/met$hist*100, lwd = 3, col = species_cols_dark[s])
				text(2024, tail((dat$short_trend - met$hist)/met$hist*100, 1), pos = 4, paste0(species[s], "  ", round(met$short_trend*100), "%"), col = species_cols_dark[s], xpd = NA, font = 2)	
			}
			
			
			# polygon(x = c(dat$year[!is.na(dat$short_trend)], rev(dat$year[!is.na(dat$short_trend)])),
			# 				y = (c(dat$short_trend_lwr[!is.na(dat$short_trend)], rev(dat$short_trend_upr[!is.na(dat$short_trend)])) - met$hist)/met$hist*100,
			# 				col = paste0(species_cols_light[s], 30),
			# 				border = NA)
			
		} else if(trend == "long"){
			if(met$long_trend_cat == "arrows-left-right"){ # Non-significant
				text(2024, tail((dat$long_trend - met$hist)/met$hist*100, 1), pos = 4, paste0(species[s], "  ", "(stable)"), col = species_cols_dark[s], xpd = NA, font = 2)	
			} else {
				lines(dat$year, (dat$long_trend - met$hist)/met$hist*100, lwd = 3, col = species_cols_dark[s])
				text(2024, tail((dat$long_trend - met$hist)/met$hist*100, 1), pos = 4, paste0(species[s], "  ", round(met$long_trend*100), "%"), col = species_cols_dark[s], xpd = NA, font = 2)	
			}
			# polygon(x = c(dat$year[!is.na(dat$long_trend)], rev(dat$year[!is.na(dat$long_trend)])),
			# 				y = (c(dat$long_trend_lwr[!is.na(dat$long_trend)], rev(dat$long_trend_upr[!is.na(dat$long_trend)])) - met$hist)/met$hist*100,
			# 				col = paste0(species_cols_light[s], 30),
			# 				border = NA)
		}
		
	}

	
# 	} # end regions
# 
# dev.off()
# 
# quartz(width = 3, height = 3, pointsize = 10)	
# 	plot(1,1,"n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
# legend("center", lwd = c(1.5, 1), col = c(species_cols_dark[s], species_cols_light[s]), c("Spawners", "Run size"), bty = "n")	

	
	#_________________
	
head(sps_dat)

frco <- read.csv("data/E. Hertz - IFC Data.csv")
y <- tapply(frco$Natural.Returns, frco$ReturnYear, sum)

dat.i <- sps_dat %>%
	filter(region == "Fraser", species == "Coho")

met.i <- sps_metrics %>% 
	filter(region == "Fraser", species == "Coho")
quartz(width = 5, height = 8)
par(mfrow = c(2,1))
plot(dat.i$year, dat.i$spawners*10^-3, "l", ylim = c(0, 85), xlim = c(1983, 2023), las = 1, ylab = "Aggregate spawners (thousands)", col = 2, lwd = 2, xpd = NA, bty = "l")
# lines(dat.i$year, dat.i$smoothedSpawners*10^-3, col = 2, lwd = 0.8)
lines(as.numeric(names(y)), y*10^-3,  "l")
abline(h = met.i$average_abundance[met.i$type == "Spawners"]*10^-3, lty = 2, col = 2)
legend(1990, 120, lty = c(1,1,2), col = c(1,2,2), lwd = c(1, 1.5, 1), c("Natural spawners", "Total spawners (used in SoS)", "Historical avg (SoS)"), bty = "n", xpd = NA)

plot(dat.i$year, dat.i$runsize*10^-3, "l", xlim = c(1983, 2023), las = 1, ylab = "Aggregate total return (thousands)", col = 2, lwd = 2, xpd = NA, bty = "l")
# lines(dat.i$year, dat.i$smoothedSpawners*10^-3, col = 2, lwd = 0.8)
abline(h = met.i$average_abundance[met.i$type == "Total return"]*10^-3, lty = 2, col = 2)

###############################################################################
# Extra chunks of code for diggin into data


z <- sps_dat[which(sps_dat$species == "Chinook" & sps_dat$region == "Haida Gwaii"), c("year", "spawners", "smoothedSpawners", "runsize", "smoothedRunsize")]


z


sps_metrics[which(sps_metrics$species == "Steelhead" & sps_metrics$type == "Spawners"),]


# Plot coho abundances
regionsCol <- PNWColors::pnw_palette("Bay", n = 9)

selected_species <- "Chum"

ylims <- range(sps_dat$smoothedSpawners[sps_dat$species == selected_species], na.rm = TRUE)
par(mar = c(3, 4, 1, 12))
plot(c(1950, 2023), c(0, ylims[2]*10^-3), "n", xlab= "", ylab = "Smoothed spawners (thousands)", main = selected_species, yaxs = "i")
abline(h = seq(0, 700, 50), col = grey(0.8), lty = 3)
abline(v = seq(1950, 2023, 5), col = grey(0.8), lty = 3)
abline(v = seq(1950, 2023, 10), col = grey(0.5), lty = 3)

for(r in 1:9){
	sps_dat.r <- sps_dat %>% 
		filter(region == regions[r], species == selected_species)
	if(nrow(sps_dat.r) > 0){
		lines(sps_dat.r$year, sps_dat.r$smoothedSpawners*10^-3, lwd = 2, col = regionsCol[r])
	}
}
legend(2028, ylims[2]*0.9*10^-3, xpd = NA, lwd = 2, col = regionsCol, legend = regionsName, bty = "n")

# Relative to average
par(mar = c(3, 4, 1, 12))
plot(c(1950, 2023), c(-100, 180), "n", xlab= "", ylab = "Smoothed spawners (anomalies)", yaxs = "i")
abline(h = seq(-100, 100, 10), col = grey(0.8), lty = 3)
abline(v = seq(1950, 2023, 5), col = grey(0.8), lty = 3)
abline(v = seq(1950, 2023, 10), col = grey(0.5), lty = 3)
abline(h = 0)
for(r in 1:9){
	sps_dat.r <- sps_dat %>% 
		filter(region == regions[r], species == selected_species)
	avg.r <- sps_metrics$average_abundance[which(sps_metrics$region == regions[r] &sps_metrics$species == selected_species & sps_metrics$type == "Spawners")]
	
	if(nrow(sps_dat.r) > 0){
		lines(sps_dat.r$year, (sps_dat.r$smoothedSpawners - avg.r)/avg.r*100, lwd = 2, col = regionsCol[r])
	}
}
legend(2028, 100, xpd = NA, lwd = 2, col = regionsCol, legend  = regionsName, bty = "n")

#-------
# Average spawners by region and species
r.offset <- seq(-0.4, 0.4, length.out = 9)

plot(c(1,6), c(0, 1500), "n", xaxt = "n", ylab = "Average spawners", xlim = c(0.5, 6.5), xaxs = "i")
axis(side = 1, at = 1:6, labels = species)
abline(v = seq(1.5, 5.5, 1))

y <- tapply(sps_metrics$average_abundance[sps_metrics$type == "Spawners"], sps_metrics$species[sps_metrics$type == "Spawners"], mean, na.rm = TRUE)
for(s in 1:6){
	segments(x0 = s - 0.5, x1 = s+0.5, y0 = y[s]*10^-3, y1 = y[s]*10^-3, lwd = 3, col = grey(0.8))
}

for(r in 1:9){
	sr <- sps_metrics[sps_metrics$type == "Spawners" & sps_metrics$region == regions[r], ]
	points(match(sr$species, species) + r.offset[r], sr$average_abundance*10^-3, col = regionsCol[r], pch = 19, cex = 2)
}

ind <- which(sps_metrics$average_abundance > 1500*10^3 & sps_metrics$type == "Spawners")
arrows(x0 = 4 + r.offset[match(sps_metrics$region[ind], regions)], x1 = 4 + r.offset[match(sps_metrics$region[ind], regions)], y0 = 1500, y1 = 1550, col = regionsCol[match(sps_metrics$region[ind], regions)], lwd = 5, length = 0.08, xpd = NA)

