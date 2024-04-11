# Extra figures
# Apr 9, 2024
species <- unique(sps_dat$species)
###############################################################################
# Fishy dot plot
###############################################################################
quartz(width = 8, height = 4.5, pointsize = 10)
yr <- range(sps_metrics$status[sps_metrics$type == "Spawners"], na.rm = TRUE)
par(mar = c(2, 5, 2, 1))
plot(c(0.5, 6.5), c(-120, 140), "n", yaxt = 'n', ylab = "Salmon Abundance", xaxt = "n", xlab = "", xaxs = "i")
axis(side = 2, at = seq(-100, 100, 50), las = 1)
abline(h = 0, lwd = 5, col = SWP_cols['stone1'])
axis(side = 2, at = -115, "Unknown", las = 1)
for(s in 1:6){
	text(s, 148, species[s], col = species_cols_dark[s], xpd = NA, pos = 3)
	polygon(x = s + c(-0.45, -0.45, 0.45, 0.45),
					y = c(-100, 200, 200, -100),
					col = paste0(species_cols_light[s], 50),
					border = NA)
	polygon(x = s + c(-0.45, -0.45, 0.45, 0.45),
					y = c(-200, -105, -105, -200), 
					col = grey(0.8),
					border =NA)
	
	ind <- which(sps_metrics$species == species[s] & sps_metrics$type == "Spawners")
	ptCol <- ifelse(sps_metrics$status[ind] > 0, status_cols['green'], status_cols['red'])
	points(seq(s-0.3, s+0.3, length.out = 9), sps_metrics$status[ind]*100, pch = 19, cex = 2, col = ptCol)
	points(seq(s-0.3, s+0.3, length.out = 9), ifelse(is.na(sps_metrics$status[ind]), -115, NA), cex = 2, col = grey(0.5), pch = 19)
	
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

# legend(-0.2, 180, pch = c(19, NA), pt.cex = 2, c("Spawners", ""), xpd = NA, bty = "n")

legend(-0.2, 180, pch = c(19, 1), pt.cex = 2, c("Spawners", "Run size"), xpd = NA, bty = "n")

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
