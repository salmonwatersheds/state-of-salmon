###############################################################################
# Plotting spawners abundance for each region and species, calculating
# percent decline over most recent generation
# 
# July 6, 2023
# Steph Peacock (speacock@psf.ca)
###############################################################################
library(viridis)
# Source PSF and SWP colour palettes
# source("code/colours.R")

# sp_cols <- SWP_cols[c('salmon', 'clay', 'soil2', 'soil1', 'tidal', 'greengrey')]
sp_cols <- viridis(n = 5)
sp_cols2 <- viridis(n = 5, alpha = 0.5)

###############################################################################
# Load regional spawner abundance
###############################################################################

# Arrange regions from north to south
regions <- c("Yukon", "Transboundary", "Haida Gwaii", "Nass", "Skeena", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")

# Select region
r <- "Fraser"
r <- "Haida Gwaii"
r <- "Central Coast"
# source("code/regional-expansions.R")

# spawners <- readRDS(paste0("output/", r, "-spawners_nuseds.rds"))
spawners <- readRDS(paste0("output/", r, "-spawners.rds"))

yrs <- as.numeric(dimnames(spawners)[[3]])
n.yrs <- length(yrs)

species <- dimnames(spawners)[[2]]
n.species <- length(species)

if(r == "Fraser"){
	cu_abund <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% subset(region == "Fraser" & species_name == "Pink (odd)")
	cu_abund$estimated_count[cu_abund$year == 2021] <- 7827444.568
	if(3 == 2){
		# Check how estimated count from CU-level spawner abundance comapres to 
		# expanded spawners calculated from NuSEDS
		plot(cu_abund$year, cu_abund$estimated_count, ylim = c(0,4e7), xlab = "", ylab = "Spawners", bty = "l", main = "Fraser Pink (Odd)")
		points(cu_abund$year, cu_abund$observed_count, pch = 3)
		points(yrs[which(spawners[1, 4, ] > 0)], spawners[1, 4, which(spawners[1, 4, ] > 0)], pch = 3, col = 2)
		points(yrs, spawners[2, 4, ], col = 2, cex = 0.8)
		abline(v = c(2001.5, 2006.5, 2008.5), lty = 2)
		legend("top", ncol = 2, pch = c(3,1,3,1), col = c(1,1,2,2), legend = c("Obs. CU spawner abundance (PSE)", "Est. CU spawner abundance (PSE)", "Obs. regional sum (NuSEDS)", "Expanded regional sum (NuSEDS"), bg = "white")
	}
	
	spawners[2, 4, ] <- cu_abund$estimated_count[match(cu_abund$year, yrs)]
	
	# Check cu abundance for Fraser coho
	if(3 == 2){
		cu_abund <- read.csv("data/spawner_abundance.csv", na.strings = "-989898") %>% subset(region == "Fraser" & species_name == "Coho")
		plot(cu_abund$year, cu_abund$estimated_count, "n", xlim = c(1980, 2021), bty = "l", xlab = "", ylab = "Spawner abundance")
		cus <- sort(unique(cu_abund$cu_name_pse))
		for(i in 1:length(cus)){
			lines(cu_abund$year[cu_abund$cu_name_pse == cus[i]], cu_abund$estimated_count[cu_abund$cu_name_pse == cus[i]], "o", col = viridis(n = length(cus))[i], lwd = 1.5)
		}
		legend("topright", bty = "n", col = viridis(n = length(cus)), lwd = 1.5, legend = cus)
	}
}
###############################################################################
# Smoothing
###############################################################################

# Smooth abundance using the geometric running mean over the generation length
g <- read.csv("data/gen_length_regions.csv") %>% subset(region == r)
genLength <- g$gen_length; names(genLength) <- g$species
rm(g)

# Calculate geometric average
smoothedSpawners <- array(NA, dim = c(n.species, n.yrs))
for(s in 1:n.species){
	g <- as.numeric(genLength[species[s]])
	for(k in 1:n.yrs){ # For each year
		smoothYrs <- c(max(yrs[1], yrs[k] - g + 1):yrs[k]) # define previous years over which to smooth
		# Unweighted geometric mean
		S <- spawners[2, s, yrs %in% smoothYrs]
		Y <- sum(!is.na(S))
		if(Y > 0){ # If there are no data, leave as NA
			smoothedSpawners[s, k] <- prod(S, na.rm = TRUE) ^ (1/Y)
		}
	}
}

###############################################################################
# Main abdunance plot
###############################################################################

baseline <- 1980

# Which year is the baseline? 
quartz(width = 8, height = 4, pointsize = 12)
par(mar = c(4,4,2,10))

plot(yrs, smoothedSpawners[s, ]/smoothedSpawners[s, which(yrs == baseline)], "n", las = 1, ylim = range(smoothedSpawners[, which(yrs > 1970)]/smoothedSpawners[, which(yrs == baseline)], na.rm = TRUE), ylab = "Proportional spawners", xlab = "", main = r, bty = "l", xlim = c(1970, 2021))
# plot(yrs, smoothedSpawners[s, ]/smoothedSpawners[s, which(yrs == baseline)], "n", las = 1, ylim = c(0,3), ylab = "Proportional spawners", xlab = "", main = r, bty = "l", xlim = c(1970, 2021))

# abline(v = baseline, lty = 2)
for(s in 1:length(species)){
	lines(yrs, smoothedSpawners[s, ]/smoothedSpawners[s, which(yrs == baseline)], col = sp_cols[s], lwd = 2)
}

percDecline <- rep(NA, n.species)
for(s in 1:n.species){
	points(max(yrs), smoothedSpawners[s, which(yrs == max(yrs))]/smoothedSpawners[s, which(yrs == baseline)], col = sp_cols[s], pch = 19, cex = 1.5)
	
	g <- genLength[s]
	points(max(yrs) - g, smoothedSpawners[s, which(yrs == (max(yrs) - g))]/smoothedSpawners[s, which(yrs == baseline)], col = sp_cols[s], pch = 21, bg = "white", cex = 1.5, lwd = 2)
	percDecline[s] <- (smoothedSpawners[s, which(yrs == max(yrs))] - smoothedSpawners[s, which(yrs == (max(yrs) - g))]) / smoothedSpawners[s, which(yrs == (max(yrs) - g))]
}

# Add plus if positive
percDecline2 <- round(percDecline*100)
percDecline2[which(percDecline2 > 0)] <- paste0("+", percDecline2[which(percDecline2 > 0)])
# legend(u[2] + 0.01*(u[2]-u[1]), u[4], col = col_sp, lwd = 2, species, xpd = NA, bty = "n")
y <- smoothedSpawners[, n.yrs]/smoothedSpawners[, which(yrs == baseline)]
if(r == "Nass"){
	y[2] <- 0
	y[1] <- 0.6
	y[5] <- 1.2
	y[3] <- 2
} else if(r == "Skeena"){
# y[1] <- 1.8 # PSE database
# y[5] <- 2.3 # PSE database
	y[1] <- 2
	y[5] <- 0.5
	y[3] <- 1
	y[4] <- 1.5
} else if(r == "Fraser"){
	y[3] <- 1.5
} else if(r == "Haida Gwaii"){
	y[4] <- 0.1
	y[5] <- 0.7
} else if(r == "Central Coast"){
	y[1] <- 0.35
	y[5] <- 0.2
}
text(max(yrs) + 3, y, paste0(species, " (", percDecline2, "%)"), col = sp_cols, xpd = NA, adj = 0, font = 2)

###############################################################################
# Supplemental exploratory plots
###############################################################################

#------------------------------------------------------------------------------
# Plot expansion
#------------------------------------------------------------------------------
par(mar = c(3, 5, 2, 1))
plot(yrs, spawners[2, s, ]*10^-3, "n", las = 1,  ylab = "Spawner abundance (thousands)", xlab = "", main = paste(r, species[s]), bty = "l")
lines(yrs, spawners[1, s, ]*10^-3, "o", pch = 19, col = "#C17E43")
lines(yrs, spawners[2, s, ]*10^-3, "o", pch = 19, col = "#6578AD")
legend("topright", pch = 19, col = c("#C17E43", "#6578AD"), c("Observed", "Estimated"), bty = "n")

#------------------------------------------------------------------------------
# log transform?
#------------------------------------------------------------------------------

percBasline <- smoothedSpawners/smoothedSpawners[, which(yrs == baseline)]

par(mar = c(4,4,2,10))

plot(yrs, percBasline[s, ], "n", las = 1, ylim = range(percBasline[, which(yrs > 1970)], na.rm = TRUE), ylab = "Proportional spawners", xlab = "", main = r, bty = "l", xlim = c(1970, 2021))
abline(h = 1)
for(s in 1:length(species)){
	lines(yrs, percBasline[s, ], col = sp_cols[s], lwd = 2)
}

logSpawners <- log(smoothedSpawners / smoothedSpawners[, which(yrs == baseline)] + 0.1)

par(mar = c(4,4,2,10))

plot(yrs, logSpawners[s, ], "n", las = 1, ylim = range(logSpawners[, which(yrs > 1970)], na.rm = TRUE), ylab = "log spawners", xlab = "", main = r, bty = "l", xlim = c(1970, 2021))
abline(h = 0)
for(s in 1:length(species)){
	lines(yrs, logSpawners[s, ], col = sp_cols[s], lwd = 2)
	# par(new = TRUE)
	# plot(yrs, logSpawners[s, ], "l", col = sp_cols[s], lwd = 2, xlab = "n", ylab = "n", bty = "n", xlim = c(1990, 2021), xaxt = "n", yaxt = "n")
}

#------------------------------------------------------------------------------
# Fraser coho estimated vs observed
#------------------------------------------------------------------------------
s <- which(species == "Coho")
plot(yrs, spawners[1, s, ], pch = 3, ylim = range(spawners[, s, ], na.rm = TRUE), xlim = c(1970, 2021), bty = "l", xlab = "", ylab = "Spawner abundance", main = paste(r, species[s]))
segments(x0 = yrs, x1 = yrs, y0 =  spawners[1, s, ], y1 =  spawners[2, s, ], col = sp_cols2[s], lwd = 5)
points(yrs, spawners[2, s, ], pch = 21, bg = "white")
legend(2025, mean(par('usr')[3:4]), pch = c(3, 1, NA), lwd = c(NA, NA, 5), col = c(1, 1, sp_cols2[s]), legend = c("Observed", "Estimated", "Expansion"), xpd = NA)
