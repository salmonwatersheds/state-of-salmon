statusCols <- rev(c(extinct = "#604848",
								poor = "#B46764",
								fair = "#DDD490",
								good = "#81A47A",
								datadeficient = "#D8D8D8"
								))

cu <- read.csv("data/conservationunits_revised.csv") %>%
	subset(SPECIES_QUALIFIED %in% c("SEL", "SER") & region == "Fraser")


###############################################################################
# Distribution of 3-gen trends
###############################################################################

trends <- read.csv("data/dataset391.csv", na.string = c(-989898))
trends$species_pooled <- trends$species_name
trends$species_pooled[trends$species_pooled %in% c("Pink (even)", "Pink (odd)")] <- "Pink"
trends$species_pooled[trends$species_pooled %in% c("Lake sockeye", "River sockeye")] <- "Sockeye"

status <- read.csv("data/status.csv")

regions <- c("Yukon", "Transboundary", "Nass", "Skeena", "Haida Gwaii", "Central Coast", "Vancouver Island & Mainland Inlets", "Fraser", "Columbia")
species <- c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Steelhead")
status_out <- unique(status$psf_status)

status_mat <- list(); length(status_mat) <- length(regions); names(status_mat) <- regions

for(r in 1:length(regions)){
	status.r <- status[which(status$region == regions[r]), c("species_pooled", "psf_status")]
	status_mat[[r]] <- matrix(data = NA, nrow = 6, ncol = 5, dimnames = list(species, status_out))
	
	for(s in 1:6){
		dum <- tapply(status.r$psf_status[status.r$species_pooled == species[s]], status.r$psf_status[status.r$species_pooled == species[s]], length)
		status_mat[[r]][s, match(names(dum), status_out)] <- dum
	}
	status_mat[[r]][which(is.na(status_mat[[r]]), arr.ind = TRUE)] <- 0
}

 # plot
par(mfrow = c(3,3), oma = c(0, 3, 3, 0))
for(r in 1:length(regions)){
	bp <- barplot(t(status_mat[[r]]/apply(status_mat[[r]], 1, sum, na.rm = TRUE)), col = statusCols, main = regions[r])
	# mtext(side = 3, line = 1.5, )
	text(bp, 0.05, xpd = NA, apply(status_mat[[r]], 1, sum, na.rm = TRUE), font = 2, cex = 1)
}
mtext(side = 2, line = 1, outer= TRUE, "Proportion of Conservation Units")
mtext(side = 3, line = 1, outer = TRUE, "Distribution of Biological Status Outcomes", font = 2)

#------------------------------------------------------------------------------
# 3-gen trends
#------------------------------------------------------------------------------

trends_mat <- list(); length(trends_mat) <- length(regions); names(trends_mat) <- regions

for(r in 1:length(regions)){
	trends.r <- trends[which(trends$region == regions[r]), c("species_pooled", "threegen_slope")]
	trends_mat[[r]] <- matrix(data = NA, nrow = 6, ncol = 3, dimnames = list(species, c("data-deficient", "declining", "increasing")))
	
	for(s in 1:6){
		if(length(which(trends.r$species_pooled == species[s])) > 0){
		trends_mat[[r]][s, ] <- c(dd = sum(is.na(trends.r$threegen_slope[trends.r$species_pooled == species[s]])),
						 declining = length(which(trends.r$threegen_slope[trends.r$species_pooled == species[s]] <= 0)),
						 increasing = length(which(trends.r$threegen_slope[trends.r$species_pooled == species[s]] > 0)))
		} else {
			trends_mat[[r]][s, ] <- 0
		}
	}
	# trends_mat[[r]][which(is.na(trends_mat[[r]]), arr.ind = TRUE)] <- 0
}

# plot
par(mfrow = c(3,3), oma = c(0, 3, 3, 0))
for(r in 1:length(regions)){
	bp <- barplot(t(trends_mat[[r]]/apply(trends_mat[[r]], 1, sum, na.rm = TRUE)), col = c(statusCols['datadeficient'], "#44AA99", "#88CCEE"), main = regions[r])
	# mtext(side = 3, line = 1.5, )
	text(bp, 0.05, xpd = NA, apply(trends_mat[[r]], 1, sum, na.rm = TRUE), font = 2, cex = 1)
	if(r == 1) legend("topright", fill = c(statusCols['datadeficient'], "#44AA99", "#88CCEE"), c("Data deficient", "Declining", "Increasing"))
}
mtext(side = 2, line = 1, outer= TRUE, "Proportion of Conservation Units")
mtext(side = 3, line = 1, outer = TRUE, "Distribution of 3-Generation Trends in Spawner Abundance", font = 2)


###############################################################################
# Distribution of 3-gen trends
###############################################################################

trends <- read.csv("data/dataset391.csv") %>%
	subset(region == "Fraser" & Species %in% c("SEL", "SER"))

head(trends)


hist(trends$X3gen_slope)

cu$trend <- NA
cu$trend[match(trends$CUID, cu$cuid)] <- trends$X3gen_slope
cu$trend[grep("Extinct", cu$cuname)] <- -989898


trend_cat <- c(pos = length(which(cu$trend[!is.na(cu$trend)] > 0)),
							 neg = length(which(cu$trend[!is.na(cu$trend)] <= 0)),
							 dd = sum(is.na(cu$trend)),
							 extinct = length(which(cu$trend == -989898)))



barplot(trend_cat, beside = FALSE, col = c(statusCols[c(1,3)], grey(0.6), statusCols[4]), las = 1, ylab = "Number of CUs", xlab = "Trend over Most Recent 3 Generations", main = "Fraser Sockeye")

dummy_trends <-  cbind(Chinook = c(4, 5, 12, 0), Chum = c(1, 2, 2, 0), Coho = c(4, 8, 12, 2), Pink = c(1, 0, 0, 0), Sockeye = trend_cat, Steelhead = c(0, 2, 5, 1))

barplot(dummy_trends, beside = FALSE, col = c(statusCols[c(1,3)], grey(0.6), statusCols[4]), las = 1, ylab = "Number of CUs", xlab = "Trend over Most Recent 3 Generations", main = "Fraser Region", space = 0.1)
legend(0, 45, xpd = NA, fill = c(statusCols[c(1,3)], grey(0.6), statusCols[4]), legend = c("Positive", "Negative", "Data Deficient", "Extinct"), bty = "n")

barplot(t(t(dummy_trends)/apply(dummy_trends, 2, sum)), beside = FALSE, col = c(statusCols[c(1,3)], grey(0.6), statusCols[4]), las = 1, ylab = "Proportion of CUs", xlab = "Trend over Most Recent 3 Generations", main = "Fraser Region", space = 0.1)

###############################################################################
# Coefficient of variation
###############################################################################

spawner_surveys <- read.csv("data/spawner_surveys.csv", na.strings = -989898) %>%
	subset(region == "Fraser")


unique(spawner_surveys$species_name)
spawner_surveys$species_pooled <- spawner_surveys$species_name
spawner_surveys$species_pooled[spawner_surveys$species_pooled %in% c("Lake sockeye", "River sockeye")] <- "Sockeye"
spawner_surveys$species_pooled[spawner_surveys$species_pooled == "Pink (odd)"] <- "Pink"

# Set 0 observed to NA
spawner_surveys$stream_observed_count[spawner_surveys$stream_observed_count == 0] <- NA
spawner_surveys <- spawner_surveys[!is.na(spawner_surveys$stream_observed_count), ]


# subset to only include streams with 10+ years of data

dum <- tapply(spawner_surveys$stream_observed_count[spawner_surveys$species_pooled == "Sockeye"], spawner_surveys$stream_name_pse[spawner_surveys$species_pooled == "Sockeye"], length)
keepStreams <- names(dum)[as.numeric(which(dum > 10))]
spawner_surveys <- spawner_surveys[which(spawner_surveys$stream_name_pse %in% keepStreams), ]
#---
# CU-level

spawner_abundance <- read.csv("data/spawner_abundance.csv", na.strings = -989898) %>%
	subset(region == "Fraser")
spawner_abundance$species_pooled <- spawner_abundance$species_name
spawner_abundance$species_pooled[spawner_abundance$species_pooled %in% c("Lake sockeye", "River sockeye")] <- "Sockeye"
spawner_abundance$species_pooled[spawner_abundance$species_pooled == "Pink (odd)"] <- "Pink"


spawner_region <- read.csv("data/Total Fraser_run_size_2023-09-20.csv")

cv_sockeye <- list(
	stream = tapply(log(spawner_surveys$stream_observed_count[spawner_surveys$species_pooled == "Sockeye"]), spawner_surveys$stream_name_pse[spawner_surveys$species_pooled == "Sockeye"], sd, na.rm = TRUE),
	cu = tapply(log(as.numeric(spawner_abundance$estimated_count[spawner_abundance$species_pooled == "Sockeye"])),
							spawner_abundance$cu_name_pse[spawner_abundance$species_pooled == "Sockeye"], sd, na.rm = TRUE),
	region = sd(log(spawner_region$Spawning.Escapement[spawner_region$Management.Group == "Total Fraser" & spawner_region$Year > 1950]), na.rm = TRUE)
)


hist(cv_sockeye$stream, breaks = seq(0, 4, 0.1), freq = FALSE, yaxs = "i", xaxs = "i", main = "Standard deviation in \nlog Fraser sockeye spawner abundance", xlab = "Standard deviation")
lines(density(cv_sockeye$stream[!is.na(cv_sockeye$stream)]), lwd = 2)

hist(cv_sockeye$cu, , breaks = seq(0, 4, 0.1), freq = FALSE, col = "#FF000030", border = 2, add = TRUE)
lines(density(cv_sockeye$cu[!is.na(cv_sockeye$cu)]), lwd = 2, col = 2)

arrows(x0 = cv_sockeye$region, x1 = cv_sockeye$region, y0 = 1, y1 = 0, col = 4, length = 0.08, lwd = 2)

# arrows(x0 = 35000, x1 = 40000, y0 = 3e-4, y1 = 3e-4, col = 4, length = 0.08, lwd = 2)
# text(32000, 3e-4, round(cv_sockeye$region), col = 4, font = 2)

legend(3, 1.1, col = c(1, 2, 4), lwd = 2, c("Stream", "CU", "Region"), bty = "n")

library(gplots)
plotCI(1:3, c(median(cv_sockeye$stream, na.rm = TRUE), median(cv_sockeye$cu, na.rm = TRUE), cv_sockeye$region), li = c(quantile(cv_sockeye$stream, 0.025, na.rm = TRUE), quantile(cv_sockeye$cu, 0.025, na.rm = TRUE), cv_sockeye$region), ui = c(quantile(cv_sockeye$stream, 0.975, na.rm = TRUE), quantile(cv_sockeye$cu, 0.975, na.rm = TRUE), cv_sockeye$region), col = c(1, 2, 4), gap = 0, pch = 19, cex = 1.5, lwd = 2, xlim = c(0.5, 3.5), xaxt = "n", xlab = "", ylab= "SD in log(spawners)", las = 1)
abline(h = seq(0, 4, 0.5), col = "#00000030")
axis(side = 1, at = c(1:3), c("Stream", "CU", "region"))
