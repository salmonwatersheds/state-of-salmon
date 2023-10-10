##############################################################################
# This code compiles region-specific run size and escapement information into 
# a single data file
# 
# Author: Steph Peacock
# Date: Sept 25, 2023
###############################################################################

# Clear workspace (I rarely do this in a script, but need to avoid have variables
# spawners and numStreams already in environment)
rm(list=ls())
library(dplyr)
library(abind)

# Generation length by species and region (for smoothing)
genLength <- read.csv("data/gen_length_regions.csv") 

#-----------------------------------------------------------------------------
# Fraser: sockeye
#-----------------------------------------------------------------------------
se <- read.csv("data/Total Fraser_run_size_2023-09-20.csv")

abund_se <- data.frame(
	region = rep("Fraser", length(se$Year)),
	species = rep("Sockeye", length(se$Year)),
	year = se$Year,
	spawners = se$Spawning.Escapement,
	runsize = se$Run.Size,
	smoothedSpawners = genSmooth(
		abund = se$Spawning.Escapement,
		years = se$Year,
		genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Sockeye"]
	),
	smoothedRunsize = genSmooth(
		abund = se$Run.Size,
		years = se$Year,
		genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Sockeye"]
	)
)


#---
# Deep dive for PSAC: Fraser sockeye

colo <- c("#985A3F", "#64837B")
g <- genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Sockeye"]

abund3 <- c(current = tail(abund_se$smoothedRunsize, 1), 
						lastgen = tail(abund_se$smoothedRunsize, g + 1)[1],
						hist = mean(abund_se$smoothedRunsize)
)

(abund3[1] - abund3[3])/abund3[3]
(abund3[1] - abund3[2])/abund3[2]

fit <- lm(log(abund_se$smoothedRunsize) ~ abund_se$year)
dum <- predict(fit)
(exp(dum[130]) - exp(dum[1]))/exp(dum[130])

quartz(width = 8, height = 4.5, pointsize = 10)
plot(abund_se$year, abund_se$smoothedRunsize*10^-6, "l", lwd = 2, col = colo[1], bty = "l", las=1, xlab = "", ylab = "Run size (millions)")
lines(abund_se$year, abund_se$runsize*10^-6, col = colo[1], lty = 3)

points(max(abund_se$year), abund3[1]*10^-6, pch = 19, col = colo[1], cex = 1.5)
points(max(abund_se$year) - g, abund3[2]*10^-6, pch = 21, col = colo[1], cex = 1.5, bg = "white", lwd = 2)
abline(h = abund3[3]*10^-6, col = colo[1])

lines(abund_se$year, exp(dum)*10^-6, col = "#CB9882", lwd = 2)
points(abund_se$year[c(1, 130)], exp(dum[c(1, 130)])*10^-6, lwd = 2, pch = c(21, 19), col = "#CB9882", bg = "white", cex = 1.5)


# SPAWNERS
abund3 <- c(current = tail(abund_se$smoothedSpawners, 1), 
						lastgen = tail(abund_se$smoothedSpawners, g + 1)[1],
						hist = mean(abund_se$smoothedSpawners)
)

(abund3[1] - abund3[3])/abund3[3]
(abund3[1] - abund3[2])/abund3[2]

fit <- lm(log(abund_se$smoothedSpawners) ~ abund_se$year)
dum <- predict(fit)
(exp(dum[130]) - exp(dum[1]))/exp(dum[130])

quartz(width = 8, height = 4.5, pointsize = 10)
plot(abund_se$year, abund_se$smoothedSpawners*10^-6, "l", lwd = 2, col = colo[2], bty = "l", las=1, xlab = "", ylab = "Spawners (millions)")
lines(abund_se$year, abund_se$smoothedRunsize*10^-6, col = paste0(colo[1], 30), lwd = 2, xpd = NA)
lines(abund_se$year, abund_se$spawners*10^-6, col = colo[2], lty = 3)

points(max(abund_se$year), abund3[1]*10^-6, pch = 19, col = colo[2], cex = 1.5)
points(max(abund_se$year) - g, abund3[2]*10^-6, pch = 21, col = colo[2], cex = 1.5, bg = "white", lwd = 2)
abline(h = abund3[3]*10^-6, col = colo[2])

lines(abund_se$year, exp(dum)*10^-6, col = "#BBCBC7", lwd = 2)
points(abund_se$year[c(1, 130)], exp(dum[c(1, 130)])*10^-6, lwd = 2, pch = c(21, 19), col = "#BBCBC7", bg = "white", cex = 1.5)


spawners_se <- subset(spawners, region == "Fraser" & species == "Sockeye")
lines(spawners_se$year, spawners_se$smoothedSpawners*10^-6, lwd = 2)
#-----------------------------------------------------------------------------
# Fraser: pink
#-----------------------------------------------------------------------------

pk <- read.csv("data/pink_run_size_2023-09-20.csv")

pk.yrs <- pk$Year[1]:max(pk.yrs)

abund_pk <- data.frame(
	region = rep("Fraser", length(pk.yrs)),
	species = rep("Pink", length(pk.yrs)),
	year = pk.yrs,
	spawners = pk$Escapement[match(pk.yrs, pk$Year)],
	runsize = pk$Run.Size[match(pk.yrs, pk$Year)],
	smoothedSpawners = genSmooth(
		abund = pk$Escapement[match(pk.yrs, pk$Year)],
		years = pk.yrs,
		genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Pink"]
	),
	smoothedRunsize = genSmooth(
		abund = pk$Run.Size[match(pk.yrs, pk$Year)],
		years = pk.yrs,
		genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Pink"]
	)
)

#-----------------------------------------------------------------------------
# Fraser: Chinook
#-----------------------------------------------------------------------------

ck_spawn <- read.csv("data/Atlas2023/CK_esc_FINAL.csv")
ck_run <- read.csv("data/Atlas2023/CK_TotalRun_FINAL.csv") %>% 
	subset(group == "salish" & grepl("skagit", population) == FALSE & grepl("cowichan", population) == FALSE)

# Look at contribution of each population to run size
plot(ck_run$year, ck_run$tot_run, "n")
for(i in 1:4){
	points(ck_run$year[ck_run$population == unique(ck_run$population)[i]], ck_run$tot_run[ck_run$population == unique(ck_run$population)[i]], col = i)
}
tapply(ck_run$year, ck_run$year, length) # Have all four populations from 1989 onward

ck.yrs <- c(1989:2019)

abund_ck <- data.frame(
	region = rep("Fraser", length(ck.yrs)),
	species = rep("Chinook", length(ck.yrs)),
	year = ck.yrs,
	spawners = tapply(ck_run$w_spawn, ck_run$year, sum)[match(ck.yrs, as.numeric(names(tapply(ck_run$w_spawn, ck_run$year, sum))))],
	runsize = tapply(ck_run$tot_run, ck_run$year, sum)[match(ck.yrs, as.numeric(names(tapply(ck_run$tot_run, ck_run$year, sum))))],
	smoothedSpawners = rep(NA, length(ck.yrs)),
	smoothedRunsize = rep(NA, length(ck.yrs))
)

abund_ck$smoothedSpawners <- genSmooth(
	abund = abund_ck$spawners,
	years = abund_ck$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Chinook"]
)

abund_ck$smoothedRunsize <- genSmooth(
	abund = abund_ck$runsize,
	years = abund_ck$year,
	genLength = genLength$gen_length[genLength$region == "Fraser" & genLength$species == "Chinook"]
)

