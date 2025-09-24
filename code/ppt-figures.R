library(readxl)
library(dplyr)

inseas <- read_xlsx("data/SockPink_Passage_Summary_2025-09-22.xlsx", range = "A15:F134")
head(inseas)

# pre-season run size forecast
forecasts <- read.csv("data/fraser_forecast_retrospective.csv")
forecast_pre <- forecasts$p50[which(forecasts$species == "sockeye" & forecasts$year == 2025)]

quartz(width = 9, height = 4)
par(family = "Sofia Pro Light", mar = c(3,5,2,1))


plot(inseas$`Mission Date`, inseas$`Cummulative Estimated Passage`*10^-6, "n", xlab = "", ylab = "Fraser sockeye\ncumulative count (millions)", bty = "l", las = 1)
abline(h = forecast_pre*10^-6, col = "#74BDB8")

plot(inseas$`Mission Date`, inseas$`Cummulative Estimated Passage`*10^-6, "l", col = "#74BDB8", lwd = 3, xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")


plot(inseas$`Mission Date`, inseas$`Cummulative Estimated Passage`*10^-6, "n", xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")
abline(h = forecast_pre, col = "#D32314", lwd = 2)


frse <- read.csv("data/Fraser Sockeye Run Size_2025-04-24.csv") %>%
	filter(Management.Group == "Total Fraser")

plot(frse$Year, frse$Run.Size*10^-6, "l", xlab = "", ylab = "Fraser sockeye\ncumulative count (millions)", bty = "l", las = 1, col = "#74BDB8", lwd = 2)
abline(h = forecast_pre, col = "#D32314", lwd = 2)
points(2025, 7.94200, col = "#74BDB8", pch = 19)
cond_gen <- which(frse$Year %in% seq(2025-4*40, 2025, 4))
points(frse$Year[cond_gen], frse$Run.Size[cond_gen]*10^-6, pch = 19, col = "#74BDB8")

plot(frse$Year, frse$Run.Size*10^-6, "n", xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")

#-------------
# Plot of raw vs smoothed for later
sps_dat.frse <- read.csv("output/sps-data.csv") %>% filter(region == "Fraser", species == "Sockeye")
quartz(width = 5, height = 2.5, pointsize = 12)
par(mfrow = c(1,1), mar = c(3,3,1,0), cex.lab = 1, family = "Sofia Pro Light")
plot(frse$Year, frse$Run.Size*10^-6, "l", xlab = "", ylab = "", bty = "l", las = 1, col = "#74BDB8", lwd = 1, yaxt = "n")
mtext(side = 2, line = 0.5, "Fraser sockeye\ntotal abundance")
u <- par('usr')
arrows(x0 = u[1], x1 = u[1], y0 = 0, y1 = u[4], length = 0.08, xpd = NA)
points(2010, frse$Run.Size[which(frse$Year == 2010)]*10^-6, pch = 19, col = "#74BDB8")


plot(frse$Year, frse$Run.Size*10^-6, "n", xlab = "", ylab = "", bty = "n", yaxt = "n", xaxt = "n")
lines(sps_dat.frse$year, sps_dat.frse$smoothedRunsize*10^-6, lwd = 2, col = "#74BDB8")

#-------------
# Tim's question: How do pre-season forecasts compare?
# Forecast data from first weekly reports of each year: https://www.psc.org/publications/fraser-panel-in-season-information/fraser-river-panel-weekly-reports/
forecasts_se <- forecasts %>% filter(species == "sockeye")
par(mfrow = c(2,1), mar = c(4, 4, 2,1), oma = c(0,1,2,0))
plot(sps_dat.frse$year, sps_dat.frse$runsize*10^-6, "o", xlab = "", ylab = "Abundance (millions)", bty = "l", las = 1, col = "#74BDB8", lwd = 2, xlim = c(2015.5, 2025), ylim = c(0, max(forecasts$p75[which(forecasts$species == "sockeye")], na.rm = TRUE)), pch = 21, cex = 2, bg = "white")
points(2025, 7.94200, col = "#74BDB8", pch = 19, cex = 2)
lines(c(2024, 2025), c(0.473346, 7.922600), col = "#74BDB8", lty = 2)

segments(x0 = forecasts_se$year, x1 = forecasts_se$year, y0 = forecasts_se$p25, y1 = forecasts_se$p75, col = "#D3231460", lwd = 5)
# points(sps_dat.frse$year, sps_dat.frse$runsize*10^-6, pch = 21, cex = 2, col = "#74BDB8", lwd = 2, bg = "white")
points(forecasts_se$year, forecasts_se$p50, pch = 19, col = "#D32314")

legend(2021, 30, lwd = c(2, NA, 10), col = c("#74BDB8", "#D32314", "#D3231460"), pch = c(21,19, NA), pt.cex = c(2, 1, NA), legend = c("Post-season estimate", "Pre-season forecast (p50)", "50% probability envelope (p25 to p75)"), pt.bg = c("white", NA, NA), xpd = NA, bty = "n")
mtext(side = 3, line = 1, adj = 0, "Fraser sockeye salmon", cex = 1.5)

# Pink
sps_dat.frpk <- read.csv("output/sps-data.csv") %>% filter(region == "Fraser", species == "Pink")
forecasts_pk <- forecasts %>% filter(species == "pink")
plot(sps_dat.frpk$year, sps_dat.frpk$runsize*10^-6, "o", xlab = "", ylab = "Abundance (millions)", bty = "l", las = 1, col = "#74BDB8", lwd = 2, xlim = c(2015.5, 2025), ylim = c(0, max(forecasts$p75[which(forecasts$species == "pink")], na.rm = TRUE)), pch = 21, cex = 2, bg = "white")
points(2025, 17.623800, col = "#74BDB8", pch = 19, cex = 2)
lines(c(2023, 2025), c(10.513292, 17.623800), col = "#74BDB8", lty = 2)

segments(x0 = forecasts_pk$year, x1 = forecasts_pk$year, y0 = forecasts_pk$p25, y1 = forecasts_pk$p75, col = "#D3231460", lwd = 5)
# points(sps_dat.frse$year, sps_dat.frse$runsize*10^-6, pch = 21, cex = 2, col = "#74BDB8", lwd = 2, bg = "white")
points(forecasts_pk$year, forecasts_pk$p50, pch = 19, col = "#D32314")
mtext(side = 3, line = 1, adj = 0, "Fraser pink salmon", cex = 1.5)


###############
# animation

library(animation)

# check the animation options with
ani.options()


# create a gif with no controls
saveGIF({
	x <- inseas$`Mission Date`
	y <- rep(forecast_pre, length(inseas$`Mission Date`))

	for(i in 1:length(y)){
		new_y <- c(inseas$`Cummulative Estimated Passage`[1:i], rep(NA, length(y) - i))
		coly <- ifelse(new_y < forecast_pre, 2, 3)
		plot(x, new_y*10^-6, "n", ylim = c(0,10))
		lines(x, new_y*10^-6)
		points(x, new_y*10^-6, pch = 19, col = coly)
	}
	
}, title = "Fraser sockeye", 
description = "Time series of in season Fraser sockeye counts at Mission", verbose = FALSE,
convert = 'gm convert', movie.name = 'output/ignore/presentation/Fraser_sockeye.gif')

#############################
# Central Coast chinook


#
ss <- read.csv("data/spawner_surveys_revised2025.csv") %>% filter(region == "Central Coast", species_name == "Chinook")

ss.avg <- ss %>% group_by(streamid) %>% summarise(ss.avg = exp(mean(log(stream_observed_count), na.rm = TRUE)))

ss <- ss %>% left_join(ss.avg)

yrs <- c(1950:2024)

quartz(width = 8, height = 4, pointsize = 12)
par(mar = c(3,4,1,1), family = "Sofia Pro Light")
plot(c(1950, 2024), c(0, max(ss$stream_observed_count)), "n")
for(i in 1:length(unique(ss$streamid))){
	streamid.i <- unique(ss$streamid)[i]
	ss.i <- ss %>% filter(streamid == streamid.i)
	y.i <- ss.i$stream_observed_count[match(yrs, ss.i$year)]
	
	points(yrs, y.i, "o", col = "#00000020")
}

# As % anomaly
plot(c(1950, 2024), c(-10, 50), "n")
for(i in 1:length(unique(ss$streamid))){
	streamid.i <- unique(ss$streamid)[i]
	ss.i <- ss %>% filter(streamid == streamid.i)
	ss.avgi <- ss.avg$ss.avg[ss.avg$streamid == streamid.i]
	y.i <- ss.i$stream_observed_count[match(yrs, ss.i$year)]
	
	points(yrs, (y.i - ss.avgi)/ss.avgi, "o", pch = 19, cex = 0.6, col = "#00000030")
}

# Expanded spawner estimate
ccck_ex <- readRDS("output/expanded-spawners/Central Coast-spawners.rds")[, "Chinook", ]
yrs <- as.numeric(names(ccck_ex))
e <- readRDS("output/expanded-spawners/Central Coast-expansion-factors.rds")[[1]]

counted.y <- ss %>% group_by(year) %>% summarise(total_count = sum(stream_observed_count))

plot(yrs, ccck_ex[2, ], "l")
plot(yrs, ccck_ex[1, ]/ccck_ex[2, ], "l", ylim = c(0,1))
lines(yrs, counted.y$total_count/ccck_ex[2, ], col = 2)


plot(yrs, ccck_ex[2, ]*10^-3, "l", lwd = 2, col = "#0D1E3380", bty = "l", las = 1, ylab = "Spawner abundance (thousands)", ylim = c(0, 130), yaxs = "i", xaxs = "i")
abline(h = exp(mean(log(ccck_ex[2, ]), na.rm = TRUE))*10^-3, lty = 2)

# plot(yrs, ccck_ex[2, ]*10^-3, "n", ylab = "", xlab = "", ylim = c(0, 130), yaxs = "i", xaxs = "i", bty = "n", xaxt = "n", yaxt = "n")

polygon(x = c(yrs, rev(yrs)),
				y = c(ccck_ex[1, ]*10^-3, rep(0, length(yrs))),
				border =NA, col = "#0D1E3320")

lines(sps_dat$year[sps_dat$region == "Central Coast" & sps_dat$species == "Chinook"], sps_dat$spawners[sps_dat$region == "Central Coast" & sps_dat$species == "Chinook"]*10^-3, col = "#D32314", lwd = 3)
abline(h = exp(mean(log(sps_dat$spawners[sps_dat$region == "Central Coast" & sps_dat$species == "Chinook"]), na.rm = TRUE))*10^-3, lty = 2, col = "#D32314")

# TCCHINOOK Table B3
cc_ctc <- readxl::read_xlsx("data/TCCHINOOK-25-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B3", range = "H5:K54", col_types = "numeric", col_names = c("Atnarko_Total_esc", "Atnarko_CV", "Atnarko_Wild", "Rivers_Inlet")) %>%
	mutate(Year = c(1975:2024))

par(mar = c(3,4,2,4))
plot(yrs, e$exp1, pch = 4, bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = grey(0.6), ylim = c(0, 30), lwd = 2)
axis(side = 4, col = grey(0.6), las = 1)
mtext(side = 4, line = 2.5, "Expansion factor x", col = grey(0.6))
par(new = TRUE)

plot(yrs, ccck_ex*10^-3, "o", pch = 21, bg = "white", bty = "l", xlab = "", ylab = "Spawner abundance (thousands)", ylim = c(0, 150), las = 1, yaxs = "i")
abline(v = seq(1950, 2025, 5), lty = 3, col = grey(0.8))
abline(h = seq(0, 150, 50), lty = 3, col = grey(0.8))
points(cc_ctc$Year, cc_ctc$Atnarko_Total_esc*10^-3, "o", pch = 21, bg = "white", col = 2)
points(cc_ctc$Year, cc_ctc$Rivers_Inlet*10^-3, "o", pch = 21, bg = "white", col = 4)
points(cc_ctc$Year, cc_ctc$Atnarko_Total_esc*10^-3 + cc_ctc$Rivers_Inlet*10^-3, pch = 19, col = "purple", cex = 0.6)

legend("topleft", col = c(1,2,4, "purple"), c("NuSEDS expansion", "CTC Atnarko Total esc.", "CTC Rivers Inlet index esc.", "CTC sum"), pch = c(rep(21, 3), 19), pt.bg = "white", lwd = c(rep(1, 3),NA), bty = "n")

# Central coast map
spawner_surveys <- readRDS("output/ignore/spawner_surveys_CC.rds")
# 2025 boundaries split EVIMI and WVI
pse_regions <- readRDS("data/ignore/pse-regions-2025/pse-regions-2025.rds")


# Subset region of interest
pse_region <- pse_regions[which(pse_regions$regionname == "Central Coast"), ]

spawner_surveys_ck <- spawner_surveys %>% filter(species_name == "Chinook")

length(unique(spawner_surveys_ck$streamid))
length(unique(spawner_surveys_ck$streamid[which(spawner_surveys_ck$indicator == "Y")]))
unique(spawner_surveys_ck$stream_name_pse[which(spawner_surveys_ck$indicator == "Y")])

stream_points_ck <- data.frame(
	streamid = spawner_surveys_ck$streamid,
	lon = spawner_surveys_ck$longitude, 
	lat = spawner_surveys_ck$latitude) %>%
	st_as_sf(coords = c("lon", "lat"), crs = 4269)


quartz(width = 5, height = 5)
par(bg = "white")
plot(st_geometry(pse_region), col = NA, border = NA)
plot(st_geometry(pse_regions), col = paste0(PSF_cols["tidal"], 60), border = grey(0.6), add = TRUE)
plot(st_geometry(pse_region), col = "#0D1E33", border = NA, add = TRUE)
plot(st_geometry(stream_points_ck), pch = 21, bg = PSF_cols['activeyellow'], col = "white", cex = 1, add = TRUE)
plot(st_geometry(stream_points_ck[stream_points_ck$streamid %in% spawner_surveys_ck$streamid[which(spawner_surveys_ck$indicator == "Y")],]), pch = 21, bg = PSF_cols['tidal'], col = "white", cex = 1.2, add = TRUE)


plot(st_geometry(stream_points_ck), pch = 1, col = PSF_cols['activeyellow'], cex = 0.8, add = TRUE)
plot(st_geometry(stream_points_ck[stream_points_ck$streamid %in% spawner_surveys_ck$streamid[which(spawner_surveys_ck$indicator == "Y")],]), pch = 21, bg = PSF_cols['activeyellow'], col = "white", cex = 1.2, add = TRUE)
]# Add LT avg
spawner_surveys_ck <- spawner_surveys_ck %>% left_join(spawner_surveys_ck %>%
																 	group_by(streamid) %>%
																 	summarise(avg = exp(mean(log(stream_observed_count), na.rm = TRUE)))
)


# Calc perc anomaly

# Plot Wannock?

ss_i <- spawner_surveys_ck %>% filter(stream_name_pse == "KILBELLA RIVER")
ss_i <- spawner_surveys_ck %>% filter(stream_name_pse == "WANNOCK RIVER")

plot(ss_i$year, ss_i$stream_observed_count, "o")

quartz(width = 9, height = 5)
par(family = "Sofia Pro Light")

################################################################
# Highlights report Fishy dot plot
k <- 1
fishwidth <- 1
fishheight <- fishwidth * image_info(fish)$height/image_info(fish)$width


par(mar = rep(0,4), family = "Sofia Pro Semi Bold")
plot(1,1,"n", xlab = "", ylab = "", bty = "n", xaxt  = "n", yaxt = "n", xlim = c(0.02,5.98), ylim = c(0.02, 9.98), xaxs = "i", yaxs = "i")
for(r in 1:10){
	for(s in 1:6){
		region.r <- rev(regions)[r]
		species.s <- species[s]
		type.k <- c("Spawners", "Total return")[k]
		summ.rsk <- sps_summary %>% filter(region == region.r & species == species.s & type == type.k)
		
		
		if(dim(summ.rsk)[1] > 0){
			
			if(!is.na(summ.rsk$current_status) & summ.rsk$current_status == -999999){
				polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['crit'], border = NA)
			} else {
				polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['bg'], border = NA)
			}
			
			if(!is.na(summ.rsk$current_status) & summ.rsk$current_status < 0){
				rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(summ.rsk$current_status)),
										xleft = s - 0.08 - xinch(fishwidth),
										ybottom = r - 0.5 - yinch(fishheight)/2,
										xright = s - 0.08,
										ytop = r - 0.5 + yinch(fishheight)/2)
			}
			
			if(!is.na(summ.rsk$current_status) & summ.rsk$current_status == -999999){
				text(s - 0.5, r - 0.5, "!", cex = 1.3)
			} else if (!is.na(summ.rsk$current_status) & summ.rsk$current_status < 0){
				text(s - 0.5, r - 0.5, paste0(ifelse(summ.rsk$current_status > 0, "+", ""), summ.rsk$current_status, "%"), cex = 1.3)
			} 
		} # end if species are present
		
	} # end s
} # end r

# abline(v = seq(0, 6, 1), lwd = 5, col = "white")
# abline(h = seq(0, 10, 1), lwd = 5, col = "white")



par(mar = rep(0,4), family = "Sofia Pro Semi Bold")
plot(1,1,"n", xlab = "", ylab = "", bty = "n", xaxt  = "n", yaxt = "n", xlim = c(0.02,5.98), ylim = c(0.02, 9.98), xaxs = "i", yaxs = "i")
for(r in 1:10){
	for(s in 1:6){
		region.r <- rev(regions)[r]
		species.s <- species[s]
		type.k <- c("Spawners", "Total return")[k]
		summ.rsk <- sps_summary %>% filter(region == region.r & species == species.s & type == type.k)
		
		
		if(dim(summ.rsk)[1] > 0){
			
			polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['dd'], border = NA)
			rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(NA)),
									xleft = s - 0.08 - xinch(fishwidth),
									ybottom = r - 0.5 - yinch(fishheight)/2,
									xright = s - 0.08,
									ytop = r - 0.5 + yinch(fishheight)/2)
			
			text(s - 0.5, r - 0.5, "?", cex = 1.3)
		}
	}}

# Spawners and total
k <- 1
plot(1,1,"n", xlab = "", ylab = "", bty = "n", xaxt  = "n", yaxt = "n", xlim = c(0.02,5.98), ylim = c(0.02, 9.98), xaxs = "i", yaxs = "i")
for(r in 1:10){
	for(s in 1:6){
		region.r <- rev(regions)[r]
		species.s <- species[s]
		type.k <- c("Spawners", "Total return")[k]
		summ.rsk <- sps_summary %>% filter(region == region.r & species == species.s & type == type.k)
		
		
		if(dim(summ.rsk)[1] > 0){
			if(is.na(summ.rsk$current_status)){
				polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['dd'], border = NA)
			} else if(summ.rsk$current_status == -999999){
				polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['crit'], border = NA)
			} else {
				polygon(x = c(s-0.98, s-0.98, s-0.02, s-0.02), y = c(r-0.97, r-0.03, r-0.03, r-0.97), col = fishy_bgcols['bg'], border = NA)
			}
			
			rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(summ.rsk$current_status)),
									xleft = s - 0.08 - xinch(fishwidth),
									ybottom = r - 0.5 - yinch(fishheight)/2,
									xright = s - 0.08,
									ytop = r - 0.5 + yinch(fishheight)/2)
			
		} # end if species are present
		
	} # end s
} # end r

k <- 2
for(r in 1:10){
	for(s in 1:6){
		region.r <- rev(regions)[r]
		species.s <- species[s]
		type.k <- c("Spawners", "Total return")[k]
		summ.rsk <- sps_summary %>% filter(region == region.r & species == species.s & type == type.k)
		
		
		if(dim(summ.rsk)[1] > 0){
			
			rasterImage(image_colorize(image = fish, opacity = 100, color = fishy_cols_func(summ.rsk$current_status)),
									xleft = s - 0.08 - xinch(fishwidth),
									ybottom = r - 0.7 - yinch(fishheight)/2,
									xright = s - 0.08,
									ytop = r - 0.7 + yinch(fishheight)/2)
			
			rasterImage(image_colorize(image = fish_outline, opacity = 100, color = fishy_bgcols['bg']),
									xleft = s - 0.08 - xinch(fishwidth), 
									ybottom = r - 0.7 - yinch(fishheight)/2, 
									xright = s - 0.08,
									ytop = r - 0.7 + yinch(fishheight)/2)
			
			if(is.na(summ.rsk$current_status)){
				text(s - 0.5, r - 0.7, "?", cex = 1.3)
			} else if(summ.rsk$current_status == -999999){
				text(s - 0.5, r - 0.7, "!", cex = 1.3)
			} else if (summ.rsk$current_status > 150 | summ.rsk$current_status < -80){
				text(s - 0.5, r - 0.7, paste0(ifelse(summ.rsk$current_status > 0, "+", ""), summ.rsk$current_status, "%"), cex = 1.3)
			} else {
				text(s - 0.5, r - 0.7, paste0(ifelse(summ.rsk$current_status > 0, "+", ""), summ.rsk$current_status, "%"), cex = 1.3)
			}
		} # end if species are present
		
	} # end s
} # end r

########################################################
# Methods figure
##################
#
# Simulate time series over 72 years (1950 - 2021)
S <- numeric(72)
set.seed(23435543)
S[1] <- 12
for(i in 2:72){
	S[i] <- rlnorm(n = 1, meanlog = log(S[i-1] * exp(1.3 - S[i-1]/seq(14, 10, length.out = 72)[i])), sd = 0.2)
}
S[72] <- 12

g <- 4 # generation length
SS <- numeric(72)
for(i in 1:72){
	SS[i] <- prod(S[max(1, (i - g + 1)):i])^(1/length(max(1, (i - g + 1)):i))
}

# Define axes ticks for 7th plot
percticks <- c(-0.20, 0, 0.20, 0.40)
perlab <- c("-20%", "0%", "20%", "40%")
yticks <- mean(SS)*(percticks + 1)


# 2025
quartz(width = 8, height = 4, pointsize = 12)

par(mfrow = c(1,1), mar = c(4,4,2,1), cex.lab = 1, family = "Sofia Pro Light")

plot(1950:2021, SS, "n", bty = "l", las = 1, ylab = "Spawner abundance (% anomaly)", xlab = "", ylim = c(10, 22), yaxt= "n")

axis(side = 2, at = yticks, labels = perlab, las = 1)

lines(1950:2021, SS, lwd = 1.5)

abline(h = mean(SS), lty = 2)#, col = SWP_cols['stone3'])

# Trends
lines(1950:2021, exp(predSS$fit), col = "#6F99AD", lwd = 2)
lines(c(1950:2021)[(72 - 3*g + 1):72], exp(predSS2$fit), col = "#486876", lwd = 4)


plot(1950:2021, SS, "n", bty = "n", las = 1, ylab = "", xlab = "", xaxt = "n", ylim = c(10, 22), yaxt= "n")
lines(1950:2021, S, col = grey(0.6), xpd = NA)


plot(1950:2021, SS, "n", bty = "l", las = 1, ylab = "Spawner abundance (thousands)", xlab = "", ylim = c(10, 22))
lines(1950:2021, S, col = grey(0.6), xpd = NA)

plot(1950:2021, SS, "n", bty = "n", las = 1, ylab = "", xlab = "", xaxt = "n", ylim = c(10, 22), yaxt= "n")
lines(1950:2021, SS, lwd = 1.5)

####################################
# Cowichan Chinook

# (1) CTC data
evimi_ctc <- readxl::read_xlsx("data/TCCHINOOK-25-02-Appendix-B-Escapement-Detailed.xlsx", sheet = "B4", range = "A5:G54", col_types = "numeric", col_names = c("Year", "Nanaimo_esc", "Nanaimo_trun", "Cowichan_esc", "Cowichan_trun", "Phillips_esc", "Phillips_trun")) # Southern BC

evimi_ctc_summed <- evimi_ctc %>%
	group_by(Year) %>%
	summarise(esc_sum = sum(Nanaimo_esc, Cowichan_esc, Phillips_esc),
						trun_sum = sum(Nanaimo_trun, Cowichan_trun, Phillips_trun))


quartz(width = 8, height = 4, pointsize = 12)

par(mfrow = c(1,1), mar = c(4,4,2,1), cex.lab = 1, family = "Sofia Pro Light")
plot(evimi_ctc_summed$Year, evimi_ctc_summed$trun_sum*10^-3, "n", bty = "l", las = 1, xlab = "", ylab = "Total abundance (thousands)", ylim = c(0, 40), yaxs = "i", xaxs = "i")

y_tot <- cbind(data.frame(evimi_ctc)[, "Nanaimo_trun"],
							 apply(data.frame(evimi_ctc)[, c("Nanaimo_trun", "Cowichan_trun")], 1, sum),
							 evimi_ctc_summed$trun_sum)

for(i in 3:1){
	
	polygon(x = c(evimi_ctc$Year, rev(evimi_ctc$Year)),
					y = c(y_tot[, i], rep(0, length(evimi_ctc$Year)))*10^-3,
					col = PSF_cols[c(1,3,5)[i]], border = NA)
}

lines(evimi_ctc_summed$Year, evimi_ctc_summed$trun_sum*10^-3,  col = PSF_cols['deepwater'], lwd = 2)
