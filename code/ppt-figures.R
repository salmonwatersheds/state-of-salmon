library(readxl)
library(dplyr)

inseas <- read_xlsx("data/SockPink_Passage_Summary_2025-08-29.xlsx", range = "A15:F134")
head(inseas)

# pre-season run size forecast
forecast_pre <- 2941000

quartz(width = 9, height = 4)
par(family = "Sofia Pro Light", mar = c(3,5,2,1))


plot(inseas$`Mission Date`, inseas$`Cummulative Estimated Passage`*10^-6, "n", xlab = "", ylab = "Fraser sockeye\ncumulative count (millions)", bty = "l", las = 1)
abline(h = forecast_pre*10^-6, col = "#74BDB8")

plot(inseas$`Mission Date`, inseas$`Cummulative Estimated Passage`*10^-6, "l", col = "#74BDB8", lwd = 3, xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")


plot(inseas$`Mission Date`, inseas$`Cummulative Estimated Passage`*10^-6, "n", xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")
abline(h = forecast_pre*10^-6, col = "#D32314", lwd = 2)


frse <- read.csv("data/Fraser Sockeye Run Size_2025-04-24.csv") %>%
	filter(Management.Group == "Total Fraser")

plot(frse$Year, frse$Run.Size*10^-6, "l", xlab = "", ylab = "Fraser sockeye\ncumulative count (millions)", bty = "l", las = 1, col = "#74BDB8", lwd = 3)
abline(h = forecast_pre*10^-6, col = "#D32314", lwd = 2)
abline(h = 10, lty = 2, col = "#74BDB8")


inseas <- inseas %>%
	filter(!is.na(`Cummulative Estimated Passage`)) %>% 
	select(`Mission Date`, `Cummulative Estimated Passage`)
"#74BDB8"    "#0D1E33"    "#FED925"    "#F8F2E6"    "#DCF1F3" 
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

# Expanded spawner estimate
ccck_ex <- readRDS("output/expanded-spawners/Central Coast-spawners.rds")[2, "Chinook", ]
yrs <- as.numeric(names(ccck_ex))

e <- readRDS("output/expanded-spawners/Central Coast-expansion-factors.rds")[[1]]

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