library(readxl)
library(dplyr)

inseas <- read_xlsx("data/SockPink_Passage_Summary_2025-08-29.xlsx", range = "A15:F134")
head(inseas)

# pre-season run size forecast
forecast_pre <- 2941000

inseas <- inseas %>%
	filter(!is.na(`Cummulative Estimated Passage`)) %>% 
	select(`Mission Date`, `Cummulative Estimated Passage`)

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
