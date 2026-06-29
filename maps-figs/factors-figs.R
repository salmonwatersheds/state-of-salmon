# Generate catch weight figure from NPAFC Catch data

library(dplyr)
library(ggplot2)
library(grid)

# Read data
npafc_raw <- readxl::read_excel("C:/Users/hhunter/Downloads/NPAFC_Catch_Stat-1925-2024.xlsx", 
																"Catch", skip=1)

catchwt <- npafc_raw %>% filter( `Whole Country/Province/State` == "Whole country",
																 `Reporting Area` == "Whole country",
																 `Species` == "Total",
																 `Data Type` == "Round wt (MT)") %>% 
							mutate_at(c(7:ncol(npafc_raw)), as.numeric) %>%
							tidyr::pivot_longer(cols=c(7:ncol(npafc_raw)), names_to="Year", values_to="Weight (MT)") %>%
							mutate(Year = as.numeric(Year)) %>%
							filter(Country == "Canada")

# Calculate % decline since peak
(catchwt$`Weight (MT)`[catchwt$Year == 1985] - catchwt$`Weight (MT)`[catchwt$Year == 2024]) / catchwt$`Weight (MT)`[catchwt$Year == 1985]

# Create a text grob
grob1 <- grid::grobTree(textGrob("95%", x = 0.8, y = 0.9, hjust = 0, 
																 gp = gpar(col = "#095f70", fontsize = 20, fontface = "bold")))
grob2 <- grid::grobTree(textGrob("Decline \nsince 1985", x = 0.8, y = 0.75, hjust = 0, 
																 gp = gpar(col = "#095f70", fontsize = 15, fontface = "bold", lineheight = 0.7)))


# Line chart
catchwt %>%
	ggplot() + geom_path(aes(x=Year, y=`Weight (MT)`/1000), linewidth = 1, col="#1a228f") +
	#geom_segment(aes(x=1985, xend=2024, y=122, yend=35), arrow = arrow(), linewidth = 1, col="#700909") +
	#annotation_custom(grob1) +
	#annotation_custom(grob2) +
	scale_x_continuous(breaks=c(seq(1925, 2025, 20))) +
	labs(y="Canadian Commercial Salmon Catch \n(thousands of metric tonnes)", x="Source data: NPAFC (2025)") +
	theme_minimal() +
	theme(panel.grid = element_line(colour="grey95"),
				axis.text = element_text(size=13),
				axis.title.y = element_text(size=13.5, lineheight = 1.3, margin = margin(t=7, r=8)),
				plot.margin = margin(10,10,10,10),
				axis.title.x = element_text(size=9, hjust=1, margin = margin(t=12))
				)

ggsave("C:/Users/hhunter/Documents/Figures/catch-wt-notext.svg")
ggsave("C:/Users/hhunter/Documents/Figures/catch-wt-notext.pdf")

# Bar chart
catchwt %>% filter(Country == "Canada") %>% 
	ggplot() + geom_col(aes(x=Year, y=`Weight (MT)`/1000), fill="navy") +
	theme_minimal() +
	scale_x_continuous(breaks=c(seq(1925, 2025, 20))) +
	labs(y="Canadian Salmon Catch \n(thousands of metric tonnes)") 
