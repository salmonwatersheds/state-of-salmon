##############################################################################
# This code pulls the spawner data from the SWP Database and writes it to a 
# .csv that is called by the abundance-metrics.R code
# 
# Author: Steph Peacock
# Date: July 6, 2023
###############################################################################

library(dplyr)
library(RPostgreSQL)

###############################################################################
# Create connection to postreSQL salmonwatersheds database
###############################################################################

dsn_database <- "salmondb_prod"   # Specify the name of your Database
dsn_hostname <- "data.salmonwatersheds.ca"  # Specify host name e.g.:"aws-us-east-1-portal.4.dblayer.com"
dsn_port <- "5432"                # Specify your port number. e.g. 98939
dsn_uid <- "salmonwatersheds"         # Specify your username. e.g. "admin"
dsn_pwd <- readline(prompt="Enter database password: " )    # Specify your password. e.g. "xxx"

tryCatch({
	drv <- dbDriver("PostgreSQL")
	print("Connecting to Database…")
	connec <- dbConnect(drv, 
											dbname = dsn_database,
											host = dsn_hostname, 
											port = dsn_port,
											user = dsn_uid, 
											password = dsn_pwd)
	print("Database Connected!")
},
error=function(cond) {
	print("Unable to connect to Database.")
})

###############################################################################
# Import spawner survey view from appdata
###############################################################################

# Import 
spawner_surveys <- dbGetQuery(
	conn = connec,
	statement = "SELECT * FROM appdata.vwdl_streamspawnersurveys_output"
)

write.csv(spawner_surveys, file = "data/spawner_surveys.csv", row.names = FALSE)

###############################################################################
# Import CU-level spawner abundance (used for Fraser pink)
###############################################################################

spawner_abundance <- dbGetQuery(
	conn = connec,
	statement = "SELECT * FROM appdata.vwdl_dataset1cu_output"
)

write.csv(spawner_abundance, file = "data/spawner_abundance.csv", row.names = FALSE)

###############################################################################
# Import trends in spawner abundance (dataset391)
###############################################################################

trends <- dbGetQuery(
	conn = connec,
	statement = "SELECT * FROM appdata.vwdl_dataset391_output"
)

write.csv(trends, file = "data/dataset391.csv", row.names = FALSE)

###############################################################################
# Import appendix4 view from appdata
###############################################################################

# Get app4 view
app4 <- dbGetQuery(
	conn = connec, 
	statement = "SELECT * FROM appdata.vwdl_setr_appendix4"
)

app4$species_pooled <- app4$species_name
app4$species_pooled[app4$species_pooled %in% c("Pink (even)", "Pink (odd)")] <- "Pink"
app4$species_pooled[app4$species_pooled %in% c("Lake sockeye", "River sockeye")] <- "Sockeye"


write.csv(app4[, c("region", "species_name", "species_pooled", "cuid", "cu_name_pse", "psf_status")], file = "data/status.csv", row.names = FALSE)
