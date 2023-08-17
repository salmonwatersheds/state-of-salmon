#______________________________________________________________________________
#' Reference decade
#' 
#' Finds the suitable reference decade for calculation of Expansion Factors
#' 1 (and 2), based on the number of observations of spawners in indicator
#' (and non-indicator) streams in each decade.
#'
#' @param sampledSpawners  matrix of spawner abundances with number of rows 
#' equal to the number of years and number of columns equal to the number of
#' indicator streams. Note: zeroes indicate streams not sampled.
#' @param years the years (e.g., 2001) corresponding to the rows in sampledSpawners.
#' Default values are 1960-2009 so that a 50-year simulation gives complete decades.
#' @param legacy logical indicating whether calculations should be computed under 
#' legacy mode. Note: Not functional as of Dec. 28, 2018.
#'
#' @return a numeric vector with the reference decade for each year in 
#' \code{years}
#'
#' @details 

refDecade <- function(sampledSpawners, years = 1960:2009, legacy = FALSE){
	# ----------------------------------------------------------------------------
	# Step 1: Determine number of obs per indicator stream per decade
	# ----------------------------------------------------------------------------
	
	# Decadal totals (number of years with data) for each indicator stream
	decades <- floor(years/10) * 10 # cbind(years, decades)
	nDecades <- length(unique(decades)) # number of decades
	decadesFactor <- as.numeric(as.factor(decades))
	
	decade.counts <- matrix(NA, nrow = nDecades, ncol = dim(sampledSpawners)[2])
	rownames(decade.counts) <- unique(decades)
	
	for(i in 1:dim(sampledSpawners)[2]) { #for each stream
		# decade.counts[ ,i] <- tapply(sampledSpawners[,i] != 0, decades, sum)
		decade.counts[, i] <- tapply(is.na(sampledSpawners[,i]) == FALSE, decades, sum)
	}
	
	# Which decades have at least one estimate for each indicator stream?
	insufficient.info <- apply(decade.counts == 0, 1, sum)
	
	# ----------------------------------------------------------------------------
	# Step 2: Determine suitable reference decade for each year
	# ----------------------------------------------------------------------------
	
	# Begin under the assumption that each decade has at least one year from each
	# indicator streams, and no "reference decades" need to be used
	ref.decade <- decadesFactor
	
	if(sum(insufficient.info) > 0) { #If all decades do not have sufficient information
		if(legacy) { 
			stop("Currently coded for Non-Legacy mode only.") 
		} else {
			
			m <- which(insufficient.info > 0) # decades Missing data from at least 1 indicator stream
			
			if(length(m) < nDecades){ # If there are some decades that have data
				
				# Potential "reference decades" with at least one year of data for
				# each indicator stream:
				r <- which(insufficient.info == 0) 
				
				for(i in 1:length(m)){
					# Take closest decade that has info. 
					# If two equally close, take historical (first; d[1]) rather than future (second; d[2]).
					ref.decade[which(decadesFactor == m[i])] <- r[which(abs(r-m[i]) == min(abs(r-m[i])))[1]]
				} # end i
				
			} else { 
				#If no decades have sufficient data, use 1980-1999 (English et al. 2016)
				ref.decade <- 8090
			}
		} # end Non-Legacy mode
	} # end insufficient info

	return(list(ref = ref.decade, decadesFactor = decadesFactor, decades = decades))
	
} # end refDecade function


#______________________________________________________________________________
#' Expansion of spawners to account for not-monitored indicator streams
#' via application of Expansion Factor 1
#'
#' Loosely off the \code{ExpFactor1} and the \code{IndicatorEscapement} 
#' function from the NCCDBV package.
#' 
#' @param sampledSpawners  matrix of spawner abundances with number of rows 
#' equal to the number of years and number of columns equal to the number of
#' indicator streams. Note: zeroes indicate streams not sampled.
#' @param years the years (e.g., 2001) corresponding to the rows in sampledSpawners.
#' Default values are 1960-2009 so that a 50-year simulation gives complete decades.
#' @param legacy logical indicating whether calculations should be computed under 
#' legacy mode. Note: Not functional as of Dec. 28, 2018.
#'
#' @return Returns a data frame containing indicator stream adjusted escapement
#' for each year.
#' 
#' @details
#' The following notation is simplified from the NCCDBV package, since we are 
#' dealing with a single species & CU.
#' 
#'  \bold{Step 1: Average Decadal escapement for indicator streams.}
#' The average decadal escapement (\eqn{\bar{E}_{id}}{Avg Decadal E}) for 
#' indicator stream (\eqn{i}) and decade \eqn{d} is based on the observed 
#' escapement for each year \eqn{y} within a decade divided 
#' by the number of years \eqn{{Y_{id}}} with escapement for stream \eqn{i} 
#' in that decade:
#' \deqn{
#'   \bar{E}_{id} = { 
#'     \sum_{y=1}^{Y_{srd}} E_{idy}
#'           \over
#'     Y_{id}
#'    } 
#' }{Avg Decadal E = sum(Decadal E) / Years}
#' where \eqn{E_{idy}}{E_siady} is the observed escapement for a 
#' species, indicator stream, statistical stratum (e.g., StatArea or CU) and 
#' year (\eqn{y}) within a decade of interest. The sum is divide by
#' \eqn{Y_{siad}}{Y_siad} the total number of years escapement years available 
#' within the decade of interest.
#'
#' 
#' \bold{Step 2: Indicator stream decadal contributions.}
#' The decadal contributions of each indicator stream (\eqn{P_{siad}}{P_siad}) 
#' represents the average escapement contribution of a particular indicator 
#' stream (\eqn{i}) relative to other indicator streams for a given species 
#' (\eqn{s}), statistical stratum (\eqn{a}) and decade (\eqn{d}). The 
#' contributions each indicator stream is determined as:
#' \deqn{
#'   P_{siad} = { 
#'     \bar{E}_{siad}
#'       \over
#'      \sum_{i=1}^{I} \bar{E}_{siad}
#'   } 
#' }{ P = Avg Decadal E / Sum(All Avg Decadal E)}
#' where \eqn{I} is the total number of indicator streams for a species,
#' statistical stratum and decade.
#' 
#' \bold{Step 3: Expansion Factor 1.} The average decadal escapement and the
#' overall escapement contributions for available stream is used to determine 
#' Expansion Factor 1 (\eqn{F^\prime_{sady}}{F'_sady}), which is applied to the total
#' observed indicator escapement to infill contributions of indicator stream 
#' not surveyed on given year. This expansion factor is computed for each year
#'  (\eqn{y}), species (\eqn{s}), and statistical stratum (\eqn{a}) as:
#' \deqn{
#' F^\prime_{sady} = { 
#'     1
#'    \over
#'    \sum_{i=1}^{I} \left( P_{siad} \cdot w_{siady}  \right)
#'    }  
#' }{F'_sady = 1/sum(P_siad * w_siady)}
#' where \eqn{w_{siady}} is an indicator taking the value '1' if an observed
#' escapement is available for a given stream in a year and '0' if an observed 
#' escapement value is not available.  
#'  
#' \bold{Step 4: Compute the adjusted total.} The final step generates the 
#' adjusted total escapement (\eqn{E^\prime_{sady}}) for all indicator streams.
#' This total which accounts for any indicator streams not surveyed on a given 
#' year by multiplying the observed yearly indicator escapement by the 
#' corresponding Expansion Factor 1 value:
#' \deqn{
#' E^\prime_{sady} = F^\prime_{sady} \sum_{i=1}^{I} E_{siady} 
#' }{E'_sady = F'_sady * sum(E_siady)}
#' where \eqn{\sum_{i=1}^{I} E_{siady}}{sum(E_siady)} is the observed indicator escapement and
#' \eqn{F^\prime_{sady}}{F'_sady} is the corresponding expansion factor (i.e., Expansion Factor 1).  
#' 
#' 
#' @note When calculating Expansion Factor 1, there must be data for all indicator 
#' streams within a statistical stratum (i.e., statistical area or conservation 
#' unit) for each decade.  If there is insufficient data indicator stream data
#' for a given decade, a reference period is used in place. Reference periods
#' are determined by the \code{\link{ExpFactor1RefDecade}} routine.
#' 
#' Breifly, a search is conducted to find the closest decade with complete
#' coverage.  If a suitable decade is found, the values for this decade
#' are used as a subsitute.  If no suitable decade is available, then the expanded
#' reference periods are considered (i.e., 80/90s, and then 80/90/00s). If these
#' expanded periods do not have complete coverage for all indicator streams then
#' all analysis years are used as the reference period. 
#' @details 
#' Stratum decades where each indicator stream has at least one obervation are
#' used as is, for decades where this is not the case a suitable reference
#' period (decade or otherise) is determined. 
#' 
#' Reference period selection is handled in one of two different way depending
#' if legacy support is enabled.  
#'
#' \bold{Legacy Mode:}
#' \enumerate{
#'   \item  Choose closest historical decade with sufficient information
#'   \item  Failing (1), choose the nearest future decade
#'   \item  Failing (2), select 80/90s reference
#' }
#'  
#' \bold{Non-Legacy Mode:}
#' \enumerate{
#'   \item Choose the closest decade (historical or future) with sufficient information.
#'   \item Failing (1), select 80/90s reference period
#'   \item Failing (2), select 80/90/00s reference period
#'   \item Failing (3), select all analysis years as the reference period
#' }
#'
#'
#' @return Returns a list containing (1) the value of Expansion Factor 1 for each year,
#' (2) the reference decade used each year, and (3) the matrix of P (proportion
#' of spawners to each indicator stream) for each reference decade (rows) and 
#' indicator stream (columns).
#' 
#' @examples
#'
#' \dontrun{
#' # Get a list of stratums that require reference decades along with the 
#' # reference decade to use.
#'  exp1 <- ExpFactor1RefDecade( 
#'    escapement = subset(escapement, Year >= 1954 & Year <= 2017), 
#'    stratum.type = "StatArea",
#'    legacy = FALSE
#'  )
#' }
#' 
#' @export
ExpFactor1 <- function(sampledSpawners, years = 1960:2009, legacy = FALSE) {
	# In order to determine whether or not a stratum requires a reference
	# decade we need to determine 1) the number of indicator streams
	# in the stratum and 2) whether or not there was at least one observation
	# for each indicator streams.  Stratums that match this criteria can
	# be used as is when computing ExpFactor1, while stratums that do not 
	# meet this criteria will need a reference decade.  If no reference
	# decade is available we fall back to the 8090s as the reference decade.
	#
	# Reference decades selection is handled in  two different way depending
	# if legacy support is enabled.  
	#
	# Legacy Mode:
	# 1. Choose closest historical decade with sufficient information
	# 2. Failing (1), choose the nearest future decade
	# 3. Failing (2), select 8090s contribution
	# 4.
	#
	# Non-Legacy Mode
	# 1. Choose the closest decade (historical or future) ith sufficient information.
	# 3. Failing (1), select 8090s contribution
	
	# browser()
	
	# Checks ------------------------------------------------------------------
	# if (any(is.na(sampledSpawners))) stop("Expecting only non-zero escapement without missing values.") #Changed not sampled to NAs instead of zeroes
	if(dim(sampledSpawners)[1] != length(years)) stop("Number of years not equal to dim sampledSpawners")
	
	# ----------------------------------------------------------------------------
	# Step 1: Determine suitable reference decade for each year
	# ----------------------------------------------------------------------------
	
	decadeDummy <- refDecade(sampledSpawners = sampledSpawners, years = years, legacy = legacy)
	ref.decade <- decadeDummy$ref
	decadesFactor <- decadeDummy$decadesFactor

	# ----------------------------------------------------------------------------
	# Step 2: Calculate proportional contribution (P) of each indicator stream
	# 				per reference decade
	# ----------------------------------------------------------------------------
	
	if(ref.decade[1] == 8090){
		
		years.to.use <- which(years == 1980): which(years == 1999)
		avgSpawners <- apply(sampledSpawners[years.to.use, ], 2, sum, na.rm = TRUE) / apply(is.na(sampledSpawners[years.to.use, ]) == FALSE, 2, sum)
		
		P <- matrix(avgSpawners / sum(avgSpawners, na.rm=TRUE), nrow=1)
		rm(years.to.use, avgSpawners)
		
	} else {
		
		P <- matrix(NA, nrow=length(unique(ref.decade)), ncol = ncol(sampledSpawners))
		for(d in 1:length(unique(ref.decade))){ # For each reference decade
			
			years.to.use <- which(decadesFactor == unique(ref.decade)[d])
			avgSpawners <- apply(sampledSpawners[years.to.use, ], 2, sum, na.rm = TRUE) / apply(is.na(sampledSpawners[years.to.use, ]) == FALSE, 2, sum)
			
			P[d, ] <- avgSpawners / sum(avgSpawners)
			rm(years.to.use, avgSpawners)
		} # end d
	
	}
	
	# ----------------------------------------------------------------------------
	# Step 3: Calculate Expansion Factor 1 for each year
	# ----------------------------------------------------------------------------
	
	w <- is.na(sampledSpawners) == FALSE
	
	if(length(ref.decade) == 1 & ref.decade[1] == 8090){
		ExpFac1 <- apply(matrix(rep(P, dim(w)[1]), nrow = dim(w)[1], ncol=dim(w)[2], byrow = TRUE) * w, 1, sum, na.rm=TRUE) ^ (-1)
	} else {
		ExpFac1 <- apply(P[as.numeric(as.factor(ref.decade)), ] * w, 1, sum, na.rm=TRUE) ^ (-1)
	}
	
	return(list(ExpFac1, ref.decade, P))

} # end ExpFactor1 function


#______________________________________________________________________________
#' Expansion Factor 2
#' 
#' Computes Expansion Factor 2 to expand indicator escapement to account
#' for non-indicator streams.  
#'
#' @param spawnersInd  matrix of spawner abundances with number of rows 
#' equal to the number of years and number of columns equal to the number of
#' indicator streams. Note: zeroes indicate streams not sampled.
#' @param spawnersNonInd matrix of spawner abundances in non-indicator streams, 
#' formatted as for \code{spawnersInd}.
#' @param years the years (e.g., 2001) corresponding to the rows in sampledSpawners.
#' Default values are 1960-2009 so that a 50-year simulation gives complete decades.
#'
#' @return a data frame containing the values for ExpFac2 by year, the code for the reference deacde
#' applied in each year, the unique values for ExpFac2, and a code indicating the 
#' method used to obtain the reference decade (see  Details).
#'
#' @details 
#' The overall observed escapement to all streams in an area is obtained by 
#' accounting for the contribution of non-indicator streams to the total average 
#' escapement for all streams in that statistical area or conservation unit for 
#' the user defined decade or period with the best survey coverage for that 
#' statistical area or conservation unit (Appendix Table A1 and A2 respectively).
#' 
#' ExpFac2 is calculated from the decades with the 'best escapement coverage'.
#' See function for details on how we quantified this criteria based on the 
#' number of indicator and non-indicator streams with at least one year of data
#' in each decade.
#' 
#' \bold{Computing Expansion Factor 2}
#' 
#' The observed escapement of a species to an indicator stream, average over 
#' years with survey data in a decade and stratum is calculated as:
#' 
#' 
#' \deqn{
#' F^{\prime\prime}_{sady} = { 
#'     \sum_{i=1}^{I} \bar{E}_{siady} + \sum_{j=1}^{J} \bar{E}_{sjady}
#'    \over
#'    \sum_{i=1}^{I} \bar{E}_{siady}
#'    }  
#' }{F''_sady = (sum(Ebar_siady) + sum(Ebar_sjady) )/sum(Ebar_siady)}
#' 
#' where \eqn{\bar{E}_{siady}}{sum(Ebar_siady)} and \eqn{\bar{E}_{sjady}}{sum(Ebar_sjady)} are the decadal average escapement 
#' for indicator and non-indicator escapement respectively. The decadal average for 
#' as specific species (\eqn{s}), stream (\eqn{i}), stratum (\eqn{a}) and decade (\eqn{d}) can be 
#' computed as:
#' \deqn{
#' \bar{E}_{siad} =  {\sum_{y=1}^Y E_{siady} \over Y }
#' }{Ebar_siad = sum(E_siady) / Y}
#' 
#' where \eqn{Y} is the number of years available for a given stream for a particular 
#' species, stream, stratum, and decade.  Note that stratum may be either StatArea 
#' or CU.
#' 
#' 
#' 
#' @note 
#' 
#' \bold{Computing the adjusted total escapment}
#' 
#' Expansion Factor 2 (\eqn{F^{\prime\prime}_{sady}}{F''_sady}) can be combined
#' with indicator escapement estimates \eqn{E^\prime_{sady}}{E'_sady} to 
#' compute the adjusted sum of observed indicator escapement for a 
#' given species, stratum and year as:
#' \deqn{
#' E_{sady} = E^\prime_{sady} \cdot F^{\prime\prime}_{sady} 
#' }{E_sady = E'_sady * F''_sady}.
#' 
#' @examples
#' 
#' 
ExpFactor2 <- function(spawnersInd, spawnersNonInd, years = 1960:2009, legacy = FALSE) {
	
	# Checks ------------------------------------------------------------------
	if(dim(spawnersInd)[1] != length(years)) stop("Number of years not equal to dim spawnersInd")
	if(dim(spawnersNonInd)[1] != length(years)) stop("Number of years not equal to dim spawnersNonInd")
	
	# ----------------------------------------------------------------------------
	# Step 1: Determine suitable reference decade for each year
	# ----------------------------------------------------------------------------
	
	decadeDummyInd <- refDecade(sampledSpawners = spawnersInd, years = years, legacy = legacy)
	
	# What are the numbers of non-indicator streams with at least one count per decade?
	decade.counts <- matrix(NA, nrow = length(unique(decadeDummyInd$decades)), ncol = dim(spawnersNonInd)[2])
	rownames(decade.counts) <- unique(decadeDummyInd$decades)
	for(i in 1:dim(spawnersNonInd)[2]) { #for each stream
		decade.counts[ ,i] <- tapply(is.na(spawnersNonInd[,i]) == FALSE, decadeDummyInd$decades, sum)
	}
	
	atLeastOne <- apply(decade.counts > 0, 1, sum)
	
	# --------------
	# 1. At least every indicator stream monitored at least once in each decade
	if(sum(decadeDummyInd$ref-decadeDummyInd$decadesFactor) == 0){
		
		# A. and number of non-indicator streams monitored is similar across decades
		if(min(atLeastOne/max(atLeastOne)) >= 0.9){
			# then use the current decade to calculate ExpFac2
			code <- 0
			ref.decade <- decadeDummyInd$ref 
		
		# B. number of non-indicator streams is significantly different
		} else {
			
			# i. If one decade has significantly higher coverage, then use that decade:
			if(length(which(atLeastOne == max(atLeastOne))) == 1){
				code <- which(atLeastOne == max(atLeastOne))
				ref.decade <- rep(code, length(years))
			
			# If multiple decades are tied for max coverage, use the appropriate decade and infill
			# bad decades with the nearest one
			} else {
			
				code <- 6
				ref.decade <- decadeDummyInd$ref
				decadesMissingData <- which(atLeastOne/max(atLeastOne) < 0.9)
				decadesWithData <- which(atLeastOne/max(atLeastOne) >= 0.9)
				for(i in 1:length(decadesMissingData)){
					# Replace decadesMissingData in ref.decade with the closest historical or future decade with data
					ref.decade[ref.decade == decadesMissingData[i]] <- which(abs(decadesWithData - decadesMissingData[i]) == min(abs(decadesWithData - decadesMissingData[i])))[1]
				} # end decadesMissingData
			
			}
		}
	} # end every indicator stream monitored at least once per decade
	
	# --------------
	# 2. Not every indicator stream monitored at least once in each decade
	if((sum(decadeDummyInd$ref - decadeDummyInd$decadesFactor) != 0)){
		
		# A. and number of non-indicator streams monitored is similar across decades
		if(min(atLeastOne/max(atLeastOne)) >= 0.9){
			# Use ref.decade from indicator streams for ExpFac2
			ref.decade <- decadeDummyInd$ref 
			code <- 7
		
		# B. number of non-indicator streams is significantly different
		} else {
			decadeDummynonInd <- refDecade(sampledSpawners = spawnersNonInd, years = years, legacy = legacy)
			# If the suggested reference decades match for the two, then use that
			if (sum(decadeDummynonInd$ref - decadeDummyInd$ref) == 0){
				ref.decade <- decadeDummyInd$ref 
				code <- 7
			# If the suggested reference decades don't match (i.e., diverging trends in coverage of
			# indicator and non-indicator) then use 8090s
			} else {
				ref.decade <- 8090
				code <- 8
			}
		}
	} # end if not every indicator stream is monitored at least once per decade
	
	# ----------------------------------------------------------------------------
	# Step 2: Calculate average escapement to streams per decade and Expansion 
	#					Factor
	# ----------------------------------------------------------------------------
	
	# If there is not sufficient data in any decade, use 1980-1999 reference period
	if(ref.decade[1] == 8090){ 
		
		# If the number of non-indicator streams is significantly different for these two decades
		# then use the entire time series to calculate
		if(abs(atLeastOne['1980']/max(atLeastOne) - atLeastOne['1990']/max(atLeastOne)) > 0.1){
			years.to.use <- c(1:length(years))
		} else {
			years.to.use <- which(years == 1980): which(years == 1999)
		}
		
		# Average spawners in each indicator stream for the years.to.use
		avgInd <- apply(spawnersInd[years.to.use, ], 2, sum, na.rm = TRUE) / apply(is.na(spawnersInd[years.to.use, ]) == FALSE, 2, sum)
		
		# Number of years in the years.to.use that each non-indicator stream is monitored
		Yj <- apply(as.matrix(is.na(spawnersNonInd[years.to.use, ]) == FALSE), 2, sum)
	
		# Exclude non-indicator streams that weren't monitored from calculation
		avgNonInd <- apply(as.matrix(spawnersNonInd[years.to.use, which(Yj > 0)]), 2, sum, na.rm = TRUE) / Yj[which(Yj > 0)]
		
		ExpFac2 <- (sum(avgInd, na.rm=TRUE) + sum(avgNonInd, na.rm=TRUE)) / sum(avgInd, na.rm=TRUE)
	
	} else {
	
		ExpFac2 <- numeric(length(unique(ref.decade)))
		for(d in 1:length(unique(ref.decade))){ # For each reference decade
			
			years.to.use <- which(decadeDummyInd$decadesFactor == unique(ref.decade)[d])
			
			# Average spawners in each indicator stream for the years.to.use
			avgInd <- apply(spawnersInd[years.to.use, ], 2, sum, na.rm = TRUE) / apply(is.na(spawnersInd[years.to.use, ]) == FALSE, 2, sum)
			# Number of years in the years.to.use that each non-indicator stream is monitored
			Yj <- apply(is.na(spawnersNonInd[years.to.use, ]) == FALSE, 2, sum)
			# Exclude non-indicator streams that weren't monitored from calculation
			avgNonInd <- apply(spawnersNonInd[years.to.use, which(Yj > 0)], 2, sum, na.rm = TRUE) / Yj[which(Yj > 0)]
			
			ExpFac2[d] <- (sum(avgInd) + sum(avgNonInd)) / sum(avgInd)
		
			rm(years.to.use, avgInd, avgNonInd)
	
		} # end d
		
	}	
	
	return(list(ExpFac2 = ExpFac2[as.numeric(as.factor(ref.decade))], ref.decade, ExpFac2, code = code))
	
		
} # end	 ExpFactor2 function
 