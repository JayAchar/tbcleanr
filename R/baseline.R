# Steps
# 1. DONE = Join ID and treatment start date with lab data 
# 2. DONE = Filter by presence of start date
# 3. DONE = Check only 2 factor levels of lab result
# 4. DONE = Mutate - absolute number of days from treatment start
# 5. DONE = Sort by absolute number of days from treatment start
# 6. DONE = Keep result with smallest number of absolute days from treatment start
# 7. DONE = Filter by defined days culture (e.g. 30 days, 90 days)
# 8. DONE = Return ID number, start date and result

baseline <- function() {
	# require
		pkgs <- c("dplyr", "purrr")						# define required packages

			for (i in seq(along = pkgs)) {
				library(pkgs[i], character.only = T)		# load packages
			}


}


# =================================================================
# temporary function to review outputs
check_apid <- function() {
	r <- sample(d[["APID"]], size = 3)
	z <- d[APID %in% r, ]
	return(z)
}


# Prepare test data
library(readr)
require(data.table)
require(lubridate)
set.wd("functions/TB.funs/tests")

a <- read_csv("admission.csv")					# import admission data
		colnames(a) <- a[2,]
		a <- a[-c(1:2),]										# clean admission data

l <- read_csv("laboratory.csv")					# import lab data
		colnames(l) <- l[2, ]
		l <- l[-c(1:2), ]										# clean lab data

# cnvert to data.table
	a <- as.data.table(a)
	l <- as.data.table(l)

# select required variables only
	a <- a[, .(APID, STARTTRE),]
	l <- l[, .(APID, FIRST, RESULT), ]

# filter contaminated and NA culture results
	l <- l[!is.na(RESULT), ]
	l <- l[RESULT != "3", ]

save(a, l, file = "baseline_test.rda")

# =================================================================
load("baseline_test.rda")

d <- as.data.table(inner_join(a, l, by = "APID"))

# check if result variable is binary
	if (summary(levels(factor(d$RESULT)))[1] != 2) {
		stop("Result variable must have 2 levels")
	}

	
	d$FIRST <- dmy(d$FIRST)
	d$STARTTRE <- dmy(d$STARTTRE)					# reclassify date variables

	setkey(d, APID, STARTTRE, FIRST)			# set key to sort

# filter out results with no date
	d <- d[!is.na(FIRST),]

# remove ineligible records due to date
	d <- d[STARTTRE - FIRST <= 90, ]
	d <- d[STARTTRE - FIRST >= -7, ]

# absolute days from treatment start to specimen collection
	d <- d[, abs_days := abs(FIRST - STARTTRE), ]

# remove duplicate rows - id, time from treatment start, result
	subvar <- c("APID", "abs_days", "RESULT")
	setkeyv(d, subvar)
	d <- unique(d, by = subvar)

# identify id & time from treatment start duplicates
	d <- d[, dupvar := 1L * (.N > 1L), by = c("APID", "abs_days")] 

# remove negative culture if duplicated with positive on same day
	d <- d[!(dupvar == 1L & RESULT == "1"), ]

# add row id after sorting
	d <- d[, row_id := 1:.N, by = c("APID", "STARTTRE")]

# keep result closest to treatment start
	d <- d[row_id == 1,]

# rename result variable 
	selvar <- c("APID", "STARTTRE", "FIRST", "RESULT")
	d <- d[ , selvar, with = F]
	d <- setnames(d, old = c("RESULT", "FIRST"), new = c("baseline", "date"))

	return(d)