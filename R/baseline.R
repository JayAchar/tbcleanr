# Steps
# 1. Join ID and treatment start date with lab data
# 2. Filter by presence of start date
# 3. Check only 2 factor levels of lab result
# 4. Mutate - absolute number of days from treatment start
# 5. Sort by absolute number of days from treatment start
# 6. Keep result with smallest number of absolute days from treatment start
# 7. Filter by defined days culture (e.g. 30 days, 90 days)
# 8. Return ID number, start date and result

baseline <- function() {
	# require
		pkgs <- c("dplyr", "purrr")						# define required packages

			for (i in seq(along = pkgs)) {
				library(pkgs[i], character.only = T)		# load packages
			}


}


# =================================================================
# Prepare test data
library(readr)
require(data.table)
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

save(a, l, file = "baseline_test.rda")

# =================================================================
load("baseline_test.rda")

d <- inner_join(a, l, by = "APID")
