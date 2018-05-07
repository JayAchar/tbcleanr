




library(JA.funs)
library(readxl)
set.wd.data("")

xpert <- read_xlsx("Bentiu GeneXpert data from July 2017 to 13th March 2018.xlsx",
						sheet = "Raw data")






bentiu_vl <- function(x) {
		require(stringr)
		require(lubridate)
# variables to keep
		cols <- c("patient name", "Patient ID", "MTB; RIF", "Assay", "date")

	if (all(cols %in% names(x))) {
		x <- subset(x, select = cols)		# remove first columns - not required
	} else {
		stop("Column names are incorrect - please check")
	}


return(x)

# select only VL results
	x <- x[x$Assay == "Xpert_HIV-1 Viral Load", ]

}