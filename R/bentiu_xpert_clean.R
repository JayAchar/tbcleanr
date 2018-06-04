#' Bentiu Xpert laboratory data cleaning
#'
#' This function takes the Bentiu PoC lab dataset as a data.frame and
#' removes unnecessary variables, cleans results and generates new 
#' result variables.
#' @param x data frome including Bentiu PoC lab Xpert data
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @export
#' @seealso \code{\link{tbcleanr}}
#' @examples
#' \dontrun{
#' bentiu_xpert_clean(p)
#' }


bentiu_xpert_clean <- function(x) {

# check x is data.frame
	if (!is.data.frame(x)) {
		stop("Confirm x is data.frame")
	}

# remove variables not required
	rem <- c("X__1", "module", "staff", "done", "ok")
	# check variables are included
	if (!all((rem %in% names(x)))) {
		stop("Variable names not all recognised")
	}
	# remove varaibles
	x <- x[, -which(names(x) %in% rem)]

# rename variables
	names(x)[names(x) == "MTB; RIF"] 	<- "result"
	names(x)[names(x) == "Assay"] 		<- "test"
	names(x)[names(x) == "Patient ID"] 	<- "lab_id"

# categorise test variable
	if (length(table(x$test)) == 2) {
		x$test[x$test == "Xpert MTB-RIF Assay G4"] <- "tb"
		x$test[x$test == "Xpert_HIV-1 Viral Load"] <- "vl"	
	} else {
		warning("More than 2 test types recognised - test variable not formatted")
	}

# categorise result
	if (sum(is.na(x$result)) > 0) {
		warning("Missing results found in data")
	}

# convert invalid results to NA
	invalid <- c("ERROR", "INVALID", "NO RESULT")
	x$result[which(x$result %in% invalid)] <- NA

# MTB
	# extract rif resistance results
	r <- which(str_detect(x$result, "MTB DETECTED"))
	x$rif <- NA  
	x$rif[r] <- x$result[r]

	# MTB detected
	x$result[r] <- 1
	# MTB not detected
	x$result[str_detect(x$result, "MTB NOT DETECTED")] <- 0

# MTB - rif result
	x$rif[str_detect(x$rif, "Resistance NOT DETECTED")] <- 0
	x$rif[str_detect(x$rif, "Resistance DETECTED")] <- 1
	x$rif[str_detect(x$rif, "Resistance INDETERMINATE")] <- NA

# HIV
	# extract HIV VL numerical result
	v <- which(str_detect(x$result, "HIV-1 DETECTED"))
	x$vl <- NA
	x$vl[v] <- x$result[v]
	# HIV not detected
	x$result[str_detect(x$result, "HIV-1 NOT DETECTED")] <- 0
	# HIV detected
	x$result[str_detect(x$result, "HIV-1 DETECTED")] <- 1

# HIV VL numerical results
	# extract VL figures
	x$vl <- str_match(x$vl, "DETECTED (.*?) copies/mL")[ ,2]
	# VL <40 re-entered as 39 copies/ml
	x$vl[str_detect(x$vl, "< 40")] <- 39
	# correct log10 figures
	x$vl1 <- NA
	x$vl2 <- NA
	x$vl3 <- NA
	vl <- which(str_detect(x$vl, "\\d{1}.\\d{2}E\\d{2}$"))
	x$vl1 <- str_match(x$vl, "\\d{1}.\\d{2}E\\d{2}$")
	x$vl2 <- as.numeric(str_extract(x$vl1, "\\d{2}$"))
	x$vl1 <- as.numeric(str_extract(x$vl1, "^\\d{1}.\\d{2}"))
	x$vl3 <- x$vl1 * (10 ^ x$vl2)

	# transfer log transformed viral loads
	x$vl[vl] <- x$vl3[vl]

	# remove unnecessary variables
		x$vl1 <- NULL
		x$vl2 <- NULL
		x$vl3 <- NULL

	# check if any non-numeric results in VL variable
	if (any(is.na(as.numeric(x$vl[v])))) {
		stop("VL figures lost during coercion")
	} else {
		x$vl <- as.numeric(x$vl)
	}

	# reclassify rif variable
	if (length(table(x$rif)) == 2) {
		x$rif <- as.numeric(x$rif)
	} else {
		warning("Rifampicin resistance variable has > 2 levels")
	}

	# reclassify result variable
	if (length(table(x$result)) == 2) {
		x$result <- as.numeric(x$result)
	} else {
		warning("Result variable has > 2 levels")
	}

	# reclassify test variable
	if (length(table(x$test)) == 2) {
		x$test[x$test == "tb"] <- 0
		x$test[x$test == "vl"] <- 1
		x$test <- factor(x$test, 
											levels = c(0,1), 
											labels = c("MTB/RIF", "HIV VL"))
			if (any(is.na(x$test))) {
				stop("Result variable factorised incorrectly")
			}
	} else {
		warning("Result variable has > 2 levels")
	}


return(x)
}

