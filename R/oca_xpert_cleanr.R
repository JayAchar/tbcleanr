#' Bentiu Xpert laboratory data cleaning
#'
#' This function takes the Bentiu PoC lab dataset as a data.frame and
#' removes unnecessary variables, cleans results and generates new 
#' result variables.
#' @param x data frome including Bentiu PoC lab Xpert data
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @export
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertive assert_is_data.frame
#' @importFrom stringr str_detect
#' @examples
#' \dontrun{
#' oca_xpert_reporter(p)
#' }


oca_xpert_cleanr <- function(x) {

# check x is data.frame
  assert_is_data.frame(x)

# =================================== #
# ========  clean  =========================== #
# keep relevant variables
	keep <- c("Patient ID", "MTB; RIF", "Assay", "ok", "date")

	# check variables are included
	if (!all((keep %in% names(x)))) {
		stop("Variable names not all recognised in raw data.")
	}

	# keep varaibles
	x <- x[, which(names(x) %in% keep)]

# rename variables
	names(x)[names(x) == "MTB; RIF"] 	<- "result"
	names(x)[names(x) == "Assay"] 		<- "test"
	names(x)[names(x) == "Patient ID"] 	<- "lab_id"
	names(x)[names(x) == "ok"] 	<- "status"

# remove rows with all NA
	x <- x[! rowSums(is.na(x)) == ncol(x), ]

# fix date
	x <- oca_xpert_date(x)

# =================================== #
# ============== clean variables ===================== #
# fix patient ID 


# =================================== #
# fix test variable
	if (length(table(x$test)) == 3) {
		x$test[str_detect(x$test, "MTB-RIF")] <- "tb"
		x$test[str_detect(x$test, "Viral Load")] <- "hiv_viral_load"
		x$test[str_detect(x$test, "Qual")] <- "hiv_qual"
	} else {
		warning("All test types have not been recognised. 
			Suggest reviewing oca_xpert_reporter function.")
	}
# =================================== #
# check and fix status variable
	x$status[str_detect(x$status, "Error")] <- "Error"

	if (! dim(table(x$status)) == 2) {
		warning("All status values have not been recognised. 
			Suggest reviewing oca_xpert_reporter function.")
	}

# =================================== #
# fix result variable

	missing_result <- sum(is.na(x$result))
	if (missing_result > 0) {
		warning(paste0("Missing results = ", missing_result))
	}

	# convert invalid results to NA
		invalid <- c("ERROR", "INVALID", "NO RESULT")
		x$result[which(x$result %in% invalid)] <- NA

# MTB
	x$result[str_detect(x$result, "MTB DETECTED") &
						(str_detect(x$result, "Resistance NOT") |
						str_detect(x$result, "INDETERMINATE"))] <- 1L
	x$result[str_detect(x$result, "MTB DETECTED") &
						str_detect(x$result, "Resistance DETECTED")] <- 2L
	x$result[str_detect(x$result, "MTB NOT DETECTED")] <- 0L


# HIV
	# EID
	x$result[str_detect(x$result, "^HIV-1 DETECTED$")] <- 1L
	x$result[str_detect(x$result, "^HIV-1 NOT DETECTED$")] <- 0L

	# VL
	x$result[str_detect(x$result, "^HIV-1 DETECTED <")]	<- 0L
	x$result[str_detect(x$result, "^HIV-1 DETECTED \\d") |
						str_detect(x$result, "^HIV-1 DETECTED >")] <- 1L

x$result <- as.numeric(x$result)


x
}

