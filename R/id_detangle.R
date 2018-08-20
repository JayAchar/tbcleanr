#' Detangle ID number
#'
#' Handle TB data ID number based on database. If APID, split 
#' into 3 constituent parts to ascertain district, DS or DR TB 
#' categorisation and numerical ID number. If 
#' @param x data frame containing APID variable from KK programme
#' @param software define software being used
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom stringr str_match str_extract
#' @importFrom assertive assert_is_data.frame
#' @examples
#' \dontrun{
#' apid_detangle(p)
#' }

id_detangle <- function(x, software = c("excel", "koch_6", "epiinfo"), 
							project = c("kk", "chechnya"),
							file = c("adm", "lab", "clinical_lab"), 
							rm_orig = FALSE, ...) {

# check input
	assert_is_data.frame(x)

# check all args
	software <- match.arg(software)


# =================================================================
# set software specific variables 
		if (software == "koch_6" & file != "lab") {

			id <- "registrationnb"

		} else if (software %in% c("epiinfo", "excel") &
				 project == "kk") {

			id <- "APID"

		} else {
			return(x)
		}
# =================================================================

# check variable exists
	if (! id %in% names(x)) {
		stop("ID variable not present in data frame")
	} 

# warning regarding missing APID in records
	if (sum(is.na(x[[id]])) > 0) {
		m <- sum(is.na(x[[id]]))
		warning(paste("There are", m, "missing ID values in this dataset"))
	}

# if software = "koch_6"
	if (id == "registrationnb") {

		# check if all string identifiers are teh same
			z <- str_match(x[[id]], "^.{3}")
			if (length(unique(z)) == 1) {
				message("All string identifiers in ID number identical and therefore dropped")
			dummy <- 0
			} else {
		# keep ID string if useful identifier
				x$idstring <- as.character(z)
			dummy <- 1
			}

		# extract numerical part of id
				# extract all digits after characters
			x$idno <- str_extract(x[[id]], "\\B\\d+$")
			x$idno <- as.numeric(x$idno)


		# check uniqueness of id number
			if (dummy == 0 && ! length(unique(x$idno)) == dim(x)[1]) {
					warning("id is not a unique identifier in this data set")
			}

			if (dummy == 1 && ! dim(unique(x[c("idstring", "idno")]))[1] == dim(x)[1]) {
					warning("id and idstring combination is not a unique identifier")
			}

	}


	if (id == "APID") {
		# if 4th character is "D" = DS TB
			x$ds_dr <- NA		# new empty variable
			ds <- grep("^.{3}D", x[[id]])
				x$ds_dr[ds] <- 0
				x$ds_dr[is.na(x$ds_dr)] <- 1
				x$ds_dr <- factor(x$ds_dr, 
							levels = c(0,1),
							labels = c("DS-TB", "DR-TB"))
			x$ds_dr[is.na(x[[id]])] <- NA

# take rayon and generate district variable
	x$district <- str_match(x[[id]], "[[:alpha:]]{3}")
	x$district <- as.character(x$district)

# take all digits and generate id number
	x$idno <- str_match(x[[id]], "\\d+")
	x$idno <- as.numeric(x$idno)

	}


	# remove original variables
	if (rm_orig %in% c("TRUE", "T")) {
		 		x[, id] <- NULL
		} else {
	# rename original id variable
			colnames(x)[match(id, colnames(x))] <- "id"
		}

x
}
