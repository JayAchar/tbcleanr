#' Detangle ID number
#'
#' Handle TB data ID number based on database. If APID, split 
#' into 3 constituent parts to ascertain district, DS or DR TB 
#' categorisation and numerical ID number. If 
#' @param x data frame containing APID variable from KK programme
#' @param db define database being used - "k6", "epi_info"
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @import stringr
#' @examples
#' \dontrun{
#' apid_detangle(p)
#' }

id_detangle <- function(x, db = "k6", rm_orig = TRUE, ...) {
# acceptable values for "set" arg
	s <- c("k6", "epi_info")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check db is within acceptable values
	if (! db %in% s) {
			set_options <- paste(s, collapse = ", ")
			error_message <- paste("\'set\' arg should be ", set_options, sep = "")
		stop(error_message)
		}


# =================================================================
# set db specific variables 
		if (db == "k6") {
			id <- "registrationnb"
		}	
		if (db == "epi_info") {
			id <- "APID"
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

# if db = "k6"
	if (db == "k6") {

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
			x$id <- NA
				# extract all digits after characters
			x$id <- str_extract(x[[id]], "\\B\\d+$")
			x$id <- as.numeric(x$id)


		# check uniqueness of id number
			if (dummy == 0 && ! length(unique(x$id)) == dim(x)[1]) {
					warning("id is not a unique identifier in this data set")
			}

			if (dummy == 1 && ! dim(unique(x[c("idstring", "id")]))[1] == dim(x)[1]) {
					warning("id and idstring combination is not a unique identifier")
			}

	}



# if db == "epi_info"
	if (db == "epi_info") {
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
	x$id <- str_match(x[[id]], "\\d+")
	x$id <- as.numeric(x$id)
	}

	# remove original variables
	if (rm_orig %in% c("TRUE", "T")) {
		 		x[, id] <- NULL
		}

return(x)
}
