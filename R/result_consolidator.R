#' Consolidate smear results
#'
#' Take laboratory data set and consolidate repeated results to give summary variable
#' @param x data frame containing variables
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param test smear or culture
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom purrr map_at
#' @examples
#' \dontrun{
#' result_consolidator(p, set = "chechnya_myco_lab", rm_orig = TRUE)
#' }


result_consolidator <- function(x, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"), 
								test = c("smear", "culture"), 
								rm_orig = TRUE, ...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)
	test <- match.arg(test)

# smear recoding
	if (test == "smear") {

		# define smear variables
		if (software == "excel" && project == "chechnya" && file == "lab") {
			smear_vars <- c("micro1", "micro2", "micro3", "ctmicres")
			} else if (software %in% c("excel", "epiinfo") && project == "kk" && file == "lab") {
			smear_vars <- c("BK1", "BK2", "BK3")	
			} else {
			return(x)
			}

		# use smear_recode function
				x[] <- map_at(x, .at = smear_vars, .f = smear_recode,
					software = software, project = project, file = file)

		# find maximum smear for each sample
			x$smear <- do.call(pmax, c(x[ , smear_vars], na.rm = T))

		# factorise and order smear variable
		 	x$smear <- factor(x$smear, levels = 0:3,
		 		labels = c("Negative", "1+", "2+", "3+"),
				ordered = T)

		# remove original variables
		 	if (rm_orig %in% c("TRUE", "T")) {
		 		x[, smear_vars] <- NULL
		 	}
	}

# culture recoding
	if (test == "culture") {
		
		# define culture variables
		if (software == "excel" && project == "chechnya" && file == "lab") {
			culture_vars <- c("mgitres", "ljres", "ctmgitres")
		} else if (software %in% c("excel", "epiinfo") && project == "kk" && file == "lab") {
			culture_vars <- c("RES", "RES02", "RES03", "RES04", "RESULT", 
								"RESULT02", "RESULT03", "RESULT04")
		}	

		# use smear_recode function
				x[] <- map_at(x, .at = culture_vars, .f = culture_recode, 
					software = software, project = project, file = file)

		# find maximum culture for each sample
			x$culture <- do.call(pmax, c(x[ , culture_vars], na.rm = T))

		# factorise and order culture variable
		 	x$culture <- factor(x$culture, levels = 0:1,
		 		labels = c("Negative", "Positive"),
				ordered = T)

		# remove original variables
		 	if (rm_orig %in% c("TRUE", "T")) {
		 		x[, culture_vars] <- NULL
		 	}

	}
x
}