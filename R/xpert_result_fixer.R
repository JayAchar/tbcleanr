#' Consolidate Xpert results
#'
#' Take laboratory data set and consolidate Xpert results from Chechen data
#' @param x data frame containing variables
#' @param set define variable set to apply. Values can be - "chechnya_myco_lab",
#'  "nukus_myco_lab".
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' xpert_result_fixer(p, set = "chechnya_myco_lab", rm_orig = TRUE)
#' }


xpert_result_fixer <- function(x, set, rm_orig = TRUE, ...) {

# acceptable values for "set" arg
	s <- c("chechnya_myco_lab", "nukus_myco_lab")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check set is within acceptable values
	if (! set %in% s) {
		stop("Specify set argument within specified values")
	}

# =================================================================
# set db specific variables 
		if (set == "checknya_myco_lab") {
			x_vars <- c("xpert1err", "xpert2err",
						"xpert1res", "xpert2res",
						"xpert1rif", "xpert2rif")
		}	
		if (set == "nukus_myco_lab") {
			x_vars <- c("GX_res1", "GX_res2", "GX_res3")
		}
# =================================================================
#check vars are within data frame
	if (! all(x_vars %in% names(x))) {
		stop("Xpert variables are not all present in data frame")
	}

	if (set == s[1]) {
		# define sub variables
	err <- xpert_vars[1:2]
	res <- xpert_vars[3:4]
	rif <- xpert_vars[5:6]
		
		# recode errors
					error_adjust <- function(e) {
						e[!is.na(e)] <- 1
						e[is.na(e)] <- 0
						e <- as.numeric(e)
					return(e)
					}
	x[ , err] <- lapply(x[, err], error_adjust)
		
		# recode results applying error corrections
	x$xpert1res[x$xpert1res == "MTB NOT DETECTED"] <- 0
	x$xpert1res[! is.na(x$xpert1res) & ! x$xpert1res == 0] <- 1
	x$xpert1res[x$xpert1err == 1] <- NA

	x$xpert2res[x$xpert2res == "MTB NOT DETECTED"] <- 0
	x$xpert2res[! is.na(x$xpert2res) & ! x$xpert2res == 0] <- 1
	x$xpert2res[x$xpert2err == 1] <- NA

		# consolidate rif results
				rif_adjust <- function(r) {
					r[r == "NOT DETECTED"] <- 0
					r[r == "INDETERMINATE"] <- NA
					r[r == "DETECTED"] <- 1
					r <- as.numeric(r)
				return(r)
				}
	x[ ,rif] <- lapply(x[ ,rif], rif_adjust)

	}

	if (set == s[2]) {

			x_split <- function(x) {
				# convert invalid results to NA
				invalid <- c("ERROR", "INVALID", "NO RESULT")
				x[x %in% invalid] <- NA

				# not detected = 0
				x[x == "MTB NOT DETECTED"] <- 0

				# detected - rif sensitive
				

				# detected - rif resistant





				x
			}


	}









# consolidate xpert results
	x$xpert_res <- do.call(pmax, c(x[ , res], na.rm = T))

# consolidate xpert rif result
	x$xpert_rif <- do.call(pmax, c(x[ , rif], na.rm = T))

# remove original variables
 	if (rm_orig %in% c("TRUE", "T")) {
 		x[, xpert_vars] <- NULL
 	}

# factorise final variables
 	x$xpert_res <- factor(x$xpert_res, levels = 0:1,
 					labels = c("Negative", "Positive"))

 	x$xpert_rif <- factor(x$xpert_rif, levels = 0:1,
 					labels = c("Not detected", "Detected"))


return(x)
}
