#' Consolidate Xpert results
#'
#' Take laboratory data set and consolidate Xpert results from Chechen data
#' @param x data frame containing variables
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at map_df
#' @importFrom stringr str_detect
#' @export
#' @examples
#' \dontrun{
#' xpert_result_fixer(p, set = "chechnya_myco_lab", rm_orig = TRUE)
#' }


xpert_result_fixer <- function(x, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"),
								rm_orig = TRUE, ...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)

# =================================================================
# set db specific variables 
		if (software == "excel" && project == "chechnya" && file == "lab") {
			x_vars <- c("xpert1err", "xpert2err",
						"xpert1res", "xpert2res",
						"xpert1rif", "xpert2rif")
		}	else if (software %in% c("excel", "epiinfo") && project == "kk" && file == "lab") {
			x_vars <- c("GX_res1", "GX_res2", "GX_res3", "GX_res4")
		} else {
			return(x)
		}
# =================================================================
#check vars are within data frame
	if (! all(x_vars %in% names(x))) {
		stop("Xpert variables are not all present in data frame")
	}

	if (software == "excel" && project == "chechnya" && file == "lab") {
		# define sub variables
	err <- x_vars[1:2]
	res <- x_vars[3:4]
	rif <- x_vars[5:6]
		
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


# consolidate xpert results
	x$xpert_res <- do.call(pmax, c(x[ , res], na.rm = T))

# consolidate xpert rif result
	x$xpert_rif <- do.call(pmax, c(x[ , rif], na.rm = T))

	}

	if (software %in% c("excel", "epiinfo") && project == "kk" && file == "lab") {
		# reclassify errors across all variables
		fix_errors <- function(e) {
			# convert invalid results to NA
				e[e %in% c("ERROR", "INVALID", "NO RESULT")] <- NA
				e
		}

		# generate MTB result
			x_mtb <- function(m) {
				# not detected = 0
				m[m == "MTB NOT DETECTED"] <- 0

				# MTB detected 				
				detected <- str_detect(m, pattern = "MTB DETECTED")
				m[detected] <- 1	

				m
			}

		# generate rif result
			x_rif <- function(r) {
				# MTB not detected = NA
				not_detect <- str_detect(r, pattern = "MTB NOT DETECTED")
				r[not_detect] <- NA

				# not detected
				string <- str_detect(r, pattern = paste(c("Rif Resistance NOT DETECTED", "Rif Resistance INDETERMINATE"), collapse = "|"))
				r[string] <- 0

				# detected
				string_detected <- str_detect(r, pattern = "Resistance DETECTED")
				r[string_detected] <- 1

				r
			}

			# apply error fix to all original variables
			x <- map_at(.x = x, .at = x_vars, .f = fix_errors)

			# apply MTB result generator to all original variables
			mtb <- map_df(.x = x[x_vars], .f = x_mtb)
			y_names <- c("mtb1", "mtb2", "mtb3", "mtb4")
			names(mtb) <- y_names

			# apply rif resistance generator to all original variables
			resist <- map_df(.x = x[x_vars], .f = x_rif)
			r_names <- c("rif1", "rif2", "rif3", "rif4")
			names(resist) <- r_names

# consolidate xpert results
	x$xpert_res <- do.call(pmax, c(mtb[, y_names], na.rm = T))

# consolidate xpert rif result
	x$xpert_rif <- do.call(pmax, c(resist[, r_names], na.rm = T))

	x <- as.data.frame(x)
	}

# remove original variables
 	if (rm_orig %in% c("TRUE", "T")) {
 		x[, x_vars] <- NULL
 	}

# factorise final variables
 	x$xpert_res <- factor(x$xpert_res, levels = 0:1,
 					labels = c("Negative", "Positive"))

 	x$xpert_rif <- factor(x$xpert_rif, levels = 0:1,
 					labels = c("Not detected", "Detected"))


x
}
