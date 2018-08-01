#' Consolidate DST results
#'
#' Take Chechen laboratory data set and consolidate DST results - choose aggregate
#' for categorised results
#' @param x data frame containing variables
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param aggregate choose whether to aggregate to categories or retain all drug results
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom purrr map_at
#' @importFrom stringr str_which
#' @examples
#' \dontrun{
#' dst_consolidator(p, set = "chechnya_myco_lab", aggregate = TRUE, rm_orig = TRUE)
#' }

dst_consolidator <- function(x, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"),
								aggregate = FALSE, 
								rm_orig = TRUE, ...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)

# ============================================================================
# define dst variables
	if (software == "excel" && project == "chechnya" && file == "lab") {
			dst_vars <- c("ms", "mr", "mh", "mz", "me", "mcm", "mam", "mlfx",
						"ljs", "ljr", "ljh", "ljz", "lje",
						"cts", "ctr", "cth", "ctz", "cte", "ctcm", "ctam", "ctlfx",
						"ctmfx", "ctmfx2", "ctlzd")
			rif <- c("mr", "ljr", "ctr")
			inh <- c("mh", "ljh", "cth")
			pza <- c("mz", "ljz", "ctz")
			eth <- c("me", "lje", "cte")
			cm <-  c("mcm", "ctcm")
			am <-  c("mam", "ctam")
			lfx <- c("mlfx", "ctlfx")
			mfx <- str_which(names(x), pattern = "ctmfx")
			mfx2 <- str_which(names(x), pattern = "ctmfx2")
			lzd <- str_which(names(x), pattern = "ctlzd")
			sli <- c("mcm", "mam","ctcm", "ctam")
			fq <- c("mlfx", "ctlfx", "ctmfx", "ctmfx2")
		} else if (software %in% c("excel", "epiinfo") && project == "kk" && file == "lab") {
			
			dst_vars <- c("MGITH", "MGITE", "MGITR", "MGITZ", 
						"H", "E", "R", "Z", "KM", "OF", "CAP",
						"H1", "E1", "R1", "Z1", "KM1", "OF1", "CAP1",
						"MFX1")
			rif <- c("R", "R1", "MGITR")
			inh <- c("H", "H1", "MGITH")
			pza <- c("Z", "Z1", "MGITZ")
			eth <- c("E", "E1", "MGITE")
			cm  <- c("CAP", "CAP1")
			km  <- c("KM", "KM1")
			ofx <- c("OF", "OF1")
			mfx <- str_which(names(x), pattern = "MFX1")
			sli <- c("CAP", "CAP1", "KM", "KM1")
			fq  <- c("OF", "OF1")

		} else {
			return(x)
		}

# recode all dst variables
	x[] <- map_at(x, .at = dst_vars, .f = dst_recoder,
				software = software, project = project, file = file)

# consolidate multiple methods for phenotypic dst
	x$dst_p_rif <- do.call(pmax, c(x[ , rif], na.rm = T))
	x$dst_p_inh <- do.call(pmax, c(x[ , inh], na.rm = T))

	if (aggregate == TRUE) {
		x$dst_p_sli <- do.call(pmax, c(x[ , sli], na.rm = T))
		x$dst_p_fq  <- do.call(pmax, c(x[ , fq], na.rm = T))
	} else {
		x$dst_p_pza <- do.call(pmax, c(x[ , pza], na.rm = T))
		x$dst_p_eth <- do.call(pmax, c(x[ , eth], na.rm = T))	
		x$dst_p_cm <- do.call(pmax, c(x[ , cm], na.rm = T))	
		x$dst_p_mfx <- x[[mfx]]

if (software == "excel" && project == "chechnya" && file == "lab") {
		x$dst_p_am <- do.call(pmax, c(x[ , am], na.rm = T))
		x$dst_p_lfx <- do.call(pmax, c(x[ , lfx], na.rm = T))
		x$dst_p_mfxhigh <- x[[mfx2]]
		x$dst_p_lzd <- x[[lzd]]
	}
if (software %in% c("excel", "epiinfo") && project == "kk" && file == "lab") {
		x$dst_p_km <- do.call(pmax, c(x[ , km], na.rm = T))
		x$dst_p_ofx <- do.call(pmax, c(x[ , ofx], na.rm = T))
	}
	}


# factorise all dst_p results
	where_names <- str_which(names(x), pattern = "dst_p")
	x[] <- map_at(x, .at = where_names, .f = factor, levels = 0:1,
								labels = c("Sensitive", "Resistant"))

# remove original variables
		 	if (rm_orig %in% c("TRUE", "T")) {
		 		x[, dst_vars] <- NULL
		 	}

x
}

