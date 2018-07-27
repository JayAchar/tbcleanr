#' Consolidate Hain MTBDRplus results
#'
#' Take laboratory data set and consolidate Hain MTBDRplus results from Chechen data
#' @param x data frame containing variables
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at
#' @export
#' @examples
#' \dontrun{
#' mtbdrplus_fixer(p, set = "chechnya")
#' }


mtbdrplus_fixer <- function(x, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"),
								 ...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}
# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)

# =========================================================
#specify hain variables
	if (software == "excel" && project == "chechnya" && file == "lab") {
		hain_vars <- c("cthres", "cthrifres", "cthinhres")
		h_resist <- c("cthinhres", "cthrifres")
	} else if (software %in% c("excel", "epiinfo") && project == "kk" && file == "lab") {
		h_resist <- c("HAINH", "HAINR")
	} else {
		return(x)
	}
# =========================================================

# recode hain resistance results
	x[] <- map_at(x, .at = h_resist, .f = dst_recoder,
				software = software, project = project, file = file)

# factorise hain resistance results
	x[] <- map_at(x, .at = h_resist, .f = factor, levels = 0:1,
								labels = c("Sensitive", "Resistant"))
# rename resistance vars
	colnames(x)[match(h_resist, colnames(x))] <- c("hain_inh", "hain_rif")

# recode hain result
if (exists("hain_vars")) {
	x$hain_res <- culture_recode(x$cthres, 
					software = software, project = project, file = file)
	# remove original hain result variable
	x$cthres <- NULL

} else {
	return(x)
}
	
	x$hain_res <- factor(x$hain_res, levels = 0:1,
					labels = c("Negative", "Positive"))

x
}