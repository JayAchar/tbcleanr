#' Consolidate Hain MTBDRplus results
#'
#' Take laboratory data set and consolidate Hain MTBDRplus results from Chechen data
#' @param x data frame containing variables
#' @param set define variable set to apply. Values can be - "chechnya"
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at
#' @export
#' @examples
#' \dontrun{
#' mtbdrplus_fixer(p, set = "chechnya")
#' }


mtbdrplus_fixer <- function(x, set = "chechnya_myco_lab", ...) {
# acceptable values for "set" arg
	s <- c("chechnya_myco_lab")	

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check set is within acceptable values
	if (! set %in% s) {
			set_options <- paste(s, collapse = ", ")
			error_message <- paste("\'set\' arg should be ", set_options, sep = "")
		stop(error_message)	
	}

# =========================================================
#specify hain variables
	if (set == "chechnya_myco_lab") {
		hain_vars <- c("cthres", "cthrifres", "cthinhres")
		h_resist <- c("cthinhres", "cthrifres")
	}

# recode hain resistance results
	x[] <- map_at(x, .at = h_resist, .f = dst_recoder)

# factorise hain resistance results
	x[] <- map_at(x, .at = h_resist, .f = factor, levels = 0:1,
								labels = c("Sensitive", "Resistant"))

# recode hain result
	x$hain_res <- culture_recode(x$cthres)
	x$hain_res <- factor(x$hain_res, levels = 0:1,
					labels = c("Negative", "Positive"))

# rename resistance vars
	colnames(x)[match(h_resist, colnames(x))] <- c("hain_inh", "hain_rif")

# remove original hain result variable
	x$cthres <- NULL

return(x)
}