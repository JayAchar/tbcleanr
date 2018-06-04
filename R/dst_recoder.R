#' Recode DST results
#'
#' Recode individual DST variables
#' @param dst_var DST variable 
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' dst_recoder(dst_var)
#' }


dst_recoder <- function(dst_var) {


# check var is.character
	if (! is.character(dst_var)) {
		stop("DST variable should be is.character")
	}
# check number of levels of smeDSTar variable
	if (length(table(dst_var)) > 2) {
		stop("Check the number of DST variable levels")
	}

# ===================================================
# Recode DST variable	

		dst_var[dst_var == "R"] <- 1
		dst_var[dst_var == "S"] <- 0


# convert to numerical
	dst_var <- as.numeric(dst_var)

return(dst_var)
}