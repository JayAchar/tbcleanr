#' Factorise Epiinfo HIV variable
#'
#' Take HIV variables from data frame and factorise. 
#' @param x data frame containing EpiInfo admission variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export 

hiv_fixer.epiinfo <- function(x, ...) {

# check variables are present
    if (! "HIV" %in% names(x)) {
        stop("Required HIV variables not included in data frame")
    }    

# check that levels for HIV variable are correct
    assert_that(unique(x$HIV) %in% c(1:3, NA_integer_) %>% all())
    
# recode variable
    # 1 = positive; 2 = negative; 3 = pending
    x$HIV[x$HIV == 2] <- 0L
    x$HIV[x$HIV == 3] <- NA_integer_

# factorise gender variable
    x$HIV <- factor(x$HIV, levels = c(0:1),
                    labels = c("Negative", "Positive"))    
    
  x  
}
