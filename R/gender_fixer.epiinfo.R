#' Factorise Epiinfo gender variable
#'
#' Take gender variable from Epiinfo data frame and factorise. 
#' @param x data frame containing Koch 6 admission variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' 

gender_fixer.epiinfo <- function(x, ...) {
    
    # object class
    object_class <- class(x)
    
    # check gender variable present
    if (! "SEX" %in% names(x)) {
        stop("Gender variable not present in data frame")
    }
    
    # message if gender levels are not all 1 or 2
    if (any(! x$SEX %in% c("M", "F"))) message("Unrecognised gender levels detected - coerced to NA")

    # convert character to numeric
    x$SEX[x$SEX == "M"] <- 1
    x$SEX[x$SEX == "F"] <- 2
    
    # factorise gender variable
    x$SEX <- factor(x$SEX, levels = c(1:2),
                       labels = c("Male", "Female"))
    
    # check only 2 levels of gender variable
    assert_that(x$SEX %>% levels() %>% length == 2)
    
    # reapply object clas
    class(x) <- object_class

    x
}