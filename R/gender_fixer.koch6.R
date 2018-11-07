#' Factorise Koch 6 gender variable
#'
#' Take gender variable from Koch 6 data frame and factorise. 
#' @param x data frame containing Koch 6 admission variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' 

gender_fixer.koch6 <- function(x, ...) {
    
    # object class
    object_class <- class(x)
    
    # check gender variable present
    if (! "gender" %in% names(x)) {
        stop("Gender variable not present in data frame")
    }
    
    # message if gender levels are not all 1 or 2
    if (any(! x$gender %in% c(0:2))) message("Unrecognised gender levels detected - coerced to NA")
    
    # factorise gender variable
    x$gender <- factor(x$gender, levels = c(1:2),
                       labels = c("Male", "Female"))
    
    # check only 2 levels of gender variable
    assert_that(x$gender %>% levels() %>% length() == 2)
    
    # reapply object clas
    class(x) <- object_class
    
    x
}