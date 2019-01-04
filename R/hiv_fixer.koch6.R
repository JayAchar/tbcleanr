#' Factorise Koch 6 HIV variable
#'
#' Take HIV variables from data frame and factorise. 
#' @param x data frame containing Koch 6 admission variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>% 
#' @importFrom rlang .data
#' @export 

hiv_fixer.koch6 <- function(x, ...) {
    
# check variables are present
    if (! all(c("HIV", "cdhivenrol") %in% names(x))) {
        stop("Required HIV variables not included in data frame")
    }
    
# check that levels for HIV variable are correct
    assert_that(unique(x$cdhivenrol) %in% c(0:5, NA_integer_) %>% all())

# save object class
    object_class <- class(x)
    
# recode cdhivenrol
    # 0 = not defined, 1 = positive, 2 = negative, 3 = not done
    # 4 = unknown, 5 = pending
    x$cdhivenrol[x$cdhivenrol == 0] <- NA_integer_
    x$cdhivenrol[x$cdhivenrol %in% c(3:5)] <- NA_integer_
    x$cdhivenrol[x$cdhivenrol == 2] <- 0L

# recode hiv 
    # 0 = Unknown, 1 = positive, 2 = negative, 3 = not done, 4 = pending
    x$HIV[x$HIV %in% c(0, 3:4)] <- NA_integer_
    x$HIV[x$HIV == 2] <- 0
    
# combine known status with baseline hiv result
    x <- x %>% 
        mutate(hiv_sum = rowSums(x[c("HIV", "cdhivenrol")], na.rm = T)) %>% 
        mutate(HIV = ifelse(.data$hiv_sum >= 1, 1, .data$HIV)) %>% 
        mutate(HIV = ifelse(is.na(.data$HIV), .data$cdhivenrol, .data$HIV)) %>% 
        select(-.data$hiv_sum, -.data$cdhivenrol)

    
# factorise gender variable
    x$HIV <- factor(x$HIV, levels = c(0:1),
                    labels = c("Negative", "Positive"))    

# reattach object_class
    class(x) <- object_class
    
    x
}
