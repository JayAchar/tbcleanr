#' Fix Koch 6 treatment outcomes
#'
#' Combine  treatment outcome variables to leave one 
#' factorised, labelled variable
#' @param x data frame containing outcome variables
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @export


outcome_fixer.koch6 <- function(x, 
                                rm_orig = TRUE, 
                                ...) {
    
    
    # specify outcome vars
    # outcome variables in Koch 6
    v <- c("outfirst", "outfirst2013", "2013outcome")
    v1 <- "outfirst"
    v2 <- "outfirst2013"
    v3 <- "2013outcome"
    labs <- c("On treatment",
              "Cured",
              "Completed",
              "Death",
              "Fail",
              "LTFU",
              "Transfer out",
              "Other",
              "Transfer back to SCC")
    
    # check outcome variables present
    if (! all(v %in% names(x))) {
        stop("Check all outcome variables are included in data frame")
    }
    
    # generate final outcome variable
    x$outcome <- NA	
    
    # use all correct results from "outfirst" variable
    x$outcome[is.na(x[[v3]])] <- x[[v1]][is.na(x[[v3]])]
    
    # use all correct results from "outfirst2013" variable
    w <- which(x[[v3]] == "est 1 outcome 2013")
    x$outcome[w] <- x[[v2]][w]	
    
    # factorise outcome variable
    x$outcome <- factor(x$outcome, 
                        levels = c(0:8),
                        labels = labs)
    # remove original variables
    if (rm_orig) {
        x[, v] <- NULL
    }
    
    x
}
