#' Fix EpiInfo treatment outcomes
#'
#' Combine treatment outcome variables to leave one 
#' factorised, labelled variable
#' @param x data frame containing outcome variables
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @importFrom dplyr case_when mutate
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @export 

outcome_fixer.epiinfo <- function(x, 
                                  rm_orig = TRUE, 
                                  ...) {
    
    
    # specify outcome vars
    v <- "RES"
    labs <- c("On treatment",
              "Cured",
              "Completed",
              "Death",
              "Fail",
              "LTFU",
              "Transfer out",
              "Fail & amplify",
              "Transfer to Cat 4")
    
    # check outcome variables present
    if (! all(v %in% names(x))) {
        stop("Check all outcome variables are included in data frame")
    }
    
    # new outcome variable
    x$outcome <- x[[v]]
    
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
