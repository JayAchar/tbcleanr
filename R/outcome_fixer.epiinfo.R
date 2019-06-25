#' Fix EpiInfo treatment outcomes
#'
#' Combine treatment outcome variables to leave one 
#' factorised, labelled variable
#' @param x data frame containing outcome variables
#' @param who_defined create new factor variable with levels defined by WHO (2013)
#' @param bin_outcome create new binary factor variable simplifying treatment outcome
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @importFrom dplyr case_when mutate
#' @importFrom rlang .data
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @export 

outcome_fixer.epiinfo <- function(x, 
                                  who_defined = TRUE,
                                  bin_outcome = TRUE,
                                  rm_orig = TRUE, 
                                  ...) {
    
    # capture class of original data
    start_class <- class(x)
    
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
    
    # create WHO defined outcome variable
    if (who_defined) {
    x <- x %>% 
        mutate(outcome_who = case_when(RES == 1 ~ "Cured", 
                                       RES == 2 ~ "Treatment completed",
                                       RES %in% c(4, 7) ~ "Treatment failed",
                                       RES == 3 ~ "Died",
                                       RES == 5 ~ "Lost to follow-up",
                                       RES == 6 ~ "Not evaluated", 
                                       RES == 8 ~ "Transfer to Cat 4 - remove from register",
                                       TRUE ~ NA_character_),
               outcome_who = factor(.data$outcome_who))
    }
    
    # create binary outcome variable 
    if (bin_outcome) {
        x <- x %>% 
            mutate(outcome_bin = case_when(outcome %in% c("Cured", "Completed") ~ "Treatment successful",
                                           outcome %in% c("Fail", "Death", "LTFU", "Fail & amplify") ~ "Treatment unsuccessful",
                                           TRUE ~ NA_character_),
                   outcome_bin = factor(.data$outcome_bin))
    }
    
    # remove original variables
    if (rm_orig) {
        x[, v] <- NULL
    }
 
    class(x) <- start_class
       
x
}
