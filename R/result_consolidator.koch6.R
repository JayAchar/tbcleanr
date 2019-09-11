#' Consolidate smear and culture results
#'
#' Take laboratory data set and consolidate repeated results to give summary variable
#' @inheritParams result_consolidator
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom dplyr case_when
#' @export 

result_consolidator.koch6 <- function(x, rm_orig = TRUE, ...) {

  
  # define culture vars
  culture_vars <- c("CultResult")
  
  # recode smear result
  x$smear <- dplyr::case_when(x$SmearResult == 1 ~ 0, 
                   x$SmearResult %in% c(0, 6:8) ~ NA_real_,
                   x$SmearResult == 5 ~ 1, 
                   TRUE ~ x$SmearResult)

  # factorise and order smear variable
  x$smear <- factor(x$smear, levels = 0:4,
                    labels = c("Negative", "Scanty", "1+", "2+", "3+"),
                    ordered = TRUE)
  
  # remove original smear variables
  if (rm_orig) x$SmearResult <- NULL
  

  # recode culture result
  x$culture <- dplyr::case_when(x$CultResult == 1 ~ 0,
                   x$CultResult == 2 ~ 1, 
                   x$CultResult %in% c(0, 3:5) ~ NA_real_, 
                   TRUE ~ x$CultResult)
  

  # factorise and order culture variable
  x$culture <- factor(x$culture, levels = 0:1,
                      labels = c("Negative", "Positive"),
                      ordered = TRUE)
  
  # remove original variables
  if (rm_orig) x$CultResult <- NULL
  
  x
  
}
