#' Treatment response weight cleanr
#'
#' @param x adherence or BMI data frame including monthly weight measurements
#'
#' @return data frame with treatment month and weight per patient. Duplicate
#' monthly measurements are removed with weights closest to the preceeding month
#' retained. 
#' @seealso \code{\link{tbcleanr}}
#' @author Jay Achar 
#' @importFrom dplyr %>% select filter group_by mutate n distinct ungroup lag arrange
#' rename
#' @importFrom rlang .data
#' @export
#'

response_weight_cleanr.epiinfo <- function(x){
  
  # start class
  start_class <- class(x)
  
  # detect duplicate monthly weights
  duplicates <- x %>% 
    select(.data$APID, .data$FOLAFT, .data$WEIGHT) %>% 
    filter(!is.na(.data$WEIGHT)) %>% 
    group_by(.data$APID, .data$FOLAFT) %>% 
    mutate(num_wts = n()) %>% 
    filter(.data$num_wts == 2) %>% 
    ungroup() %>% 
    distinct(.data$APID, .data$FOLAFT) %>% nrow()
  
  message(paste0
          ("Response weight: ", duplicates, " patient month weights duplicates removed."))
  
  # keep duplicate which is closest to preceeding month weight
  if (duplicates > 0) {
   x <- x %>% 
      select(.data$APID, .data$FOLAFT, .data$WEIGHT) %>% 
      filter(!(is.na(.data$WEIGHT))) %>% 
      group_by(.data$APID, .data$FOLAFT) %>% 
      mutate(num_wts = n()) %>%  
      ungroup() %>% 
      group_by(.data$APID) %>% 
      mutate(previous_month = ifelse(.data$num_wts >1,
                                     # problems might arise if more than one month is duplicated for any specific patient
                                     lag(.data$FOLAFT),
                                     NA_integer_)) %>% 
      ungroup()
   
   x <- x %>% 
      group_by(.data$APID, .data$FOLAFT) %>%
      mutate(abs_wt_diff = ifelse(.data$num_wts > 1,
                                  .data$WEIGHT - filter(x, .data$FOLAFT == min(.data$previous_month, na.rm = TRUE))$WEIGHT,
                                 NA_integer_)) %>%

      arrange(.data$APID, .data$FOLAFT, .data$abs_wt_diff) %>%
      ungroup() %>% 
      distinct(.data$APID, .data$FOLAFT, .keep_all = TRUE) %>%
      select(.data$APID, .data$FOLAFT, .data$WEIGHT)
  }
    
  x <- x %>% 
    rename(tx_month = .data$FOLAFT)

  class(x) <- start_class
  
  x
}


