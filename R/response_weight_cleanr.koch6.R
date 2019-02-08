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
#' @importFrom lubridate dmy
#' @importFrom rlang .data
#' @export
#'

response_weight_cleanr.koch6 <- function(x){
  
  # start class
  start_class <- class(x)
  
  # detect duplicate monthly weights
  duplicates <- x %>% 
    select(.data$RegistrationNb, .data$Date, .data$weight) %>% 
    filter(!is.na(.data$weight)) %>% 
    group_by(.data$RegistrationNb, .data$Date) %>% 
    mutate(num_wts = n()) %>% 
    filter(.data$num_wts == 2) %>% 
    ungroup() %>% 
    distinct(.data$RegistrationNb, .data$Date) %>% nrow()
  
  message(paste0
          ("Response weight: ", duplicates, " patient month weights duplicates removed."))
  
  # keep duplicate which is closest to preceeding month weight
  if (duplicates > 0) {
    x <- x %>%
      select(.data$RegistrationNb, .data$Date, .data$weight) %>%
      mutate(Date = lubridate::dmy(.data$Date)) %>% 
      filter(!(is.na(.data$weight))) %>%
      group_by(.data$RegistrationNb, .data$Date) %>%
      mutate(num_wts = n()) %>%
      ungroup() %>%
      group_by(.data$RegistrationNb) %>%
      mutate(previous_month = ifelse(.data$num_wts >1,
                                     # problems might arise if more than one month is duplicated for any specific patient
                                     lag(.data$Date),
                                     NA_integer_)) %>%
      ungroup()

    x <- x %>%
      group_by(.data$RegistrationNb, .data$Date) %>%
      mutate(abs_wt_diff = ifelse(.data$num_wts > 1,
                                  .data$weight - filter(x, .data$Date == min(.data$previous_month, na.rm = TRUE))$weight,
                                  NA_integer_)) %>%

      arrange(.data$RegistrationNb, .data$Date, .data$abs_wt_diff) %>%
      ungroup() %>%
      distinct(.data$RegistrationNb, .data$Date, .keep_all = TRUE) %>%
      select(.data$RegistrationNb, .data$Date, .data$weight)
  }


  class(x) <- start_class
  
  x
}


