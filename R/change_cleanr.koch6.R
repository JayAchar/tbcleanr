#' Clean drug change data
#'
#' Take drug change data from routine TB programmes and clean for 
#' further analysis.
#' 
#' @param x data frame containing drug variables
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @importFrom lubridate dmy
#' @importFrom rlang .data quos 
#' @importFrom dplyr select filter
#' @importFrom purrr as_mapper modify_at
#' @export

change_cleanr.koch6 <- function(x) {
  
  # save class
  start_class <- class(x)
  
  ## Adjust variable names
  # variable names to change
  original_names <- c("changedate", 
                      "CBdq", 
                      "CDld", 
                      "CMfx", 
                      "CCfz", 
                      "CLfx",
                      "CImpCln",
                      "CLzd")
  
  # define new names for drug variables
  new_names <- c("change_dt", 
                 "bdq_change",
                 "dlm_change",
                 "mfx_change",
                 "cfz_change", 
                 "lfx_change",
                 "imp_change",
                 "lzd_change")
  
  # define drug variables only
  drug_names <- new_names[!new_names == "change_dt"]
  
  # confirm all variables present in data set
  assert_that(all(original_names %in% names(x)))
  
  # check length of old vs new names
  assert_that(length(original_names) == length(new_names))
  
  # Change selected variable names
  name_position <- match(original_names, names(x))
  
  names(x)[name_position] <- new_names
  
  ## Subset and clean data
  # variables to retain
  keep_vars <- rlang::quos(c("RegistrationNb", new_names))
  
  x <- x %>% 
    ## subset to only keep specified variables
    select(!!! keep_vars)
  
  ## convert drug dose changes to NA
  x[x == 3] <- NA_integer_
  
  ## convert 0 (ND) to NA
  x[x == 0] <- NA_integer_
  
  ## convert 4 (stop permanently) to 2 (stop)
  x[x == 4] <- 2L
  
  # generate count variable to look for missing values in drug changes
  x$row_na <- as.numeric(rowSums(is.na(x[, drug_names])))
  
  # remove rows with all NA in drug variables
  x <- x %>% 
    filter(.data$row_na < length(drug_names)) %>% 
    # remove NA count
    select(- .data$row_na)
  
  # define mapper to convert drugs to factors
  fct_drugs <- purrr::as_mapper(~ factor(.x, 
                                         levels = 1:2, 
                                         labels = c("Start", "Stop")))
  # convert drug variables to factor
  x <- x %>% 
    purrr::modify_at(.f = fct_drugs,
                     .at = drug_names)
  
  # convert to dates
  x$change_dt <- lubridate::dmy(x$change_dt)
  
  # convert all variable to lower case
  names(x) <- tolower(names(x))
  
  # re-apply starting class
  class(x) <- start_class
  
  x
}
