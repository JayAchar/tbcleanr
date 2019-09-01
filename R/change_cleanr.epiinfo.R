#' Clean drug change data
#'
#' Take drug change data from routine TB programmes and clean for 
#' further analysis.
#' 
#' @inheritParams change_cleanr
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @importFrom lubridate dmy
#' @importFrom purrr as_mapper modify_at
#' @export

change_cleanr.epiinfo <- function(x, 
                                  add = NULL) {

# save class
  start_class <- class(x)
  
# check and adjust add arg
  add <- add[add %in% names(x)]
  
## Adjust variable names
  # variable names to change
  original_names <- c(
    "DACHAN",
    "CXG",
    "CXI",
    "CXC",
    "CCLO",
    "CXE",
    "CXH",
    "CXF",
    "CH",
    "CR",
    "CE",
    "CZ",
    "WS",
    "CKA",
    "CCAP",
    "COFL",
    "CETH",
    "CCYC",
    "CPAS",
    "CAMX",
    "CXD"
  )
  
  # define new names for drug variables
  new_names <- c(
    "change_dt",
    "bdq_change",
    "dlm_change",
    "mfx_change",
    "cfz_change",
    "lfx_change",
    "imp_change",
    "lzd_change",
    "inh_change",
    "rif_change",
    "eth_change",
    "pza_change",
    "str_change",
    "kan_change",
    "cap_change",
    "ofx_change",
    "eto_change",
    "cyc_change",
    "pas_change",
    "amx_change",
    "pto_change"
  )

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
  ## subset to only keep specified variables
  x <- x[, c("APID", new_names, add)]

    ## convert drug dose changes to NA
    x[x == 3] <- NA
  
    # generate count variable to look for missing values in drug changes
    x$row_na <- as.numeric(rowSums(is.na(x[, drug_names])))

  # remove rows with all NA in drug variables
  x <- x[x$row_na < length(drug_names), ]
    
  x$row_na <- NULL
    
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
  
  # re-apply starting class
  class(x) <- start_class

  x
}
