#' Consolidate DST results
#'
#' Take laboratory data set and consolidate DST results - choose aggregate
#' for categorised results
#' @param x data frame containing variables
#' @param aggregate choose whether to aggregate to categories or retain all drug results
#' @param rm_orig remove original variables - TRUE or FALSE
#' @importFrom stringr str_which
#' @importFrom purrr map_at
#' @importFrom dplyr recode
#' @importFrom magrittr %>% 
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export 

dst_consolidator.epiinfo <- function(x, aggregate = FALSE, rm_orig = TRUE) {
  
  # define dst variables
  dst_vars <- c("MGITH", "MGITE", "MGITR", "MGITZ", 
                "H", "E", "R", "Z", "KM", "OF", "CAP",
                "H1", "E1", "R1", "Z1", "KM1", "OF1", "CAP1",
                "MFX1")
  rif <- c("R", "R1", "MGITR")
  inh <- c("H", "H1", "MGITH")
  pza <- c("Z", "Z1", "MGITZ")
  eth <- c("E", "E1", "MGITE")
  cm  <- c("CAP", "CAP1")
  km  <- c("KM", "KM1")
  ofx <- c("OF", "OF1")
  mfx <- str_which(names(x), pattern = "MFX1")
  sli <- c("CAP", "CAP1", "KM", "KM1")
  fq  <- c("OF", "OF1")
  
  # recode all dst variables
  x[] <- map_at(x, .at = dst_vars, .f = ~ as.character(.x) %>% 
                  dplyr::recode(.x = ., `1` = 1L, `2` = 0L, .default = NA_integer_))
  
  # consolidate multiple methods for phenotypic dst
  x$dst_p_rif <- do.call(pmax, c(x[ , rif], na.rm = T))
  x$dst_p_inh <- do.call(pmax, c(x[ , inh], na.rm = T))
  
  if (aggregate == TRUE) {
    x$dst_p_sli <- do.call(pmax, c(x[ , sli], na.rm = T))
    x$dst_p_fq  <- do.call(pmax, c(x[ , fq], na.rm = T))
  } else {
    x$dst_p_pza <- do.call(pmax, c(x[ , pza], na.rm = T))
    x$dst_p_eth <- do.call(pmax, c(x[ , eth], na.rm = T))	
    x$dst_p_cm <- do.call(pmax, c(x[ , cm], na.rm = T))	
    x$dst_p_mfx <- x[[mfx]]
  
    x$dst_p_km <- do.call(pmax, c(x[ , km], na.rm = T))
    x$dst_p_ofx <- do.call(pmax, c(x[ , ofx], na.rm = T))  
  }
    # factorise all dst_p results
    where_names <- str_which(names(x), pattern = "dst_p")
    x[] <- map_at(x, .at = where_names, .f = factor, levels = 0:1,
                  labels = c("Sensitive", "Resistant"))
    
    # remove original variables
    if (rm_orig) {
      x[, dst_vars] <- NULL
    }
    
  x
}
