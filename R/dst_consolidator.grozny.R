#' Consolidate DST results
#'
#' Take laboratory data set and consolidate DST results - choose aggregate
#' for categorised results
#' @param x data frame containing variables
#' @param aggregate choose whether to aggregate to categories or retain all drug results
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @importFrom stringr str_which
#' @importFrom purrr map_at
#' @importFrom dplyr recode
#' @importFrom magrittr %>%
#' @seealso \code{\link{tbcleanr}}

dst_consolidator.grozny <- function(x, aggregate = FALSE, rm_orig = TRUE) {
  
  # define dst variables
  dst_vars <- c("ms", "mr", "mh", "mz", "me", "mcm", "mam", "mlfx",
                "ljs", "ljr", "ljh", "ljz", "lje",
                "cts", "ctr", "cth", "ctz", "cte", "ctcm", "ctam", "ctlfx",
                "ctmfx", "ctmfx2", "ctlzd")
  rif <- c("mr", "ljr", "ctr")
  inh <- c("mh", "ljh", "cth")
  pza <- c("mz", "ljz", "ctz")
  eth <- c("me", "lje", "cte")
  cm <-  c("mcm", "ctcm")
  am <-  c("mam", "ctam")
  lfx <- c("mlfx", "ctlfx")
  mfx <- str_which(names(x), pattern = "ctmfx")
  mfx2 <- str_which(names(x), pattern = "ctmfx2")
  lzd <- str_which(names(x), pattern = "ctlzd")
  sli <- c("mcm", "mam","ctcm", "ctam")
  fq <- c("mlfx", "ctlfx", "ctmfx", "ctmfx2")
  
  # recode all dst variables
  x[] <- map_at(x, .at = dst_vars, .f = ~ as.character(.x) %>% 
                  dplyr::recode(.x = ., "R" = 1L, "S" = 0L, .default = NA_integer_))
  
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
  
    x$dst_p_am <- do.call(pmax, c(x[ , am], na.rm = T))
    x$dst_p_lfx <- do.call(pmax, c(x[ , lfx], na.rm = T))
    x$dst_p_mfxhigh <- x[[mfx2]]
    x$dst_p_lzd <- x[[lzd]]
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
  