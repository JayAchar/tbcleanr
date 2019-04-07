#' Add object class to identify admission database
#'
#' From variable characteristics identify data entry tool and assign
#' object class to identify database for further cleaning
#' @param x data frame containing raw TB admission data
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' 

adm_classr <- function(x) {

  assert_that(is.data.frame(x))

# if adm object class already present - stop further evaluation
  if (any(c("epiinfo", "koch6") %in% class(x))) {
    return(x)
  }

# names of variables in Epiinfo
  epiinfo_varnames <- c("APID", "BIRTDATE", "BDQDBDQ", "DATEDEAT")

# names of variables in Koch_6
  k6_varnames <- c("registrationnb", "dateofbirth", "Bdq", "dateout")


# assign admission file object class
  if (all(epiinfo_varnames %in% names(x))) {

          class(x) <- c(class(x), "epiinfo")

  } else if (all(k6_varnames %in% names(x))) {

          class(x) <- c(class(x), "koch6")

  }

# warning if no object class assigned
  if (! any(c("epiinfo", "koch6") %in% class(x)))
    warning("Data set not recognised - no object class assigned")

x
}
