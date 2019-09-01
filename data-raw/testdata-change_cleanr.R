## Test data for change_cleanr()

library(magrittr)
new_patient <- function(id_number,
                        change_type) {
  
  assertthat::assert_that(is.numeric(id_number),
                          is.numeric(change_type))
  
  id <- paste0("XYZ", id_number)
  
  
  COFL <- change_type
  
  
  out <- data.frame(APID = id,
                    DACHAN = "1/1/2010",
                    STARTTRE = NA_integer_,
                    MEDTT = NA_integer_,
                    REACHAN = sample(1:18, 1),
                    CXG = NA_integer_,
                    CXI = NA_integer_,
                    CXC = NA_integer_,
                    CCLO = NA_integer_,
                    CXE = NA_integer_,
                    CXH = NA_integer_,
                    CXF = NA_integer_,
                    CH = NA_integer_,
                    CR = NA_integer_,
                    CE = NA_integer_,
                    CZ = NA_integer_,
                    WS = NA_integer_,
                    CKA = NA_integer_,
                    CCAP = NA_integer_,
                    COFL = COFL,
                    CETH = NA_integer_,
                    CCYC = NA_integer_,
                    CPAS = NA_integer_,
                    CAMX = NA_integer_,
                    CXD = NA_integer_,
                    stringsAsFactors = FALSE)
  out
}

# define each patient
records <- list(
  # stop drug
  list(1, 2),
  # start drug
  list(2, 1),
  # change drug
  list(3, 3),
  # all NA
  list(4, NA_integer_)
)

# construct formula to generate data frame
num <- length(records[[1]])
args <- paste0(".x[[", 1:num, "]]", collapse = ", ")
form <- as.formula(paste0("~ new_patient(", args, ")"))

epi <- purrr::map(records, .f = form) %>% 
  dplyr::bind_rows()

saveRDS(epi, "inst/testdata/change_cleanr_epi.rds", version = 2)


# Koch6 data
k6 <- epi

new_names <- c(
  "RegistrationNb",
  "changedate",
  "reason",
  "SEdescrip", 
  "Datepi",
  "CBdq",
  "CDld",
  "CMfx",
  "CCfz",
  "CLfx",
  "CImpCln",
  "CLzd",
  "CH",
  "CR",
  "CE",
  "CZ",
  "CS",
  "CKm",
  "CCm",
  "COfx",
  "CEto",
  "CCs",
  "CPAS",
  "CAmx-Clv",
  "CPto"
)

# apply new names
names(k6) <- new_names

  # add 4 and 0
# k6$CBdq[1] <- 4L
# k6$CLzd[10] <- 4L
# k6$CLfx[5] <- 0L

# save koch6 change test data
saveRDS(k6, "inst/testdata/change_cleanr_koch6.rds", version = 2)

