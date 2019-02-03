## Test data for change_cleanr()

# EpiInfo data
set.seed(1000)
epiinfo <- data.frame(APID = paste0("XYZ", 1:10),
                      DACHAN = c(rep("1/1/2017", 9), NA_character_),
                      CCLO = sample(c(1:3, NA_integer_), size = 10, replace = T),
                      CXC = sample(c(1:3, NA_integer_), size = 10, replace = T),
                      CXE = sample(c(1:3, NA_integer_), size = 10, replace = T),
                      CXF = sample(c(1:3, NA_integer_), size = 10, replace = T),
                      CXG = sample(c(1:3, NA_integer_), size = 10, replace = T),
                      CXH = sample(c(1:3, NA_integer_), size = 10, replace = T),
                      CXI = sample(c(1:3, NA_integer_), size = 10, replace = T),
                      stringsAsFactors = FALSE)
# add epiinfo class
class(epiinfo) <- c(class(epiinfo), "epiinfo")
# save epiinfo change test data
saveRDS(epiinfo, "inst/testdata/change_cleanr_epiinfo.rds")


# Koch6 data
k6 <- epiinfo

new_names <- c("RegistrationNb", "changedate", "CCfz", "CMfx", "CLfx", "CLzd", "CBdq",
               "CImpCln", "CDld")

# apply new names
names(k6) <- new_names

  # add 4 and 0
  k6$CBdq[1] <- 4L
  k6$CLzd[10] <- 4L
  k6$CLfx[5] <- 0L
  
# save koch6 class  
class(k6) <- c("data.frame", "koch6")
# save koch6 change test data
saveRDS(k6, "inst/testdata/change_cleanr_koch6.rds")

