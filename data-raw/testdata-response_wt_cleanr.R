# test data for response_wt_cleanr()

# epiinfo
epi <- data.frame(APID = rep("XYZ0001", 11),
                        FOLAFT = c(1, 2, 2, 3:7, 7:9),
                        WEIGHT = c(30, 31, 32, 34, 36, 40, 41, 52, 47, 50, NA_integer_),
                        stringsAsFactors = F)

class(epi) <- c(class(epi), "epiinfo")
saveRDS(epi, "inst/testdata/response_wt_cleanr_epiinfo.rds")
