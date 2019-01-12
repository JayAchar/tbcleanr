# EpiInfo test data
x <- structure(list(ALCO = c(1L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 2L, 1L, 0L, 0L, 0L, 0L, 0L), 
                    INJECT = c("N", "Y", "N", "Y", "Y","Y", "Y", 
                                    "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
                    TEST = c(rep(T, 10))), 
                    row.names = c(NA, -16L), 
                    class = c("tbl_df", "tbl", "data.frame", "epiinfo"))

saveRDS(x, "inst/testdata/yn_binary.rds")
