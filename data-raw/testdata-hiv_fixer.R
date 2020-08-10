# Generate test data for hiv_fixer()

## Koch 6 data
hiv_results <- expand.grid(HIV = c(0:4, NA_integer_),
                           cdhivenrol = c(0:5, NA_integer_))
k6_raw <- data.frame(registrationnb = paste0("XYZ", 1:42), stringsAsFactors = F)
k6_raw <- cbind(k6_raw, hiv_results)
class(k6_raw) <- c("koch6", class(k6_raw))

saveRDS(k6_raw, "inst/testdata/hiv_k6.rds", version = 2)


# Epiinfo
epi <- structure(
  list(
    APID = c("XYZ1", "XYZ2", "XYZ3", "XYZ4"),
    HIV = c(1L,
            2L, 3L, NA)
  ),
  row.names = c(NA,-4L),
  class = c("epiinfo", "tbl_df", "tbl",
            "data.frame")
)

saveRDS(epi, "inst/testdata/hiv_epi.rds", version = 2)
