## code to prepare `testdata-outcome_fixer` dataset goes here

# Epiinfo
  
  epi <- data.frame(APID = paste0("XYZ", 1:9), 
                    RES = 0:8, 
                    BIRTDATE = NA_integer_,
                    BDQDBDQ = NA_integer_, 
                    DATEDEAT = NA_character_,
                    stringsAsFactors = FALSE)
  
saveRDS(epi, "inst/testdata/outcome_epi.rds")



# Koch 6

## create k6 test data and adjust tests to match epiinfo
grid <- expand.grid(outfirst = 0:8,
                    outfirst2013 = 0:8,
                    '2013outcome' = c("est 1 outcome 2013",
                                      NA_character_))

df <- data.frame(registrationnb = paste0("XYZ", 1:nrow(grid)),
                 dateofbirth = NA_integer_,
                 Bdq = NA_integer_,
                 dateout = NA_integer_,
                 stringsAsFactors = FALSE)

k6 <- cbind(df, grid)

saveRDS(k6, "inst/testdata/outcome_k6.rds")

