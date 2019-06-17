# testdata for adhere_classr()
# EpiInfo

data <- data.frame(APID = paste0("XYZ", 1:10),
                   PHASE = rep(c("IN", "OUT"), 5), 
                   PH = sample(400, 10, replace = TRUE),
                   NDCLO = sample(31, 10, replace = TRUE),
                   stringsAsFactors = FALSE)

saveRDS(data, "inst/testdata/adhere_classr_epiinfo.rds")
