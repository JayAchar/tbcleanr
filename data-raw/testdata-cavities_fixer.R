# test data for cavities_fixer()

# Epiinfo

epi <- data.frame(expand.grid(XRAYRES = 1:4,
                              ABNORM = 1:2),
                  stringsAsFactors = FALSE)

class(epi) <- c(class(epi), "epiinfo")

saveRDS(epi, "inst/testdata/cavities_fixer_epiinfo.rds")


# Koch 6

k6 <- data.frame(expand.grid(XRay_Finding = 0:2,
                             cav = 0:2,
                             cavD = 0:2,
                             cavsimple = 0:2),
                 stringsAsFactors = FALSE)

class(k6) <- c(class(k6), "koch6")

saveRDS(k6, "inst/testdata/cavities_fixer_koch6.rds")
