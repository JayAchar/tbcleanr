# Epiinfo data
x <- structure(
  list(HAINH = c(1, 2, 5), HAINR = c(1, 2, 5)),
  class = c("epiinfo", "data.frame"),
  row.names = c(NA,-3L)
)

saveRDS(x, file = "inst/testdata/mtbdr_epiinfo.rds", version = 2)


# Grozny data
gg <- structure(
  list(
    cthres = structure(
      c(2L, 1L, NA, NA),
      .Label = c("Neg",
                 "Pos"),
      class = "factor"
    ),
    cthrifres = structure(
      c(3L, 2L, 1L,
        NA),
      .Label = c("M", "R", "S"),
      class = "factor"
    ),
    cthinhres = structure(
      c(3L,
        2L, 1L, NA),
      .Label = c("M", "R", "S"),
      class = "factor"
    )
  ),
  class = c("grozny", "data.frame"),
  row.names = c(NA,-4L)
)

saveRDS(gg, file = "inst/testdata/mtbdr_grozny.rds", version = 2)
