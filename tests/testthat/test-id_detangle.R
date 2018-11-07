context("id_detangle")
library(tbcleanr)

# Load EpiInfo data
epi_raw <- system.file("testdata", "id_detangle_epi.rds", package = "tbcleanr") %>% 
    readRDS()

epi_output <- id_detangle(epi_raw)

test_that("correct epiinfo works", {
    expect_true("epiinfo" %in% class(epi_output))
    expect_equal(class(epi_raw), class(epi_output))
    expect_equal(nrow(epi_raw), nrow(epi_output))
    expect_equal(ncol(epi_raw) + 2, ncol(epi_output))
    expect_true(c("APID", "ds_dr", "district") %in% names(epi_output) %>% all())
    expect_equal(sum(epi_output$ds_dr == "DR-TB"), 8)
    expect_equal(sum(epi_output$district == "CHM"), 4)
    
})


# EpiInfo data - introduce NA
epi_NA <- epi_raw
epi_NA[20, ] <- NA_character_

epi_NA_output <- id_detangle(epi_NA)

test_that("epiinfo with NA works", {
    expect_true("epiinfo" %in% class(epi_NA_output))
    expect_equal(class(epi_NA), class(epi_NA_output))
    expect_equal(nrow(epi_NA), nrow(epi_NA_output))
    expect_equal(ncol(epi_NA) + 2, ncol(epi_NA_output))
    expect_true(c("APID", "ds_dr", "district") %in% names(epi_NA_output) %>% all())
    expect_equal(sum(epi_NA_output$ds_dr == "DR-TB", na.rm = TRUE), 8)
    expect_equal(sum(epi_NA_output$district == "CHM", na.rm = TRUE), 4)
    expect_message(id_detangle(epi_NA), "There are 1 missing ID values in the dataset")
    
})



# Handle non-EpiInfo dataset
k6_raw <- system.file("testdata", "gender_k6.rds", package = "tbcleanr") %>% 
    readRDS()

k6_output <- id_detangle(k6_raw)

test_that("epiinfo with NA works", {
    expect_equal(class(k6_raw), class(k6_output))
    expect_equal(dim(k6_raw), dim(k6_output))
    expect_message(id_detangle(k6_raw), "No adm object class detected: id_detangle()")
    
})


