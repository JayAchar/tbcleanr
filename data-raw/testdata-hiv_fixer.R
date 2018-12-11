# Generate test data for hiv_fixer()

## Koch 6 data
hiv_results <- expand.grid(HIV = c(0:4, NA_integer_),
                           cdhivenrol = c(0:5, NA_integer_))
k6_raw <- data.frame(registrationnb = paste0("XYZ", 1:42), stringsAsFactors = F)
k6_raw <- cbind(k6_raw, hiv_results)
class(k6_raw) <- c(class(k6_raw), "koch6")
