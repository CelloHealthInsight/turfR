data('turf_ex_data')
data('turf_ex_output')
message_count <- dim(turf_ex_data)[2] - 2


testthat::test_that("Outputs of turf function are of correct format", {
  
  turf_analysis <- turfR::turf(turf_ex_data, 
                               n = message_count, 
                               k = 2)$turf[[1]]
  
  #Check for correct dimensions of output
  testthat::expect_equal(dim(turf_analysis)[2], message_count+3)
  #Check that columns are of correct class
  testthat::expect_true(all(sapply(turf_analysis, class) == c('factor', 'numeric', 'numeric', rep('numeric', message_count)))
)
  
  
})


testthat::test_that("Invalid combination argument produces expected error", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data, 
                                 n = message_count, 
                                 k = 0)$turf[[1]], 
    "min\\(k\\) must be greater than or equal to 1"
    
  )
 
})


testthat::test_that("Invalid message number argument produces expected error", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data, 
                                 n = 1, 
                                 k = 2)$turf[[1]], 
    "max\\(k\\) must be less than the total number of items n"
    
  )
  
})

testthat::test_that("Invalid message number argument produces expected error", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data[,1:4], 
                                 n = message_count, 
                                 k = 2)$turf[[1]], 
    "Data must have respondent id, weight and at least n additional columns"
    
  )
  
})

testthat::test_that("Invalid argument produces expected error", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data, 
                                 n = message_count, 
                                 k = 2,
                                 random_arg = 0)$turf[[1]], 
    "Argument random_arg not matched"
    
  )
  
})

testthat::test_that("Invalid depth argument produces expected error", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data, 
                                 n = message_count, 
                                 k = 2,
                                 depth = 0)$turf[[1]], 
    "depth must be a scalar greater than or equal to 1"
    
  )
  
})

testthat::test_that("Invalid keep argument produces expected error", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data, 
                                 n = message_count, 
                                 k = 2,
                                 keep = -1)$turf[[1]], 
    "keep must be a scalar greater than or equal to 0"
    
  )
  
})

testthat::test_that("Invalid mc argument produces expected error", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data, 
                                 n = message_count, 
                                 k = 2,
                                 mc = -1)$turf[[1]], 
    "mc must be TRUE/FALSE"
    
  )
  
})


testthat::test_that("Invalid sort argument produces expected error", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data, 
                                 n = message_count, 
                                 k = 2,
                                 sort = "x")$turf[[1]], 
    "sort must be %in% c\\('a', 'd', 'n'\\)"
    
  )
  
})



testthat::test_that("Test the combos argument type", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data, 
                                 n = message_count, 
                                 k = 2,
                                 combos = "x")$turf[[1]], 
    "combos must be a list structure"
    
  )
  
})

testthat::test_that("Test the combos argument characteristics", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data, 
                                 n = message_count, 
                                 k = 2,
                                 combos = list(1,2,3))$turf[[1]], 
    "combos must have k components"
    
  )
  
})


testthat::test_that("Check that the variable combos is valid size", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data, 
                                 n = message_count, 
                                 k = 2,
                                 combos = list(matrix(0, nrow = 5, ncol = 3)))$turf[[1]], 
    "combos\\[\\[i\\]\\] must have n columns"
    
  )
  
})

testthat::test_that("Check that the variable combos had valid values contained", {
  
  testthat::expect_error(
    turf_analysis <- turfR::turf(turf_ex_data, 
                                 n = message_count, 
                                 k = 2,
                                 combos = list(matrix(2, nrow = 23, ncol = 10)))$turf[[1]], 
    "combos\\[\\[i\\]\\] may contain only 0s and 1s"
    
  )
})

testthat::test_that("Data in output is identical to expected values", {
  
  turf_analysis <- turfR::turf(turf_ex_data, 
                               n = message_count, 
                               k = 2)$turf[[1]]
  testthat::expect_equal(turf_analysis, turf_ex_output)
})

