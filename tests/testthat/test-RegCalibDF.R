test_that("RegCalibDF returns the expected output components", {
  # Load the example datasets included in the package.
  data("main_data_sim", package = "RegCalib")
  data("valid_data_sim", package = "RegCalib")
  
  # Use a smaller stratified sample to keep the test fast.
  set.seed(123)
  
  rows_by_outcome <- split(
    seq_len(nrow(main_data_sim)),
    main_data_sim$case
  )
  
  test_rows <- unlist(
    lapply(
      rows_by_outcome,
      function(rows) sample(rows, min(length(rows), 500L))
    ),
    use.names = FALSE
  )
  
  main_test <- main_data_sim[test_rows, , drop = FALSE]
  
  result <- RegCalibDF(
    supplyEstimates = FALSE,
    ms = main_test,
    vs = valid_data_sim,
    sur = c("fqtfatinc", "fqcalinc", "fqalcinc"),
    exp = c("drtfatinc", "drcalinc", "dralcinc"),
    covCalib = "agec",
    covOutcomePlus = NULL,
    outcome = "case",
    method = "glm",
    family = binomial,
    link = "logit",
    external = TRUE,
    pointEstimates = NA,
    vcovEstimates = NA
  )
  
  expect_type(result, "list")
  
  expect_true(
    all(
      c(
        "correctedCoefTable",
        "correctedVCOV",
        "standardCoefTable"
      ) %in% names(result)
    )
  )
  
  expect_true(is.matrix(result$correctedVCOV))
  expect_true(all(is.finite(result$correctedCoefTable[, 1])))
})