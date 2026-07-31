test_that("RegCalibSub returns the expected output components", {
  data("main_data_sim", package = "RegCalib")
  data("valid_data_sim", package = "RegCalib")
  
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
  
  result <- RegCalibSub(
    ms = main_test,
    vs = valid_data_sim,
    sur = c("fqtfatinc", "fqcalinc", "fqalcinc"),
    exp = c("drtfatinc", "drcalinc", "dralcinc"),
    covCalib = "agec",
    covOutcome = "agec",
    outcome = "case",
    method = "glm",
    family = binomial,
    link = "logit",
    external = TRUE
  )
  
  expect_type(result, "list")
  
  expect_true(
    all(
      c(
        "correctedCoefTable",
        "correctedVCOV"
      ) %in% names(result)
    )
  )
  
  expect_true(is.matrix(result$correctedVCOV))
  expect_true(all(is.finite(result$correctedCoefTable[, 1])))
})