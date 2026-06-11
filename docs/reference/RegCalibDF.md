# Regression Calibration Using the Deattenuation Factor Method

## Usage

``` r
RegCalibDF(
  supplyEstimates = FALSE,
  ms,
  vs,
  sur,
  exp,
  covCalib = NULL,
  covOutcome = NULL,
  outcome = NA,
  event,
  time,
  method = "lm",
  family = NA,
  link = NA,
  external = TRUE,
  pointEstimates = NA,
  vcovEstimates = NA
)
```

## Arguments

- supplyEstimates:

  Logical. If `TRUE`, uncorrected estimates are supplied by the user via
  `pointEstimates` and `vcovEstimates`, and `ms` is optional. Standard
  regression results are not returned in this case. Default `FALSE`.

- ms:

  Main study data frame. Must contain all variables specified in `sur`,
  `covCalib`, and `covOutcome`. Required when `supplyEstimates = FALSE`.

- vs:

  Internal or external validation study data frame. Must contain all
  variables specified in `exp`, `sur`, and `covCalib`.

- sur:

  Character vector of mismeasured exposure(s)/covariate(s) (surrogates)
  in the main study dataset.

- exp:

  Character vector of correctly-measured exposure(s)/covariate(s) in the
  validation dataset. Must correspond one-to-one with `sur` and have the
  same length.

- covCalib:

  Character vector of correctly-measured covariates to adjust for in
  both the calibration model and the outcome model. Default `NULL`.

- covOutcome:

  Character vector of correctly-measured risk factors for the outcome
  that are *not* associated with the exposure or surrogate. These are
  included in the outcome model only and must not overlap with
  `covCalib`. Default `NULL`.

- outcome:

  Character. Name of the outcome variable. Required when `method` is
  `"lm"` or `"glm"`.

- event:

  Character. Name of the event status indicator for Cox models (0 =
  censored, 1 = event). Required when `method = "cox"`.

- time:

  Character. Name of the follow-up time variable for Cox models.
  Required when `method = "cox"`.

- method:

  Character. Outcome modelling method: `"lm"`, `"glm"`, or `"cox"`.
  Default `"lm"`.

- family:

  Family function for `glm` (e.g., `binomial`). Not a character string.
  Required when `method = "glm"`.

- link:

  Character. Link function for `glm` (e.g., `"logit"`). Required when
  `method = "glm"`.

- external:

  Logical. `TRUE` (default) for an external validation study; `FALSE`
  for an internal validation study. When `external = FALSE`, `vs` must
  contain the outcome variable and `supplyEstimates` must be `FALSE`.

- pointEstimates:

  Named numeric vector of uncorrected point estimates from standard
  regression (intercept excluded). Names must match the (expanded)
  covariate names from `covCalib` followed by `covOutcome`. Required
  when `supplyEstimates = TRUE`.

- vcovEstimates:

  Named square matrix of uncorrected variance-covariance estimates
  (intercept excluded). Column names must match those of
  `pointEstimates`. Required when `supplyEstimates = TRUE`.

## Value

A named list containing:

- correctedCoefTable:

  Data frame of corrected estimates, standard errors, Z-values,
  p-values, and 95\\ correctedVCOVVariance-covariance matrix of the
  corrected estimates. standardCoefTable(When `supplyEstimates = FALSE`)
  Results from the uncorrected standard regression. standardVCOV(When
  `supplyEstimates = FALSE`) Variance- covariance matrix from the
  uncorrected standard regression. calibrationModelCoefTableCalibration
  model slope estimates. calibrationModelVCOVResidual
  variance-covariance matrix from the calibration model.

Corrects for measurement error in continuous exposures (and covariates)
and returns corrected coefficients, standard errors, p-values, and
variance-covariance matrices. Users may supply their own uncorrected
estimates (e.g., from Cox or logistic models) instead of using the
built-in outcome model. Both external and internal validation study
designs are supported. Based on Rosner, Spiegelman & Willett (1989,
1990) and Spiegelman, McDermott & Rosner (1997). Rosner B, Willett WC,
Spiegelman D (1989). Correction of logistic relative risk estimates and
confidence intervals for systematic within-person measurement error.
*Statistics in Medicine* 8:1051–1069.Rosner B, Spiegelman D, Willett WC
(1990). Correction of logistic regression relative risk estimates and
confidence intervals for measurement error: the case of multiple
covariates measured with error. *American Journal of Epidemiology*
132:734–735.Spiegelman D, McDermott A, Rosner B (1997). The many uses of
the 'regression calibration' method for measurement error bias
correction in nutritional epidemiology. *American Journal of Clinical
Nutrition* 65:1179S–1186S.Spiegelman D, Carroll RJ, Kipnis V (2001).
Efficient regression calibration for logistic regression in main
study/internal validation study designs with an imperfect reference
instrument. *Statistics in Medicine* 20:139–160.
