# Regression Calibration Using Substitution Method

## Usage

``` r
RegCalibSub(
  ms,
  vs,
  sur,
  exp,
  vsIndicator,
  covCalib = NULL,
  covOutcome = NULL,
  outcome = NA,
  method = "lm",
  family = NA,
  link = NA,
  external = TRUE
)
```

## Arguments

- ms:

  Main study data frame. Must contain all variables specified in `sur`,
  `outcome`, and `covOutcome` (if any).

- vs:

  External validation study data frame. Must contain all variables
  specified in `exp`, `sur`, and `covCalib` (if any). Required when
  `external = TRUE`.

- sur:

  Character vector of mismeasured exposure(s)/covariate(s) (surrogates)
  in the main study dataset.

- exp:

  Character vector of correctly-measured exposure(s)/covariate(s) in the
  validation dataset. Must correspond one-to-one with `sur` and have the
  same length.

- vsIndicator:

  Character. Name of the indicator variable in the main study dataset
  that identifies subjects with a validation record (1 = has validation
  record). Required when `external = FALSE`.

- covCalib:

  Character vector of correctly-measured covariates to adjust for in the
  calibration model, including any non-linear terms. Default `NULL`.

- covOutcome:

  Character vector of correctly-measured covariates to adjust for in the
  outcome model (with corrected exposure), including any non-linear
  terms. Default `NULL`.

- outcome:

  Character. Name of the outcome variable. Required.

- method:

  Character. Outcome modelling method: `"lm"` or `"glm"`. Default
  `"lm"`.

- family:

  Family function for `glm` (e.g., `binomial`). Not a character string.
  Required when `method = "glm"`.

- link:

  Character. Link function for `glm` (e.g., `"logit"` or `"log"`).
  Required when `method = "glm"`.

- external:

  Logical. `TRUE` (default) for an external validation study; `FALSE`
  for an internal validation study embedded in `ms`.

## Value

A named list containing:

- correctedCoefTable:

  Matrix of corrected estimates, standard errors, Z-values, p-values,
  and 95\\ correctedVCOVVariance-covariance matrix of the corrected
  estimates.

Corrects for measurement error in continuous exposures (and covariates)
and returns corrected coefficients, standard errors, p-values, and
variance-covariance matrices using the
Carroll-Ruppert-Stefanski-Crainiceanu (CRS) sandwich variance estimator.
Supports linear and generalized linear outcome models under external and
internal validation study designs. Standard errors are derived
analytically via the sandwich estimator (not bootstrap). Non-linear
terms such as interaction terms should be pre-computed as permanent
columns in the input datasets rather than specified in the formula.
Carroll RJ, Ruppert D, Stefanski LA, Crainiceanu CM (2006). *Measurement
Error in Nonlinear Models*, 2nd ed. Chapman & Hall/CRC.
