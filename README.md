# RegCalib

**RegCalib** is an R package for correcting measurement error in continuous exposures (and covariates) using regression calibration. It provides corrected coefficients, standard errors, p-values, and variance-covariance matrices for linear, generalized linear, and Cox proportional hazards outcome models under both external and internal validation study designs.

---

## Functions

| Function | Method | Reference |
|---|---|---|
| `RegCalibDF` | Deattenuation factor method | Rosner, Spiegelman & Willett (1989, 1990); Spiegelman et al. (1997, 2001) |
| `RegCalibSub` | Substitution method | Carroll, Ruppert, Stefanski & Crainiceanu (2006) |

Both functions support single and multiple mismeasured exposures.

---

## Installation

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install RegCalib from GitHub
devtools::install_github("YOUR_USERNAME/RegCalib")
```

---

## Usage

### RegCalibDF — Deattenuation Factor Method

Suitable for linear, GLM, and Cox outcome models. Optionally accepts user-supplied uncorrected estimates (e.g., from an external Cox model).

```r
library(RegCalib)

result <- RegCalibDF(
  ms       = main_data,       # main study data frame
  vs       = validation_data, # validation study data frame
  sur      = "Z",             # surrogate (mismeasured) variable in main study
  exp      = "X",             # true exposure variable in validation study
  covCalib = c("V1", "V2"),   # covariates in calibration and outcome model
  outcome  = "Y",             # outcome variable
  method   = "lm",            # "lm", "glm", or "cox"
  external = TRUE             # TRUE = external validation study
)

result$correctedCoefTable   # corrected estimates, SE, p-values, 95% CI
result$correctedVCOV        # corrected variance-covariance matrix
result$standardCoefTable    # uncorrected estimates for comparison
```

For a GLM outcome model:

```r
result <- RegCalibDF(
  ms       = main_data,
  vs       = validation_data,
  sur      = "Z",
  exp      = "X",
  covCalib = c("V1", "V2"),
  outcome  = "Y",
  method   = "glm",
  family   = binomial,
  link     = "logit",
  external = TRUE
)
```

### RegCalibSub — Substitution Method

Suitable for linear and GLM outcome models. Uses the Carroll-Ruppert-Stefanski-Crainiceanu substitution approach with analytical sandwich standard errors.

```r
result <- RegCalibSub(
  ms       = main_data,
  vs       = validation_data,
  sur      = "Z",
  exp      = "X",
  covCalib = c("V1", "V2"),
  outcome  = "Y",
  method   = "lm",
  external = TRUE
)

result$correctedCoefTable
result$correctedVCOV
```

For an internal validation study:

```r
result <- RegCalibSub(
  ms          = main_data,      # main study contains both main and validation subjects
  sur         = "Z",
  exp         = "X",
  vsIndicator = "is_validated", # indicator variable: 1 = has validation record
  covCalib    = c("V1", "V2"),
  outcome     = "Y",
  method      = "lm",
  external    = FALSE
)
```

---

## Dependencies

RegCalib imports: `stats`, `dplyr`, `Matrix`, `matrixcalc`, `earth`, `survival`. These are installed automatically.

---

## References

Rosner B, Willett WC, Spiegelman D (1989). Correction of logistic relative risk estimates and confidence intervals for systematic within-person measurement error. *Statistics in Medicine* 8:1051–1069.

Rosner B, Spiegelman D, Willett WC (1990). Correction of logistic regression relative risk estimates and confidence intervals for measurement error: the case of multiple covariates measured with error. *American Journal of Epidemiology* 132:734–735.

Spiegelman D, McDermott A, Rosner B (1997). The many uses of the 'regression calibration' method for measurement error bias correction in nutritional epidemiology. *American Journal of Clinical Nutrition* 65:1179S–1186S.

Spiegelman D, Carroll RJ, Kipnis V (2001). Efficient regression calibration for logistic regression in main study/internal validation study designs with an imperfect reference instrument. *Statistics in Medicine* 20:139–160.

Carroll RJ, Ruppert D, Stefanski LA, Crainiceanu CM (2006). *Measurement Error in Nonlinear Models*, 2nd ed. Chapman & Hall/CRC.

---

## Authors

Wenze Tang, Molin Wang, Jingyu Cui
