# RegCalib

**RegCalib** is an R package for correcting measurement error in continuous
exposures and covariates using regression calibration. It provides corrected
coefficients, standard errors, p-values, confidence intervals, and
variance-covariance matrices for linear and generalized linear outcome models
under external validation study design.

---

## Methods

| Function | Method | Supported outcome models | Reference |
|---|---|---|---|
| `RegCalibDF` | Deattenuation factor method | Linear models (`"lm"`) and generalized linear models (`"glm"`) | Rosner, Spiegelman, and Willett (1989, 1990); Spiegelman et al. (1997, 2001) |
| `RegCalibSub` | Substitution method | Linear models (`"lm"`) and generalized linear models (`"glm"`) | Carroll et al. (2006) |

Both methods support single or multiple error-prone exposures.

---

## Installation

```r
# Install pak once if needed.
install.packages("pak")

# Install RegCalib from GitHub.
pak::pkg_install("JingyuCui639/RegCalib")
```

---

## Example: External Validation Study with a Binary Outcome

This example uses the simulated main-study and external-validation datasets
included in the package. The outcome variable, `case`, is binary, so both
methods are applied using logistic regression.

### Load the package and example data

```r
# Load the RegCalib package.
library(RegCalib)

# Load the simulated main-study dataset included in RegCalib.
data("main_data_sim", package = "RegCalib")

# Display the first six observations in the main-study dataset.
head(main_data_sim)

# Load the simulated external-validation dataset included in RegCalib.
data("valid_data_sim", package = "RegCalib")

# Display the first six observations in the validation dataset.
head(valid_data_sim)
```

The three surrogate variables measured with error are:

- `fqtfatinc`: surrogate measure of total fat intake;
- `fqcalinc`: surrogate measure of total caloric intake;
- `fqalcinc`: surrogate measure of alcohol intake.

Their corresponding reference measurements in the validation dataset are:

- `drtfatinc`;
- `drcalinc`;
- `dralcinc`.

The variable `agec` is an error-free covariate, and `case` is the binary
outcome.

### Deattenuation Factor Method

```r
# Apply the deattenuation factor regression-calibration method.
rcdf <- RegCalibDF(
  # Let RegCalib fit the uncorrected outcome model internally.
  supplyEstimates = FALSE,

  # Supply the main-study data frame.
  ms = main_data_sim,

  # Supply the external-validation data frame.
  vs = valid_data_sim,

  # Identify the surrogate, error-prone variables.
  sur = c("fqtfatinc", "fqcalinc", "fqalcinc"),

  # Identify the corresponding reference variables in the same order.
  exp = c("drtfatinc", "drcalinc", "dralcinc"),

  # Include age category in both the calibration and outcome models.
  covCalib = "agec",

  # Do not include additional error-free covariates only in the outcome model.
  covOutcomePlus = NULL,

  # Identify the binary outcome variable.
  outcome = "case",

  # Fit a generalized linear outcome model.
  method = "glm",

  # Use the binomial family for a binary outcome.
  family = binomial,

  # Use the logit link to fit logistic regression.
  link = "logit",

  # Indicate that the validation study is external.
  external = TRUE,

  # Do not supply separate uncorrected coefficient estimates.
  pointEstimates = NA,

  # Do not supply a separate variance-covariance matrix.
  vcovEstimates = NA
)

# Print the complete result object.
rcdf

# Display the corrected coefficient table.
rcdf$correctedCoefTable

# Display the corrected variance-covariance matrix.
rcdf$correctedVCOV

# Display the uncorrected outcome-model coefficient table for comparison.
rcdf$standardCoefTable
```

Because the outcome model is logistic regression, the corrected coefficients
and confidence limits can be exponentiated to obtain odds ratios:

```r
# Exponentiate the corrected log-odds estimates and confidence limits.
corrected_RC_DF <- exp(
  cbind(
    # Exponentiate the corrected coefficient estimates.
    OR = rcdf$correctedCoefTable[, 1],

    # Exponentiate the lower limits of the 95% confidence intervals.
    "2.5 %" = rcdf$correctedCoefTable[, 5],

    # Exponentiate the upper limits of the 95% confidence intervals.
    "97.5 %" = rcdf$correctedCoefTable[, 6]
  )
)

# Display the corrected odds ratios and 95% confidence intervals.
corrected_RC_DF
```

### Substitution Method

```r
# Apply the substitution regression-calibration method.
rcsub <- RegCalibSub(
  # Supply the main-study data frame.
  ms = main_data_sim,

  # Supply the external-validation data frame.
  vs = valid_data_sim,

  # Identify the surrogate, error-prone variables.
  sur = c("fqtfatinc", "fqcalinc", "fqalcinc"),

  # Identify the corresponding reference variables in the same order.
  exp = c("drtfatinc", "drcalinc", "dralcinc"),

  # Include age category in the calibration models.
  covCalib = "agec",

  # Include age category in the outcome model.
  covOutcome = "agec",

  # Identify the binary outcome variable.
  outcome = "case",

  # Fit a generalized linear outcome model.
  method = "glm",

  # Use the binomial family for a binary outcome.
  family = binomial,

  # Use the logit link to fit logistic regression.
  link = "logit",

  # Indicate that the validation study is external.
  external = TRUE
)

# Print the complete result object.
rcsub

# Display the corrected coefficient table.
rcsub$correctedCoefTable

# Display the corrected variance-covariance matrix.
rcsub$correctedVCOV
```

The corrected coefficients and confidence limits can again be exponentiated
to obtain odds ratios:

```r
# Exponentiate the corrected log-odds estimates and confidence limits.
corrected_RC_SUB <- exp(
  cbind(
    # Exponentiate the corrected coefficient estimates.
    OR = rcsub$correctedCoefTable[, 1],

    # Exponentiate the lower limits of the 95% confidence intervals.
    "2.5 %" = rcsub$correctedCoefTable[, 5],

    # Exponentiate the upper limits of the 95% confidence intervals.
    "97.5 %" = rcsub$correctedCoefTable[, 6]
  )
)

# Display the corrected odds ratios and 95% confidence intervals.
corrected_RC_SUB
```

---

## Supported Outcome Models

For a continuous outcome, set:

```r
method = "lm"
```

For a generalized linear outcome model, set:

```r
method = "glm"
```

and supply the appropriate `family` and `link`. For example, logistic
regression uses:

```r
family = binomial
link = "logit"
```

---

## Dependencies

The package imports the following R packages:

- `stats`
- `dplyr`
- `Matrix`
- `matrixcalc`


These dependencies are installed automatically when RegCalib is installed.

---

## References

Rosner B, Willett WC, Spiegelman D (1989). Correction of logistic relative
risk estimates and confidence intervals for systematic within-person
measurement error. *Statistics in Medicine* **8**, 1051–1069.

Rosner B, Spiegelman D, Willett WC (1990). Correction of logistic regression
relative risk estimates and confidence intervals for measurement error: the
case of multiple covariates measured with error. *American Journal of
Epidemiology* **132**, 734–745.

Spiegelman D, McDermott A, Rosner B (1997). The many uses of the regression
calibration method for measurement error bias correction in nutritional
epidemiology. *American Journal of Clinical Nutrition* **65**, 1179S–1186S.

Spiegelman D, Carroll RJ, Kipnis V (2001). Efficient regression calibration
for logistic regression in main study/internal validation study designs with
an imperfect reference instrument. *Statistics in Medicine* **20**, 139–160.

Carroll RJ, Ruppert D, Stefanski LA, Crainiceanu CM (2006).
*Measurement Error in Nonlinear Models: A Modern Perspective*, 2nd ed.
Chapman & Hall/CRC.

---

## Authors and Affiliations

- **Jingyu Cui** — Department of Biostatistics, Yale School of Public Health
- **Wenze Tang** — Vertex Pharmaceuticals
- **Molin Wang** — Department of Biostatistics, Department of Epidemiology, Harvard T.H. Chan School of Public Health

**Maintainer:** Jingyu Cui  
**Email:** jingyu.cui@yale.edu
