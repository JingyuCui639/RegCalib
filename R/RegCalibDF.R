#' Regression Calibration Using the Delta-Method (RSW Approach)
#'
#' Corrects for measurement error in continuous exposures (and covariates) and
#' returns corrected coefficients, standard errors, p-values, and
#' variance-covariance matrices. Users may supply their own uncorrected
#' estimates (e.g., from Cox or logistic models) instead of using the built-in
#' outcome model. Both external and internal validation study designs are
#' supported. Based on Rosner, Spiegelman & Willett (1989, 1990) and
#' Spiegelman, McDermott & Rosner (1997).
#'
#' @param supplyEstimates Logical. If \code{TRUE}, uncorrected estimates are
#'   supplied by the user via \code{pointEstimates} and \code{vcovEstimates},
#'   and \code{ms} is optional. Standard regression results are not returned
#'   in this case. Default \code{FALSE}.
#' @param ms Main study data frame. Must contain all variables specified in
#'   \code{sur}, \code{covCalib}, and \code{covOutcome}. Required when
#'   \code{supplyEstimates = FALSE}.
#' @param vs Internal or external validation study data frame. Must contain
#'   all variables specified in \code{exp}, \code{sur}, and \code{covCalib}.
#' @param sur Character vector of mismeasured exposure(s)/covariate(s)
#'   (surrogates) in the main study dataset.
#' @param exp Character vector of correctly-measured exposure(s)/covariate(s)
#'   in the validation dataset. Must correspond one-to-one with \code{sur} and
#'   have the same length.
#' @param covCalib Character vector of correctly-measured covariates to adjust
#'   for in both the calibration model and the outcome model. Default
#'   \code{NULL}.
#' @param covOutcome Character vector of correctly-measured risk factors for
#'   the outcome that are \emph{not} associated with the exposure or surrogate.
#'   These are included in the outcome model only and must not overlap with
#'   \code{covCalib}. Default \code{NULL}.
#' @param outcome Character. Name of the outcome variable. Required when
#'   \code{method} is \code{"lm"} or \code{"glm"}.
#' @param event Character. Name of the event status indicator for Cox models
#'   (0 = censored, 1 = event). Required when \code{method = "cox"}.
#' @param time Character. Name of the follow-up time variable for Cox models.
#'   Required when \code{method = "cox"}.
#' @param method Character. Outcome modelling method: \code{"lm"},
#'   \code{"glm"}, or \code{"cox"}. Default \code{"lm"}.
#' @param family Family function for \code{glm} (e.g., \code{binomial}). Not
#'   a character string. Required when \code{method = "glm"}.
#' @param link Character. Link function for \code{glm} (e.g.,
#'   \code{"logit"}). Required when \code{method = "glm"}.
#' @param external Logical. \code{TRUE} (default) for an external validation
#'   study; \code{FALSE} for an internal validation study. When
#'   \code{external = FALSE}, \code{vs} must contain the outcome variable and
#'   \code{supplyEstimates} must be \code{FALSE}.
#' @param pointEstimates Named numeric vector of uncorrected point estimates
#'   from standard regression (intercept excluded). Names must match the
#'   (expanded) covariate names from \code{covCalib} followed by
#'   \code{covOutcome}. Required when \code{supplyEstimates = TRUE}.
#' @param vcovEstimates Named square matrix of uncorrected variance-covariance
#'   estimates (intercept excluded). Column names must match those of
#'   \code{pointEstimates}. Required when \code{supplyEstimates = TRUE}.
#'
#' @return A named list containing:
#' \describe{
#'   \item{correctedCoefTable}{Data frame of corrected estimates, standard
#'     errors, Z-values, p-values, and 95\% confidence intervals.}
#'   \item{correctedVCOV}{Variance-covariance matrix of the corrected
#'     estimates.}
#'   \item{standardCoefTable}{(When \code{supplyEstimates = FALSE}) Results
#'     from the uncorrected standard regression.}
#'   \item{standardVCOV}{(When \code{supplyEstimates = FALSE}) Variance-
#'     covariance matrix from the uncorrected standard regression.}
#'   \item{calibrationModelCoefTable}{Calibration model slope estimates.}
#'   \item{calibrationModelVCOV}{Residual variance-covariance matrix from
#'     the calibration model.}
#' }
#'
#' @references
#' Rosner B, Willett WC, Spiegelman D (1989). Correction of logistic relative
#' risk estimates and confidence intervals for systematic within-person
#' measurement error. \emph{Statistics in Medicine} 8:1051--1069.
#'
#' Rosner B, Spiegelman D, Willett WC (1990). Correction of logistic
#' regression relative risk estimates and confidence intervals for measurement
#' error: the case of multiple covariates measured with error.
#' \emph{American Journal of Epidemiology} 132:734--735.
#'
#' Spiegelman D, McDermott A, Rosner B (1997). The many uses of the
#' 'regression calibration' method for measurement error bias correction in
#' nutritional epidemiology. \emph{American Journal of Clinical Nutrition}
#' 65:1179S--1186S.
#'
#' Spiegelman D, Carroll RJ, Kipnis V (2001). Efficient regression calibration
#' for logistic regression in main study/internal validation study designs
#' with an imperfect reference instrument. \emph{Statistics in Medicine}
#' 20:139--160.
#'
#' @examples
#' \dontrun{
#' result <- RegCalibDF(
#'   ms         = main_data,
#'   vs         = validation_data,
#'   sur        = "Z",
#'   exp        = "X",
#'   covCalib   = c("V1", "V2"),
#'   outcome    = "Y",
#'   method     = "lm",
#'   external   = TRUE
#' )
#' result$correctedCoefTable
#' }
#'
#' @export
RegCalibDF <- function(supplyEstimates = FALSE, ms, vs,
                       sur, exp, covCalib = NULL, covOutcome = NULL,
                       outcome = NA, event, time,
                       method = "lm", family = NA, link = NA,
                       external = TRUE,
                       pointEstimates = NA, vcovEstimates = NA) {

  ###################
  # check arguments #
  ###################
  ## data related warnings
  if (missing(vs)) {
    stop("Input data vs not supplied.")
  } else if (!is.data.frame(vs)) {
    stop("Input data vs must be of data.frame class.")
  }

  if (supplyEstimates == FALSE) {
    if (missing(ms)) {
      stop("Input data ms not supplied.")
    } else if (!is.data.frame(ms)) {
      stop("Input data ms must be of data.frame class.")
    }
  }

  if (missing(sur) | missing(exp)) {
    stop("Missing exposure variable.")
  } else if (class(sur) != "character" | class(exp) != "character") {
    stop("mExp or exp is not supplied with character vector.")
  } else if (length(sur) != length(exp)) {
    stop("Length of correctly measured variables differs from length of mismeasured variables.")
  }

  if (missing(covCalib)) {
    warning("No covariates supplied.")
  } else if (length(covCalib) != 0 & class(covCalib) != "character" ||
             (length(covOutcome) != 0 & class(covOutcome) != "character")) {
    stop("covCalib or covOutcome is not supplied with character vector.")
  } else if (length(base::intersect(covCalib, covOutcome)) > 0) {
    stop("There should be no overlapping variables in `covCalib` and `covOutcome`.")
  }

  if (missing(outcome)) {
    stop("Outcome is missing.")
  } else if (class(outcome) != "character" | outcome == "" | outcome == " ") {
    stop("outcome is not supplied with appropriate character.")
  }

  ## 1. check if MS contains data indicated by sur, covCalib and covOutcome
  if (supplyEstimates == FALSE) {
    MSVars_spec <- (c(sur, covCalib, covOutcome))
    MSVars <- colnames(ms)
    inMSVars <- (MSVars_spec %in% MSVars)
    if (sum(inMSVars) != length(MSVars_spec)) {
      stop("Main study dataset does not contain all the necessary variables specified in one of the following parameter: id, sur, covCalib and covOutcome.")
    }
  }

  ## 2. check if EVS contains data indicated by sur, exp and covCalib
  EVSVars <- colnames(vs)
  EVSVars_spec <- (c(sur, exp, covCalib))
  inMSVars <- (EVSVars_spec %in% EVSVars)
  if (sum(inMSVars) != length(EVSVars_spec)) {
    stop("Validation study dataset does not contain all the necessary variables specified in one of the following parameter: id, sur, exp and covCalib.")
  }
  if (external == FALSE) {
    if (!(outcome %in% colnames(vs))) {
      stop("Outcome variable is not available in the supplied internal validation data.")
    }
  }

  if (supplyEstimates == FALSE) {
    ## check if variables in covOutcome are strongly associated with exposure or surrogates
    if (length(covOutcome) > 0 & class(covOutcome) == "character") {
      trackerPvalueGE005 <- 0
      for (i in 1:length(sur)) {
        surUnivariate <- sur[i]
        checkCovOutcomeFormula <- paste0(surUnivariate, "~", paste0(covOutcome, collapse = "+"))
        checkCovOutcomeModel <- stats::lm(data = ms, formula = stats::as.formula(checkCovOutcomeFormula))
        checkCovOutcomePValues <- summary(checkCovOutcomeModel)$coefficients[-1, 4]
        if (sum(checkCovOutcomePValues < 0.05) > 0) {
          trackerPvalueGE005 <- trackerPvalueGE005 + 1
        }
      }
      if (trackerPvalueGE005 > 0) {
        warning("At least one of the risk factors specified in covOutcome is strongly associated with exposure(s) or surrogate(s) and should be reconsidered to be specified in covCalib instead.")
      }
    }
  }

  if (supplyEstimates == TRUE) {
    ## check whether there are names for the point estimates and column names for the vcov estimates
    if (length(names(pointEstimates)) == 0 | length(colnames(vcovEstimates)) == 0) {
      stop("There must be names for user supplied point estimates and column names for variance covariance matrix!")
    }
    ## check if vcov estimates are a square matrix
    if (dim(vcovEstimates)[1] != dim(vcovEstimates)[2]) {
      stop("Covariance matrix supplied is not a symmetric square matrix. Please check!")
    }
  }

  ######################
  # Embedded functions #
  ######################
  design <- function(TMatrix, PVector) {
    zeroMatrix <- matrix(rep(0, (PVector^2)^2), nrow = PVector^2)
    for (i in 1:length(TMatrix)) {
      tPos <- TMatrix[i]
      zeroMatrix[i, tPos] <- 1
    }
    return(zeroMatrix)
  }

  ######################
  # Computation starts #
  ######################
  # step 0: create design matrix
  if (supplyEstimates == FALSE) {
    if (length(covOutcome) == 0 & length(covCalib) == 0) {
      outcomeFormula <- paste0("~", paste0(sur, collapse = "+"))
      allVars_ms <- c(outcome, sur)
    } else if (length(covOutcome) == 0) {
      outcomeFormula <- paste0("~", paste0(sur, collapse = "+"), "+", paste0(covCalib, collapse = "+"))
      allVars_ms <- c(outcome, sur, covCalib)
    } else {
      outcomeFormula <- paste0("~", paste0(sur, collapse = "+"), "+", paste0(covCalib, collapse = "+"), "+", paste0(covOutcome, collapse = "+"))
      allVars_ms <- c(outcome, sur, covCalib, covOutcome)
    }

    if (method == "cox") {
      if (length(covOutcome) == 0 & length(covCalib) == 0) {
        outcomeFormula <- paste0("~", paste0(sur, collapse = "+"))
        allVars_ms <- c(time, event, sur)
      } else if (length(covOutcome) == 0) {
        outcomeFormula <- paste0("~", paste0(sur, collapse = "+"), "+", paste0(covCalib, collapse = "+"))
        allVars_ms <- c(time, event, sur, covCalib)
      } else {
        outcomeFormula <- paste0("~", paste0(sur, collapse = "+"), "+", paste0(covCalib, collapse = "+"), "+", paste0(covOutcome, collapse = "+"))
        allVars_ms <- c(time, event, sur, covCalib, covOutcome)
      }
    }
  }

  ## identify complete cases
  if (external == TRUE) {
    allVars_vs <- c(exp, sur, covCalib)
  } else if (external == FALSE) {
    if (length(covOutcome) == 0 & length(covCalib) == 0) {
      allVars_vs <- c(exp, sur, outcome)
    } else if (length(covOutcome) == 0) {
      allVars_vs <- c(exp, sur, covCalib, outcome)
    } else {
      allVars_vs <- c(exp, sur, covCalib, covOutcome, outcome)
    }
  }

  if (length(covOutcome) == 0 & length(covCalib) == 0) {
    exposureFormulaX <- paste0("~", paste0(sur, collapse = "+"))
    exposureFormulaY <- paste0("~", paste0(exp, collapse = "+"))
  } else {
    exposureFormulaX <- paste0("~", paste0(sur, collapse = "+"), "+", paste0(covCalib, collapse = "+"))
    exposureFormulaY <- paste0("~", paste0(exp, collapse = "+"), "+", paste0(covCalib, collapse = "+"))
  }

  if (supplyEstimates == FALSE) {
    ms_complete <- ms %>% dplyr::select(dplyr::all_of(allVars_ms)) %>% stats::na.omit()
    X_MS <- stats::model.matrix(object = stats::as.formula(outcomeFormula), data = ms_complete)
    Y_MS <- ms_complete[, outcome]
    outcomeModelVarNames <- colnames(X_MS)
  } else if (supplyEstimates == TRUE) {
    outcomeModelVarNames <- c("(Intercept)", names(pointEstimates))
  }

  vs_complete <- vs %>% dplyr::select(dplyr::all_of(allVars_vs)) %>% stats::na.omit()

  X_VS <- stats::model.matrix(object = stats::as.formula(exposureFormulaX), data = vs_complete)
  Y_VS <- stats::model.matrix(object = stats::as.formula(exposureFormulaY), data = vs_complete)[, -1]
  exposureModelVarNames <- colnames(X_VS)

  riskFactorModelVarNames <- setdiff(outcomeModelVarNames, exposureModelVarNames)

  # step 1: outcome model
  if (supplyEstimates == FALSE) {
    if (method == "lm") {
      outModel <- stats::lm(formula = stats::as.formula(paste0(outcome, outcomeFormula)), data = ms_complete)
      outcomeParam <- stats::coef(outModel)
      outcomeParamVCOV <- stats::vcov(outModel)
      outcomeModelResults <- (list(outcomeParam, outcomeParamVCOV))
    } else if (method == "glm") {
      outModel <- stats::glm(
        formula = stats::as.formula(paste0(outcome, outcomeFormula)),
        data = ms_complete,
        family = do.call(family, list(link = link))
      )
      outcomeParam <- stats::coef(outModel)
      outcomeParamVCOV <- stats::vcov(outModel)
      outcomeModelResults <- (list(outcomeParam, outcomeParamVCOV))
    } else if (method == "cox") {
      outModel <- survival::coxph(
        formula = stats::as.formula(paste0("survival::Surv(", time, ",", event, ")", outcomeFormula)),
        data = ms_complete
      )
      outcomeParam <- stats::coef(outModel)
      outcomeParamVCOV <- stats::vcov(outModel)
      outcomeModelResults <- (list(outcomeParam, outcomeParamVCOV))
    }

    if (length(outcomeModelVarNames) != (length(c(exposureModelVarNames, covOutcome)))) {
      stop("At least one categorical variable in main data set does not have the same length of values as in the validation data set. This violates the positivity required for the transportability of validation model. Consider data restriction or using continuous variable for extrapolation.")
    }

    if (method == "cox") {
      Bstar <- t(t(outcomeParam))
      VBstar <- outcomeParamVCOV[1:length(outcomeParam), 1:length(outcomeParam)]
      BstarSebstarP <- summary(outModel)$coef[, ]
    } else {
      Bstar <- t(t(outcomeParam[2:length(outcomeParam)]))
      VBstar <- outcomeParamVCOV[2:length(outcomeParam), 2:length(outcomeParam)]
      BstarSebstarP <- summary(outModel)$coef[-1, ]
    }

  } else if (supplyEstimates == TRUE) {
    Bstar <- as.matrix(pointEstimates)
    VBstar <- vcovEstimates
  }

  # step 2: calibration model
  X <- as.matrix(X_VS)
  Y <- as.matrix(Y_VS)

  lCovOutcome <- length(riskFactorModelVarNames)
  n <- nrow(X)
  pMeModel <- ncol(X) - 1
  p <- ncol(X) - 1 + lCovOutcome

  if (length(covOutcome) == 0) {
    F <- solve(t(X) %*% X)
    GWI <- F %*% t(X) %*% Y
    if (length(covCalib) == 0) {
      GEV <- t(GWI[sur, ])
    } else {
      GEV <- t(GWI[, exp])
    }
    ERR <- (Y - X %*% GWI)
  } else if (length(covOutcome) > 0) {
    I <- diag(lCovOutcome)
    rightLowerZeroMatrix <- matrix(0, nrow = lCovOutcome, ncol = lCovOutcome)
    F <- solve(t(X) %*% X)
    GWI <- as.matrix(Matrix::bdiag((F %*% t(X) %*% Y), I))
    F <- as.matrix(Matrix::bdiag(F, rightLowerZeroMatrix))
    X <- cbind(X, matrix(1, nrow = nrow(X), ncol = lCovOutcome))
    Y <- cbind(Y, matrix(1, nrow = nrow(Y), ncol = lCovOutcome))
    ERR <- (Y - X %*% GWI)
    colnames(GWI) <- colnames(Y)
    if (length(covCalib) == 0) {
      GEV <- t(GWI[, sur])
    } else {
      GEV <- t(GWI[, exp])
    }
  }

  S <- (t(ERR) %*% ERR) / (n - pMeModel - 1)
  if (length(covCalib) == 0) {
    colnames(S) <- exp
    rownames(S) <- exp
    VEV <- S
  } else {
    VEV <- S[exp, exp]
  }
  G <- t(GWI[2:(p + 1), ])
  VG <- matrixcalc::direct.prod(x = S, y = F[2:(p + 1), 2:(p + 1)])
  IGT <- t(solve(t(G)))
  t_vec <- as.vector(matrix(1:p^2, nrow = p, byrow = TRUE))
  k <- design(t_vec, p)
  m_k <- k %*% (matrixcalc::direct.prod(IGT, t(IGT)))

  B <- t(IGT) %*% Bstar
  VB <- matrixcalc::direct.prod(diag(p), t(Bstar)) %*% m_k %*% VG %*% t(m_k) %*%
    t(matrixcalc::direct.prod(diag(p), t(Bstar))) + t(IGT) %*% VBstar %*% IGT

  ##################################################
  # Additional steps for internal validation study #
  ##################################################
  if (external == FALSE) {
    if (length(covCalib) == 0 & length(covOutcome) == 0) {
      expFormula <- paste0("~", paste0(exp, collapse = "+"))
    } else if (length(covOutcome) == 0) {
      expFormula <- paste0("~", paste0(exp, collapse = "+"), "+", paste0(covCalib, collapse = "+"))
    } else {
      expFormula <- paste0("~", paste0(exp, collapse = "+"), "+", paste0(covCalib, collapse = "+"), "+", paste0(covOutcome, collapse = "+"))
    }

    if (method == "lm") {
      expModel_internal <- stats::lm(data = vs_complete, formula = stats::as.formula(paste0(outcome, expFormula)))
    } else {
      expModel_internal <- stats::glm(
        data = vs_complete,
        formula = stats::as.formula(paste0(outcome, expFormula)),
        family = do.call(family, list(link = link))
      )
    }
    BV <- expModel_internal$coef[-1]
    VBV <- stats::vcov(expModel_internal)[-1, -1]
    SEBV <- sqrt(diag(VBV))

    EST1 <- B
    VEST1 <- VB

    VE <- as.matrix(Matrix::bdiag(VEST1, VBV))
    XC <- rbind(diag(p), diag(p))
    M <- solve(t(XC) %*% solve(VE) %*% XC) %*% t(XC) %*% solve(VE)
    BRCI <- M %*% c(EST1, BV)
    VBRCI <- M %*% VE %*% t(M)

    B <- BRCI
    VB <- VBRCI
  }

  remove(list = (c("k", "m_k", "GWI", "VG", "G")))

  correctedVarNames <- c(exp, setdiff(outcomeModelVarNames[-1], sur))
  SE_B <- sqrt(diag(VB))
  zValue <- B / SE_B
  pValue <- 2 * stats::pnorm(-abs(as.numeric(zValue)))
  lcl <- B - stats::qnorm(0.975) * SE_B
  ucl <- B + stats::qnorm(0.975) * SE_B
  BSebP <- cbind(B, SE_B, zValue, pValue, lcl, ucl)

  colnames(BSebP) <- c("Estimate", "Std. Error", "Z Value", "Pr(>|Z|)", "lower 95%CI", "upper 95%CI")
  rownames(BSebP) <- names(SE_B)

  if (supplyEstimates == FALSE) {
    outputList <- list(BSebP, VB, BstarSebstarP, VBstar, GEV, S)
    names(outputList) <- c("correctedCoefTable", "correctedVCOV", "standardCoefTable", "standardVCOV", "calibrationModelCoefTable", "calibrationModelVCOV")
  } else if (supplyEstimates == TRUE) {
    outputList <- list(BSebP, VB, GEV, S)
    names(outputList) <- c("correctedCoefTable", "correctedVCOV", "calibrationModelCoefTable", "calibrationModelVCOV")
  }

  return(outputList)
}
