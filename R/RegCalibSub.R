#' Regression Calibration Using Substitution Method
#'
#' Corrects for measurement error in continuous exposures (and covariates) and
#' returns corrected coefficients, standard errors, p-values, and
#' variance-covariance matrices using the Carroll-Ruppert-Stefanski-Crainiceanu
#' (CRS) sandwich variance estimator. Supports linear and generalized linear
#' outcome models under external and internal validation study designs. Standard
#' errors are derived analytically via the sandwich estimator (not bootstrap).
#' Non-linear terms such as interaction terms should be pre-computed as
#' permanent columns in the input datasets rather than specified in the formula.
#'
#' @param ms Main study data frame. Must contain all variables specified in
#'   \code{sur}, \code{outcome}, and \code{covOutcome} (if any).
#' @param vs External validation study data frame. Must contain all variables
#'   specified in \code{exp}, \code{sur}, and \code{covCalib} (if any).
#'   Required when \code{external = TRUE}.
#' @param sur Character vector of mismeasured exposure(s)/covariate(s)
#'   (surrogates) in the main study dataset.
#' @param exp Character vector of correctly-measured exposure(s)/covariate(s)
#'   in the validation dataset. Must correspond one-to-one with \code{sur} and
#'   have the same length.
#' @param vsIndicator Character. Name of the indicator variable in the main
#'   study dataset that identifies subjects with a validation record
#'   (1 = has validation record). Required when \code{external = FALSE}.
#' @param covCalib Character vector of correctly-measured covariates to adjust
#'   for in the calibration model, including any non-linear terms. Default
#'   \code{NULL}.
#' @param covOutcome Character vector of correctly-measured covariates to
#'   adjust for in the outcome model (with corrected exposure), including any
#'   non-linear terms. Default \code{NULL}.
#' @param outcome Character. Name of the outcome variable. Required.
#' @param method Character. Outcome modelling method: \code{"lm"} or
#'   \code{"glm"}. Default \code{"lm"}.
#' @param family Family function for \code{glm} (e.g., \code{binomial}). Not
#'   a character string. Required when \code{method = "glm"}.
#' @param link Character. Link function for \code{glm} (e.g., \code{"logit"}
#'   or \code{"log"}). Required when \code{method = "glm"}.
#' @param external Logical. \code{TRUE} (default) for an external validation
#'   study; \code{FALSE} for an internal validation study embedded in \code{ms}.
#'
#' @return A named list containing:
#' \describe{
#'   \item{correctedCoefTable}{Matrix of corrected estimates, standard errors,
#'     Z-values, p-values, and 95\% confidence intervals.}
#'   \item{correctedVCOV}{Variance-covariance matrix of the corrected
#'     estimates.}
#' }
#'
#' @references
#' Carroll RJ, Ruppert D, Stefanski LA, Crainiceanu CM (2006).
#' \emph{Measurement Error in Nonlinear Models}, 2nd ed.
#' Chapman & Hall/CRC.
#'
#' @examples
#' \dontrun{
#' result <- RegCalibSub(
#'   ms       = main_data,
#'   vs       = validation_data,
#'   sur      = "Z",
#'   exp      = "X",
#'   covCalib = c("V1", "V2"),
#'   outcome  = "Y",
#'   method   = "lm",
#'   external = TRUE
#' )
#' result$correctedCoefTable
#' }
#'
#' @export
RegCalibSub <- function(ms, vs,
                        sur, exp, vsIndicator, covCalib = NULL, covOutcome = NULL,
                        outcome = NA,
                        method = "lm", family = NA, link = NA,
                        external = TRUE) {

  # show intermediate quantities controller
  show_detail <- FALSE

  ###################
  # check arguments #
  ###################
  if (external == TRUE) {
    if (missing(vs)) {
      stop("Input data vs not supplied.")
    } else if (!is.data.frame(vs)) {
      stop("Input data vs must be of data.frame class.")
    }
  }

  if (missing(ms)) {
    stop("Input data ms not supplied.")
  } else if (!is.data.frame(ms)) {
    stop("Input data ms must be of data.frame class.")
  }

  if (missing(sur) | missing(exp)) {
    stop("Missing exposure variable.")
  } else if (class(sur) != "character" | class(exp) != "character") {
    stop("mExp or exp is not supplied with character vector.")
  } else if (length(sur) != length(exp)) {
    stop("Length of correctly measured variables differs from length of mismeasured variables.")
  }

  if (is.null(covCalib)) {
    warning("No covariates supplied.")
  } else if (length(covCalib) != 0 & class(covCalib) != "character" |
             (length(covOutcome) != 0 & class(covOutcome) != "character")) {
    stop("covCalib or covOutcome is not supplied with character vector.")
  }

  if (is.na(outcome)) {
    stop("Outcome is missing.")
  } else if (class(outcome) != "character" | outcome == "" | outcome == " ") {
    stop("outcome is not supplied with appropriate character.")
  }
########### Jingyu Cui June 10 2026 #####
  if (!method %in% c("lm", "glm")) {
    stop("method must be either 'lm' or 'glm'.")
  }
  
  if (method == "glm") {
    if (missing(family) || is.null(family)) {
      stop("For method = 'glm', you must supply a family function, e.g., family = binomial.")
    }
    
    if (!is.function(family)) {
      stop("For method = 'glm', family must be a function, e.g., family = binomial, not family = 'binomial'.")
    }
    
    if (missing(link) || is.null(link) || length(link) != 1 ||
        !is.character(link) || is.na(link)) {
      stop("For method = 'glm', link must be a character string, e.g., link = 'logit'.")
    }
    
    if (!link %in% c("logit", "log")) {
      stop("Currently only link = 'logit' and link = 'log' are supported.")
    }
  }
######### Jingyu Cui June 10 2026 #############
  
  ## 1. check if MS contains data indicated by sur and covOutcome
  MSVars_spec <- (c(sur, covOutcome))
  MSVars <- colnames(ms)
  inMSVars <- (MSVars_spec %in% MSVars)
  if (sum(inMSVars) != length(MSVars_spec)) {
    stop("Main study dataset does not contain all the necessary variables specified in one of the following parameter: id, sur, covCalib and covOutcome.")
  }

  ## 2. check if EVS contains data indicated by sur, exp and covCalib
  if (external == TRUE) {
    EVSVars <- colnames(vs)
    EVSVars_spec <- (c(sur, exp, covCalib))
    inMSVars <- (EVSVars_spec %in% EVSVars)
    if (sum(inMSVars) != length(EVSVars_spec)) {
      stop("Validation study dataset does not contain all the necessary variables specified in one of the following parameter: id, sur, exp and covCalib.")
    }
  }

  ######################
  # Computation starts #
  ######################
  #####################
  # 1. Point estimate #
  #####################
  commonCovariates <- intersect(covCalib, covOutcome)
  covCalibOnly <- setdiff(covCalib, commonCovariates)
  covOutcomeOnly <- setdiff(covOutcome, commonCovariates)
  covAll <- c(commonCovariates, covCalibOnly, covOutcomeOnly)

  if (external == TRUE) {
    allVars_vs <- c(exp, sur, covCalib)
    vs_complete <- vs %>% dplyr::select(dplyr::all_of(allVars_vs)) %>% stats::na.omit()
    allVars_ms <- c(sur, covAll, outcome)
    ms_complete <- ms %>% dplyr::select(dplyr::all_of(allVars_ms)) %>% stats::na.omit()
  } else if (external == FALSE) {
    allVars_ms <- c(sur, exp, covAll, outcome, vsIndicator)
    allVars_ms_miss <- c(sur, covAll, outcome, vsIndicator)
    ms_complete <- ms[complete.cases(ms[, allVars_ms_miss]), ]
    ms_complete <- ms_complete %>% dplyr::select(dplyr::all_of(allVars_ms))
    allVars_vs <- c(exp, sur, covCalib, outcome)
    vs_complete <- ms_complete %>% dplyr::select(dplyr::all_of(allVars_vs)) %>% stats::na.omit()
  }

  ## create design matrix of calibration model
  if (length(covCalib) == 0) {
    exposureFormulaX <- paste0("~", paste0(sur, collapse = "+"))
    exposureFormulaY <- paste0("~", paste0(exp, collapse = "+"))
  } else {
    exposureFormulaX <- paste0("~", paste0(sur, collapse = "+"), "+", paste0(covCalib, collapse = "+"))
    exposureFormulaY <- paste0("~", paste0(exp, collapse = "+"), "+", paste0(covCalib, collapse = "+"))
  }

  X_VS <- stats::model.matrix(object = stats::as.formula(exposureFormulaX), data = vs_complete)
  X_MS <- stats::model.matrix(object = stats::as.formula(exposureFormulaX), data = ms_complete)
  Y_VS <- stats::model.matrix(object = stats::as.formula(exposureFormulaY), data = vs_complete)[, -1]
  exposureModelVarNames <- colnames(X_VS)

  X <- as.matrix(X_VS)
  X.MS <- as.matrix(X_MS)
  Y <- as.matrix(Y_VS)

  n <- nrow(X)
  pMeModel <- ncol(X) - 1
  p <- ncol(X) - 1

  invXtX <- solve(t(X) %*% X)
  GWI <- invXtX %*% t(X) %*% Y
  if (length(covCalib) == 0) {
    GEV <- t(GWI[sur, ])
  } else {
    GEV <- t(GWI[, exp])
  }
  ERR <- (Y - X %*% GWI)

  ## prediction
  if (ncol(X.MS %*% GWI) == 1) {
    X.hat_MS <- as.matrix((X.MS %*% GWI))
    X.hat_VS <- as.matrix((X %*% GWI))
  } else {
    X.hat_MS <- as.matrix((X.MS %*% GWI)[, exp])
    X.hat_VS <- as.matrix((X %*% GWI)[, exp])
  }

  colnames(X.hat_MS) <- paste0(exp, ".hat")
  colnames(X.hat_VS) <- paste0(exp, ".hat")

  MS_new <- ms_complete
  nrow_ms_complete <- nrow(ms_complete)

  for (i in 1:length(exp)) {
    exp_i <- exp[i]
    exphat_i <- paste0(exp, ".hat")[i]
    if (external == TRUE) {
      MS_new[[exp_i]] <- X.hat_MS[, exphat_i]
    } else if (external == FALSE) {
      MS_new[MS_new[[vsIndicator]] == 0, exp_i] <- X.hat_MS[MS_new[, vsIndicator] == 0, exphat_i]
    }
  }

  # step 2: outcome model
  if (length(covOutcome) == 0) {
    outcomeFormula <- paste0("~", paste0(exp, collapse = "+"))
  } else {
    outcomeFormula <- paste0("~", paste0(exp, collapse = "+"), "+", paste0(covOutcome, collapse = "+"))
  }

  if (!method %in% c("lm", "glm")) {
    stop("method must be either 'lm' or 'glm'.")
  }

  if (method == "lm") {
    outModel <- stats::lm(formula = stats::as.formula(paste0(outcome, outcomeFormula)), data = MS_new)
  } else {
    outModel <- stats::glm(
      formula = stats::as.formula(paste0(outcome, outcomeFormula)),
      data = MS_new,
      family = do.call(family, list(link = link))
    )
  }

  if (method == "lm") {
    Y.hat_MS <- stats::predict(outModel)
  } else if (method == "glm") {
    Y.hat_MS <- stats::predict.glm(outModel, type = "response")
  }

  Y_MS <- MS_new[, outcome]

  if (external == FALSE) {
    Y.hat_VS <- Y.hat_MS[MS_new[, vsIndicator] == 1]
    Y_VS <- MS_new[MS_new[, vsIndicator] == 1, outcome]
  }

  pointEstimate <- stats::coef(outModel)

  if (show_detail == TRUE) {
    print("point estimate =:")
    print(pointEstimate)
  }

  #################################
  # 2. 95% CI and Standard Errors #
  #################################
  n <- nrow(vs_complete)
  m <- nrow(MS_new)

  ms_complete_VS <- ms_complete

  if (length(covCalib) == 0) {
    exposureFormulaX <- paste0("~", paste0(sur, collapse = "+"))
  } else {
    exposureFormulaX <- paste0("~", paste0(sur, collapse = "+"), "+", paste0(covCalib, collapse = "+"))
  }

  if (length(covOutcome) == 0) {
    exposureFormulaY <- paste0("~", paste0(exp, collapse = "+"))
  } else {
    exposureFormulaY <- paste0("~", paste0(exp, collapse = "+"), "+", paste0(covOutcome, collapse = "+"))
  }

  Z_EVS <- stats::model.matrix(object = stats::as.formula(exposureFormulaX), data = vs_complete) %>% as.data.frame()
  Z_MS  <- stats::model.matrix(object = stats::as.formula(exposureFormulaX), data = ms_complete_VS) %>% as.data.frame()
  X_MS  <- stats::model.matrix(object = stats::as.formula(exposureFormulaY), data = MS_new) %>% as.data.frame()

  if (external == FALSE) {
    EVS_new <- MS_new[MS_new[, vsIndicator] == 1, ]
    X_EVS <- stats::model.matrix(object = stats::as.formula(exposureFormulaY), data = EVS_new) %>% as.data.frame()
  }

  ## Overwrite covariate lists to include possible expansion for categorical variables
  covOutcome     <- setdiff(colnames(X_MS)[-1], exp)
  covCalib       <- setdiff(colnames(Z_EVS)[-1], sur)
  covBoth        <- intersect(covOutcome, covCalib)
  covOutcomeOnly <- setdiff(covOutcome, covBoth)
  covCalibOnly   <- setdiff(covCalib, covBoth)
  covAll         <- union(covCalib, covOutcome)

  ## (1) covariates in covCalib but not in MS
  if (length(covCalibOnly) > 0) {
    for (i in 1:length(covCalibOnly)) {
      name <- covCalibOnly[i]
      assign(name, rep(0, m))
      X_MS[, name] <- get(name)
      if (external == FALSE) {
        assign(name, rep(0, n))
        X_EVS[, name] <- get(name)
      }
    }
  }

  ## (2) covariates in covOutcome but not in EVS
  if (length(covOutcomeOnly) > 0) {
    for (i in 1:length(covOutcomeOnly)) {
      name <- covOutcomeOnly[i]
      assign(name, rep(0, n))
      Z_EVS[, name] <- get(name)
      assign(name, rep(0, m))
      Z_MS[, name] <- get(name)
    }
  }

  varOrderZ <- c("(Intercept)", sur, covAll)
  varOrderX <- c("(Intercept)", exp, covAll)
  Z_EVS <- Z_EVS[, varOrderZ] %>% as.matrix()
  Z_MS  <- Z_MS[, varOrderZ] %>% as.matrix()
  X_MS  <- X_MS[, varOrderX] %>% as.matrix()
  if (external == FALSE) {
    X_EVS <- X_EVS[, varOrderX] %>% as.matrix()
  }

  # embedded helper: mean of outer products (used in comments/legacy)
  meanOfRowVectorInMatrix1 <- function(Vhat, Z, X) {
    matrixRowValue <- matrix(NA, nrow = nrow(Z), ncol = ncol(Z) * ncol(X))
    for (i in 1:nrow(Z)) {
      Vhat_i <- as.matrix(Vhat[i])
      Z_i <- as.matrix(Z[i, ])
      X_i <- as.matrix(X[i, ])
      ZX_i <- (Z_i) %*% Vhat_i %*% t(X_i)
      matrixRowValue[i, ] <- ZX_i
    }
    meanMatrixRowValue <- apply(X = matrixRowValue, MARGIN = 2, FUN = mean)
    outputMatrix <- matrix(meanMatrixRowValue, nrow = ncol(Z), ncol = ncol(Z))
    colnames(outputMatrix) <- c("(Intercept)", exp, covAll)
    rownames(outputMatrix) <- c("(Intercept)", exp, covAll)
    return(outputMatrix)
  }

  meanOfRowVectorInMatrix2 <- function(DhatZ, DhatX, Z, X) {
    matrixRowValue <- matrix(NA, nrow = nrow(Z), ncol = ncol(Z) * ncol(X))
    for (i in 1:nrow(Z)) {
      DhatZ_i <- DhatZ[i]
      DhatX_i <- DhatX[i]
      Z_i <- as.matrix(Z[i, ])
      X_i <- as.matrix(X[i, ])
      ZX_i <- (Z_i) %*% DhatZ_i %*% t((X_i) %*% DhatX_i)
      matrixRowValue[i, ] <- ZX_i
    }
    meanMatrixRowValue <- apply(X = matrixRowValue, MARGIN = 2, FUN = mean)
    outputMatrix <- matrix(meanMatrixRowValue, nrow = ncol(Z), ncol = ncol(Z))
    return(outputMatrix)
  }

  mismeasureNumber <- length(exp)

  # step 4: V_hat coefficient for GLM families
  if (method == "lm") {
    V_hat <- rep(1, length(Y.hat_MS))
  } else if (link == "logit") {
    V_hat <- Y.hat_MS * (1 - Y.hat_MS)
  } else if (link == "log") {
    V_hat <- Y.hat_MS
  } else {
    stop("Unsupported link '", link, "'. Only 'logit' and 'log' are currently supported.")
  }

  ## preparation: residuals from calibration model
  if (ncol(Y) == 1) {
    var_X_ZV  <- (Y - X.hat_VS)^2
    diff_X_ZV <- as.matrix(Y - X.hat_VS)
  } else {
    var_X_ZV  <- (Y[, exp] - X.hat_VS)^2
    diff_X_ZV <- as.matrix(Y[, exp] - X.hat_VS)
  }

  if (show_detail == TRUE) {
    print("(X-X.hat)^2/n =:")
    print(mean(var_X_ZV))
    print(sqrt(mean(var_X_ZV)))
  }
  colnames(diff_X_ZV) <- exp

  var_Y_XV  <- (MS_new[, outcome] - Y.hat_MS)^2
  diff_Y_XV <- (MS_new[, outcome] - Y.hat_MS)

  if (external == FALSE) {
    var_Y_XV_VS  <- (MS_new[MS_new[, vsIndicator] == 1, outcome] - Y.hat_VS)^2
    diff_Y_XV_VS <- (MS_new[MS_new[, vsIndicator] == 1, outcome] - Y.hat_VS)
  }

  if (show_detail == TRUE) {
    print("(Y-Y.hat)^2/m =:")
    print(mean(var_Y_XV))
    print(sqrt(mean(var_Y_XV)))
  }

  ## A MATRICES
  ### A_11
  A_11 <- t(Z_EVS) %*% (Z_EVS) / n
  A_11_i <- A_11
  nrow_A_11_i <- nrow(A_11_i)
  ncol_A_11_i <- ncol(A_11_i)
  if (mismeasureNumber > 1) {
    for (i in 2:(mismeasureNumber)) {
      nrow_A_11 <- nrow(A_11)
      ncol_A_11 <- ncol(A_11)
      A_11_j <- as.matrix(rbind(
        cbind(A_11, matrix(0, nrow = nrow_A_11, ncol = ncol_A_11_i)),
        cbind(matrix(0, nrow = nrow_A_11_i, ncol = ncol_A_11), A_11_i)
      ))
      A_11 <- A_11_j
    }
    rownames(A_11) <- rep(colnames(Z_EVS), mismeasureNumber)
    colnames(A_11) <- rep(colnames(Z_EVS), mismeasureNumber)
  }
  A_11 <- A_11[!row.names(A_11) %in% covOutcomeOnly, !colnames(A_11) %in% covOutcomeOnly]

  if (show_detail == TRUE) {
    print("A_11 =:")
    print(A_11)
  }

  ### A_21
  A_21_blocks <- vector("list", mismeasureNumber)

  for (j in seq_len(mismeasureNumber)) {
    exp_j  <- exp[j]
    beta_j <- unname(pointEstimate[exp_j])

    if (is.na(beta_j)) {
      stop("Coefficient for exposure ", exp_j, " is missing from the outcome model.")
    }
    if (!exp_j %in% colnames(X_MS)) {
      stop("Exposure ", exp_j, " is not found in the outcome design matrix X_MS.")
    }

    residual_correction_j <- matrix(
      0, nrow = nrow(X_MS), ncol = ncol(X_MS),
      dimnames = list(NULL, colnames(X_MS))
    )
    residual_correction_j[, exp_j] <- diff_Y_XV

    A_21_X_MS_j <- beta_j * X_MS - residual_correction_j
    A_21_blocks[[j]] <- sweep(t(A_21_X_MS_j), 2, V_hat, "*") %*% Z_MS / m
  }

  A_21 <- do.call(cbind, A_21_blocks)
  rownames(A_21) <- colnames(X_MS)
  colnames(A_21) <- rep(colnames(Z_MS), mismeasureNumber)
  A_21 <- A_21[
    !rownames(A_21) %in% covCalibOnly,
    !colnames(A_21) %in% covOutcomeOnly,
    drop = FALSE
  ]

  if (show_detail == TRUE) {
    print("A_21 =:")
    print(A_21)
  }

  A_12 <- matrix(0, nrow = ncol(A_21), ncol = nrow(A_21))

  ### A_22
  A_22 <- sweep(t(X_MS), 2, V_hat, "*") %*% X_MS / m
  A_22 <- A_22[!row.names(A_22) %in% covCalibOnly, !colnames(A_22) %in% covCalibOnly]

  if (show_detail == TRUE) {
    print("A_22 =:")
    print(A_22)
  }

  A <- cbind(rbind(A_11, A_21), rbind(A_12, A_22))
  invA <- solve(A)

  ## B MATRICES
  B_11 <- matrix(NA, nrow = length(exp) * ncol(Z_EVS), ncol = length(exp) * ncol(Z_EVS))
  for (i in 1:(mismeasureNumber)) {
    for (j in 1:(mismeasureNumber)) {
      exp_i <- exp[i]
      exp_j <- exp[j]
      e_i_matrix <- diff_X_ZV[, exp_i]
      e_j_matrix <- diff_X_ZV[, exp_j]
      sigma2X_ZV_ij <- sum(e_i_matrix * e_j_matrix) / (n - length(covCalib) - mismeasureNumber - 1)
      B_11_ij <- sigma2X_ZV_ij * t(Z_EVS) %*% Z_EVS / (n^2)
      B_11[((i - 1) * (ncol(Z_EVS)) + 1):(i * (ncol(Z_EVS))), ((j - 1) * (ncol(Z_EVS)) + 1):(j * (ncol(Z_EVS)))] <- B_11_ij
    }
  }
  rownames(B_11) <- rep(c("(Intercept)", sur, covAll), mismeasureNumber)
  colnames(B_11) <- rep(c("(Intercept)", sur, covAll), mismeasureNumber)

  if (show_detail == TRUE) {
    print("B_11 =:")
    print(B_11)
  }
  B_11 <- B_11[!row.names(B_11) %in% covOutcomeOnly, !colnames(B_11) %in% covOutcomeOnly]

  X_MS_out <- X_MS[, !colnames(X_MS) %in% covCalibOnly, drop = FALSE]
  if (method == "lm") {
    sigma2Y_XhatV <- sum(var_Y_XV) / stats::df.residual(outModel)
    B_22 <- sigma2Y_XhatV * crossprod(X_MS_out) / m^2
  } else if (method == "glm") {
    score_MS <- sweep(X_MS_out, 1, diff_Y_XV, "*")
    B_22 <- crossprod(score_MS) / m^2
  }
  rownames(B_22) <- colnames(B_22) <- colnames(X_MS_out)

  if (show_detail == TRUE) {
    print("B_22 =:")
    print(B_22)
  }

  B_12 <- matrix(0, nrow = length(exp) * ncol(Z_EVS), ncol = ncol(Z_EVS))
  if (external == FALSE) {
    for (j in 1:(mismeasureNumber)) {
      exp_j <- exp[j]
      Z_weighted <- sweep(t(Z_EVS), 2, diff_X_ZV[, exp_j], "*")
      X_weighted <- sweep(X_EVS, 1, diff_Y_XV_VS, "*")
      B_12_j <- Z_weighted %*% X_weighted / (m * n)
      B_12[((j - 1) * (ncol(Z_EVS)) + 1):(j * (ncol(Z_EVS))), ] <- B_12_j
    }
  }
  rownames(B_12) <- rep(colnames(Z_EVS), mismeasureNumber)
  colnames(B_12) <- colnames(X_MS)
  B_12 <- B_12[!row.names(B_12) %in% covOutcomeOnly, !colnames(B_12) %in% covCalibOnly]

  B <- rbind(cbind(B_11, B_12), cbind(t(B_12), B_22))
  temp <- invA %*% B %*% t(invA)
  Z_EVS_varNames <- colnames(Z_EVS)
  covarianceEstimates <- temp[!row.names(temp) %in% Z_EVS_varNames, !colnames(temp) %in% Z_EVS_varNames]

  if (show_detail == TRUE) {
    print("VAR_B =:")
    print(covarianceEstimates)
  }

  outcomeModelVars_all <- colnames(X_MS)[-1]
  outcomeModel_only    <- names(stats::coef(outModel))[-1]

  kZ <- nrow(A_11)
  kT <- nrow(temp)
  covarianceEstimates <- temp[(kZ + 1):kT, (kZ + 1):kT, drop = FALSE]
  rownames(covarianceEstimates) <- colnames(covarianceEstimates) <- rownames(A_22)

  outcomeModel_only <- names(stats::coef(outModel))[-1]
  Bhat <- pointEstimate[outcomeModel_only]

  covarianceEstimates <- as.matrix(covarianceEstimates)

  missingCoefs <- setdiff(outcomeModel_only, rownames(covarianceEstimates))
  if (length(missingCoefs) > 0) {
    stop("Outcome coefficients missing from covarianceEstimates: ",
         paste(missingCoefs, collapse = ", "))
  }

  VB    <- covarianceEstimates[outcomeModel_only, outcomeModel_only, drop = FALSE]
  SE_B  <- sqrt(diag(VB))

  # compose output tables
  zValue <- Bhat / SE_B
  pValue <- 2 * stats::pnorm(-abs(as.numeric(zValue)))
  lcl    <- Bhat - stats::qnorm(0.975) * SE_B
  ucl    <- Bhat + stats::qnorm(0.975) * SE_B
  BSebP  <- cbind(Bhat, SE_B, zValue, pValue, lcl, ucl)

  colnames(BSebP) <- c("Estimate", "Std. Error", "Z Value", "Pr(>|Z|)", "lower 95%CI", "upper 95%CI")

  outputList <- list(BSebP, VB)
  names(outputList) <- c("correctedCoefTable", "correctedVCOV")

  return(outputList)
}
