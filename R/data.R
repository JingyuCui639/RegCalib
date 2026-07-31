#' Simulated Main-Study Data
#'
#' A simulated main-study dataset for demonstrating regression calibration
#' with error-prone dietary exposures.
#'
#' @format A data frame with 89,538 observations and the following variables:
#' \describe{
#'   \item{id}{Participant identifier.}
#'   \item{fqcal}{Food-frequency questionnaire estimate of caloric intake.}
#'   \item{fqcalinc}{Transformed caloric intake.}
#'   \item{fqtfat}{Food-frequency questionnaire estimate of total fat intake.}
#'   \item{fqtfatinc}{Transformed total fat intake.}
#'   \item{fqalc}{Food-frequency questionnaire estimate of alcohol intake.}
#'   \item{fqalcinc}{Transformed alcohol intake.}
#'   \item{age}{Participant age.}
#'   \item{agec}{Categorized participant age.}
#'   \item{case}{Binary outcome indicator.}
#' }
#'
#' @source Simulated data created for the RegCalib package.
"main_data_sim"
#'
#'
#'
#' Simulated External Validation Data
#'
#' A simulated external validation dataset containing surrogate and reference
#' measurements of dietary exposures.
#'
#' @format A data frame with 173 observations and variables containing
#' food-frequency questionnaire measurements, reference measurements, and age.
#' \describe{
#'   \item{id}{Participant identifier.}
#'   \item{fqcal}{Surrogate caloric intake measurement.}
#'   \item{fqcalinc}{Transformed surrogate caloric intake.}
#'   \item{fqtfat}{Surrogate total-fat intake measurement.}
#'   \item{fqtfatinc}{Transformed surrogate total-fat intake.}
#'   \item{fqalc}{Surrogate alcohol intake measurement.}
#'   \item{fqalcinc}{Transformed surrogate alcohol intake.}
#'   \item{drcal}{Reference caloric intake measurement.}
#'   \item{drcalinc}{Transformed reference caloric intake.}
#'   \item{drtfat}{Reference total-fat intake measurement.}
#'   \item{drtfatinc}{Transformed reference total-fat intake.}
#'   \item{dralc}{Reference alcohol intake measurement.}
#'   \item{dralcinc}{Transformed reference alcohol intake.}
#'   \item{age}{Participant age.}
#'   \item{agec}{Categorized participant age.}
#' }
#'
#' @source Simulated data created for the RegCalib package.
"valid_data_sim"