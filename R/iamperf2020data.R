#' The data of the 2020 IAM Performance Measurement Survey
#'
#' This data contains the survey data after the following transformation steps:
#' 1). Extraction from QuestionPro in SPSS format.
#' 2). Opening and re-saving in SPSS format with IBM SPSS to solve a technical issue with the file exported from QuestionPro.
#' 3). Data importation in R using the setup_data package function.
#' 4). Extraction of the dataset dimensions that do not contain PII information.
#' 5). Saving of the data in .RData format in the package /data folder.
#'
#' @docType data
#'
#' @usage data(iamperf2020data)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references The Open-Measure Project
#' (\href{https://www.open-measure.org}{Open-Measure})
#'
#' @source \href{https://www.open-measure.org}{Open-Measure}
#'
#' @examples
#' data(iamperf2020data);
#' head(iamperf2020data);
"iamperf2020data"
