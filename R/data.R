#' Yeast Colony Area on 384 Plate Format
#'
#' A dataset containing the plate coordinates, and data subtypes obtained by
#' categorizing the colony area of yeast using plate growth median
#' on a 384 plate format.
#'
#' @docType data
#' @keywords datasets
#' @name colonyarea
#' @author Sathish Kumar Srinivasan  \email{sathishsrinivasank@@gmail.com}
#' @usage colonyarea
#' @format A data frame with 384 rows and 3 variables.
#'
#' \tabular{rll}{
#'  [, 1] \tab \code{row}           \tab Horizontal coordinates of a plate.
#'                                       It must have character class.\cr
#'  [, 2] \tab \code{column}        \tab Vertical coordinates of a plate.
#'                                       It must have numeric class.\cr
#'  [, 3] \tab \code{data_subtypes} \tab It must have character class
#'                                       indicating the data subtypes.\cr
#' }
NULL
