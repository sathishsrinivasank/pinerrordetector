#' Yeast Colony Area on 384 Plate Format
#'
#' A dataset containing the plate coordinates, and data subtypes obtained by
#' categorizing the colony area of yeast using plate growth median
#' on a 384 plate format. The data is arranged rowwise format (across). See
#' the rowwise arrangement of \code{colonyarea} in the example given below.
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
#' @examples
#' legend_txt_bg_col <- c('Empty'                     = 'red',
#'                        'Pinning Error'             = 'black',
#'                        'Morethan Plate Median'     = '#660066',
#'                        'Lessthan Plate Median'     = 'green',
#'                        'Morethan 90% Plate Median' = 'cyan',
#'                        'Lessthan 25% Plate Median' = 'yellow',
#'                        'Excluded Colonies'         = 'blue')
#' data_subtypes_384 <- colonyarea$data_subtypes
#' plateformat <- 1536
#' across_1536 <- convert_small_to_large(plate_from = 384,
#'                                       plate_to = 1536,
#'                                       data_from = data_subtypes_384,
#'                                       in_data_flow = 'across',
#'                                       out_data_flow = 'across',
#'                                       is_plate_coords = TRUE)
#' plot_platemap(plateformat = plateformat,
#'               plot_data = across_1536,
#'               legend_txt_bg_col = legend_txt_bg_col)
#'
NULL
