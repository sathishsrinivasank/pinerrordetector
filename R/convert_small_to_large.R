#' Convert data of small plateformat to large plateformat
#'
#' Convert data of small plateformat \code{plate_from} to large plateformat
#' \code{plate_to} by expanding the four replicates into large plateformat
#' \code{plate_to}
#'
#' @param plate_from An integer of small plateformat
#' @param plate_to An integer of large plateformat
#' @param data_from vector of data for conversion
#' @param out_data_flow A string which tells the function to output the
#' converted data to a plate configuration which can be down or across
#' @param is_plate_coords logical returns plate coordinates, if its value is
#' \code{TRUE}
#'
#' @return dataframe of converted data from small plateformat \code{plate_from}
#' to large plateformat \code{plate_to}. If \code{is_plate_coords = TRUE}, then
#' it returns the plate coordinates along with the converted data.
#'
#' @export
#'
#' @examples
#' across_1536 <- convert_small_to_large(plate_from = 384,
#'                                       plate_to = 1536,
#'                                       data_from = colonyarea$data_subtypes,
#'                                       out_data_flow = 'across',
#'                                       is_plate_coords = TRUE)
#' head(across_1536)
#'

convert_small_to_large <- function(plate_from,
                                   plate_to,
                                   data_from,
                                   out_data_flow,
                                   is_plate_coords)
{
  # 1. get appropriate plate indices for the given out_data_flow
  x <- indices_4_replicates(plate_from    = plate_to,
                            plate_to      = plate_from,
                            out_data_flow = out_data_flow)

  # 2. assign data to the converted plate indices
  y <- 1:plate_to
  counter <- 1
  while(counter <= plate_from){
    y[as.numeric(x[counter,])] <- data_from[counter]
    counter <- counter + 1
  }

  # 3. add plate coordinates to output
  if(missing(is_plate_coords) || !is.logical(is_plate_coords)){
    stop('is_plate_coords must be a logical value with TRUE or FALSE')
  }

  if(is_plate_coords){
    data_to <- plate_coords(plate_to, y, out_data_flow)
    return(data_to)
  } else {
    return(data.frame(y = y, stringsAsFactors = FALSE))
  }
}
