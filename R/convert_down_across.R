#' Convert data into rowwise(across) or coloumnwise(down) within a plate format
#'
#' Convert data into rowwise(across) or coloumnwise(down) within a plate format
#'
#' Make sure the length of the given data in \code{data_from} argument matches
#' the value in \code{plateformat}
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536 or 6144
#' @param data_from vector of data for conversion
#' @param is_plate_coords logical returns plate coordinates, if its value is
#' \code{TRUE}.
#' @param out_data_flow A string which tells the function to output the
#' converted data to a plate configuration which can be down or across.
#'
#' @return dataframe of converted data from across to down or vice versa.
#'
#' @export
#'
#' @examples
#' plateformat <- 384
#' convert_down_across(plateformat = plateformat,
#'                     data_from = 1:plateformat,
#'                     is_plate_coords = TRUE,
#'                     out_data_flow = 'across')
convert_down_across <- function(plateformat,
                                data_from,
                                is_plate_coords = TRUE,
                                out_data_flow)
{
  if(!is.vector(data_from) && length(data_from) != plateformat){
    stop(paste('data_from must be a vector and its length must be equal to',
               plateformat, sep=''))
  }

  if(out_data_flow == 'down' || out_data_flow == 'across') {
    if(!is.logical(is_plate_coords)){
      stop('is_plate_coords must be a logical value with TRUE or FALSE')
    }

    if(is_plate_coords){
      return(plate_coords(plateformat, data_from, out_data_flow))
    } else {
      return(data_from)
    }
  } else {
    stop('out_data_flow argument values must be either down or across')
  }
}
