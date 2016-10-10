#' Generate Rectangular Plate Coordinates
#'
#' plate_coords generates rectangular plate coordinates
#'
#' @param plate_to An integer giving plateformat
#' @param data_from vector of data for which coordinates are created
#' @param data_format A string indicating the plate format of
#' \code{data_from}. It can take a value either down or across
#' @return Data frame with plate coordinates
#'
#' @export
#'
#' @examples
#' plate_coords(plate_to = 1536,
#'              data_from = 1:1536,
#'              data_format = "across")
#'
plate_coords <- function(plate_to, data_from, data_format)
{

  plate_to_row <- plate_nrow(plate_to)
  plate_to_col <- plate_ncol(plate_to)
  p_letters <- plate_letters()[1:plate_to_row]


  if(data_format == 'down'){
    # 1. letters for rows
    f_letters <- rep(p_letters, plate_to_col)

    # 2. numbers for columns
    f_numbers <- rep(1:plate_to_col, each = plate_to_row)

  } else if(data_format == 'across'){
    # 1. letters for rows
    f_letters <- rep(p_letters, each = plate_to_col)

    # 2. numbers for columns
    f_numbers <- rep(1:plate_to_col, plate_to_row)

  } else {
    stop('The argument data_format must have a value either down or across')
  }

  # 3. create and return data frame with coordinates and indices
  if(is.vector(data_from)){
    return(data.frame(row              = f_letters,
                      column           = f_numbers,
                      y                = data_from,
                      stringsAsFactors = FALSE))
  } else if(is.data.frame(data_from)){
    return(cbind(row              = f_letters,
                 column           = f_numbers,
                 data_from,
                 stringsAsFactors = FALSE))
  }
}
