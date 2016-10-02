#' Generate Rectangular Plate Coordinates
#'
#' plate_coords generates rectangular plate coordinates
#'
#' @param plate_to An integer giving plateformat
#' @param data_from vector of data for which coordinates are created
#' @param out_data_flow A string which tells the function to output the
#' converted data to a plate configuration which can be down or across
#' @return Data frame with plate coordinates
#'
#' @export
#'
#' @examples
#' plate_coords(1536, 1:1536, "across")
#'
plate_coords <- function(plate_to, data_from, out_data_flow)
{
  plate_to_row <- plate_nrow(plate_to)
  plate_to_col <- plate_ncol(plate_to)
  p_letters <- plate_letters()[1:plate_to_row]


  if(out_data_flow == 'down'){
    # 1. letters for rows
    f_letters <- rep(p_letters, plate_to_col)

    # 2. numbers for columns
    f_numbers <- rep(1:plate_to_col, each = plate_to_row)

  } else if(out_data_flow == 'across'){
    # 1. letters for rows
    f_letters <- rep(p_letters, each = plate_to_col)

    # 2. numbers for columns
    f_numbers <- rep(1:plate_to_col, plate_to_row)

  } else {
    stop('The argument out_data_flow must have a value either down or across')
  }

  # 3. create and return data frame with coordinates and indices
  return(data.frame(row              = f_letters,
                    column           = f_numbers,
                    y                = data_from,
                    stringsAsFactors = FALSE))
}
