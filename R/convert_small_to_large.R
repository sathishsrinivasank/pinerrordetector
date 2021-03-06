#' Convert data of small plateformat to large plateformat
#'
#' Convert data of small plateformat \code{plate_from} to large plateformat
#' \code{plate_to} by expanding the four replicates into large plateformat
#' \code{plate_to}
#'
#' @param plate_from An integer of small plateformat
#' @param plate_to An integer of large plateformat
#' @param data_from vector of data for conversion
#' @param in_data_flow A string indicating the plate format of \code{data_from}.
#' It can take a value either across or down.
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
#' down_1536 <- convert_small_to_large(plate_from = 384,
#'                                     plate_to = 1536,
#'                                     data_from = 1:384,
#'                                     in_data_flow = 'down',
#'                                     out_data_flow = 'down',
#'                                     is_plate_coords = TRUE)
#' head(down_1536)
#'

convert_small_to_large <- function(plate_from,
                                   plate_to,
                                   data_from,
                                   in_data_flow,
                                   out_data_flow,
                                   is_plate_coords)
{
  # 1. sanity checks
  stopifnot((is.vector(data_from) && length(data_from) == plate_from) ||
              (is.data.frame(data_from) && nrow(data_from) == plate_from
               && (length(data_from) == 1 || length(data_from) == 4)))
  stopifnot(length(in_data_flow) == 1 || length(out_data_flow) == 1)
  stopifnot(in_data_flow %in% c('down', 'across'))
  stopifnot(out_data_flow %in% c('down', 'across'))
  stopifnot(is.logical(is_plate_coords) && length(is_plate_coords) == 1)

  # 2. convert vector format of data_from to data frame class
  if(is.vector(data_from)){
    data_from <- data.frame(y = data_from, stringsAsFactors = FALSE)
  }

  # 3. get appropriate plate indices for the given in_data_flow
  x <- indices_4_replicates(plate_from    = plate_to,
                            plate_to      = plate_from,
                            out_data_flow = in_data_flow)

  # 4. assign data_from of in_data_flow format to data_to of in_data_flow format
  data_to <- data.frame(y = 1:plate_to)

  if(length(data_from) == 1){
    counter <- 1
    while(counter <= plate_from){
      data_to[as.numeric(x[counter,]), 'y'] <- data_from[counter, ]
      counter <- counter + 1
    }
  } else if(length(data_from)== 4){
    counter <- 1
    while(counter <= plate_from){
      row_index <- as.numeric(x[counter, ])
      data_temp <- data_from[counter, ]
      for(i in 1:4){
        data_to[row_index[i], 'y'] <- data_temp[, i]
      }
      counter <- counter + 1
    }
  }

  # 5. convert in_data_flow to out_data_flow format
  data_to <- convert_down_across(plateformat = plate_to,
                                 data_from = data_to,
                                 is_plate_coords = is_plate_coords,
                                 in_data_flow = in_data_flow,
                                 out_data_flow = out_data_flow)

  # 6. return data_to data frame
  return(data_to)

}
