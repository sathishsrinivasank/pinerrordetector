#' Convert data of large plateformat to small plateformat
#'
#' Convert data of large plateformat  \code{plate_from} to small plateformat
#' \code{plate_to} by getting the four replicates of large plateformat
#' \code{plate_from}
#'
#' Always convert the plateformat one step at a time. For example 1536 to 384
#' but not from 1536 to 96 plateformat
#'
#' @param plate_from An integer of large plateformat
#' @param plate_to An integer of small plateformat
#' @param data_from vector of data for conversion
#' @param out_data_flow A string which tells the function to output the
#' converted data to a plate configuration which can be down or across
#' @param is_plate_coords logical returns plate coordinates, if its value is
#' \code{TRUE}
#' @param is_means logical if its value is \code{TRUE}, it converts four
#' replicates into its average after removing any NA values
#'
#' @return dataframe of converted data from large plateformat \code{plate_from}
#' to small plateformat \code{plate_to}. If \code{is_plate_coords = TRUE}, then
#' it returns the plate coordinates along with the converted data
#'
#' @export
#'
#' @examples
#' across_384 <- convert_large_to_small(plate_from = 1536,
#'                                      plate_to = 384,
#'                                      data_from = 1:1536,
#'                                      out_data_flow = 'across',
#'                                      is_plate_coords = TRUE,
#'                                      is_means = FALSE)
#' head(across_384)
#'
#' # convert replicates into mean after removing NAs
#' across_384 <- convert_large_to_small(plate_from = 1536,
#'                                      plate_to = 384,
#'                                      data_from = 1:1536,
#'                                      out_data_flow = 'across',
#'                                      is_plate_coords = TRUE,
#'                                      is_means = TRUE)
#' head(across_384)
#'

convert_large_to_small <- function(plate_from,
                                   plate_to,
                                   data_from,
                                   out_data_flow,
                                   is_plate_coords,
                                   is_means)
{
  # 1. get appropriate plate indices for the given out_data_flow
  x <- indices_4_replicates(plate_from    = plate_from,
                            plate_to      = plate_to,
                            out_data_flow = out_data_flow)

  # 2. assign data to the converted plate indices
  counter <- 1
  while(counter <= plate_to){
    x[counter,] <- data_from[as.numeric(x[counter,])]
    counter <- counter + 1
  }

  data_to <- x

  # 3. add plate coordinates to output
  if(missing(is_plate_coords) || !is.logical(is_plate_coords)){
    stop('is_plate_coords must be a logical value with TRUE or FALSE')
  }

  if(is_plate_coords){
    data_to <- plate_coords(plate_to, data_to, out_data_flow)
    names(data_to)[3:6] <- c('X1', 'X2', 'X3', 'X4')
  }

  # 4. return entire data or the row means of the data
  if(is_means){
    if(is_plate_coords){
      return(data.frame(row              = data_to[, 1],
                        column           = data_to[, 2],
                        means            = rowMeans(data_to[, 3:6],
                                                    na.rm = TRUE),
                        stringsAsFactors = FALSE))
    } else {
      return(data.frame(means            = rowMeans(data_to, na.rm = TRUE),
                        stringsAsFactors = FALSE))
    }
  } else {
    return(data_to)
  }
}
