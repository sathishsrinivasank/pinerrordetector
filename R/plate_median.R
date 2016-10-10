#' Plate median of the given colony area of a plate
#'
#' Plate median of the given colony area of a plate. It removes colony area of
#' empty indices before computing the median of the plate.
#'
#' @param empty_indices A numeric vector of indexed empty locations. It can also
#' be the indices of control colonies grown on a plate, which may be used for
#' normalizing the colony area of a plate
#' @param colony_area_raw_data A numeric vector of raw data representing the area
#' yeast grown at a specific location on a nutrient medium agar plate
#'
#' @return Numeric value with plate_median of the given
#' \code{colony_area_raw_data}
#' @export
#'
#' @examples
#' data_area <- simulated_data_1536(data_384 = colonyarea$data_subtypes,
#'                                  in_data_flow = "across",
#'                                  out_data_flow = "down",
#'                                  is_plate_coords = TRUE)
#' empty_indices <- which(convert_small_to_large(plate_from = 384,
#'                                               plate_to = 1536,
#'                                               data_from = colonyarea$data_subtypes,
#'                                               in_data_flow = 'across',
#'                                               out_data_flow = "down",
#'                                               is_plate_coords = FALSE)$y %in% 'Empty')
#' plate_median(empty_indices, data_area$y)   # 600
#'
plate_median <- function(empty_indices = NULL, colony_area_raw_data, na.rm = TRUE)
{
  # 1. sanity checks
  stopifnot(is.vector(colony_area_raw_data) &&
              (is.numeric(colony_area_raw_data) ||
                 is.integer(colony_area_raw_data)))
  stopifnot(is.null(empty_indices) || (is.vector(empty_indices) &&
                                         (is.numeric(empty_indices) ||
                                            is.integer(empty_indices))))

  # 2. return plate median for colony size
  if(is.null(empty_indices)){
    return(median(colony_area_raw_data, na.rm = na.rm))
  } else {
    return(median(colony_area_raw_data[-unique(empty_indices)], na.rm = na.rm))
  }
}
