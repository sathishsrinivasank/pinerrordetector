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
#' data_area <- simulated_data_1536(colonyarea$data_subtypes)
#' empty_indices <- which(convert_384_to_1536_data(colonyarea$data_subtypes)
#'                        %in% 'Empty')
#' plate_median(empty_indices, data_area)
plate_median <- function(empty_indices, colony_area_raw_data)
{
  # first column of colony_area_raw_data: colony area
  # second column of colony_area_raw_data: circularity

  return(median(colony_area_raw_data[-(empty_indices)],
                na.rm = TRUE))
}
