#' Categorize data based on plate median
#'
#' The function \code{categorize_data} will classify the given data
#' \code{data_area} on the basis of plate median and after removing
#' missing values \code{NA}.
#'
#' @param data_area A numerical vector giving the colony size of a
#'  plate
#' @param empty_indices A numerical vector giving the indices of
#'  empty spot locations on a plate
#' @return A character vector with different classes of data
#' @export
#'
#' @examples
#' data_area <- simulated_data_1536(data_384 = colonyarea$data_subtypes,
#'                                  in_data_flow = "across",
#'                                  out_data_flow = "down",
#'                                  is_plate_coords = FALSE)
#'
#' empty_indices <- which(convert_small_to_large(plate_from = 384,
#'                                               plate_to = 1536,
#'                                               data_from = colonyarea$data_subtypes,
#'                                               in_data_flow = 'across',
#'                                               out_data_flow = "down",
#'                                               is_plate_coords = FALSE)$y %in% 'Empty')
#'
#' vec1 <- categorize_data(data_area$y, empty_indices)
#' head(vec1)
#'
categorize_data <- function(data_area, empty_indices)
{
  # 1. sanity checks
  stopifnot(is.numeric(data_area) && is.vector(data_area))
  stopifnot(is.numeric(empty_indices) && is.vector(empty_indices))

  # 2. get plate median
  plate_median <- median(data_area[-empty_indices],  na.rm = TRUE)

  # 3. get indices of different classes of data in data_area
  pe <- which(data_area <= 0)
  lessig <- which((data_area < (plate_median/4)) & (data_area > 0))
  less <- which((data_area <= plate_median) & (data_area >= (plate_median/4)))
  more <- which((data_area > plate_median) & (data_area <= (2*plate_median*0.9)))
  moresig <- which(data_area > (2*plate_median*0.9))

  # 4. assign name of the class to its defined category
  data_area[pe] <- 'Pinning Error'
  data_area[lessig] <- 'Lessthan 25% Plate Median'
  data_area[less] <- 'Lessthan Plate Median'
  data_area[moresig] <- 'Morethan 90% Plate Median'
  data_area[more] <- 'Morethan Plate Median'
  data_area[empty_indices] <- 'Empty'                # empty should be done at the end to prevent
                                                     # clashes with pe because both have values 0

  # 5. return categorized data
  return(data_area)
}
