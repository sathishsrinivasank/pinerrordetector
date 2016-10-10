#' Neighborful algorithm - Variation-F
#'
#' The variation-F of neighborful algorithm uses both \code{param1_threshold} and
#' \code{param2_threshold} exclusion criteria to remove colonies as excluded ones
#' by removing \code{empty_indices} from outer loop and by not removing
#' \code{empty_indices} from inner loop of the algorithm. The outer loop of the
#' algorithm is used to iterate over the list of \code{colony indices}. The inner
#' loop of the algorithm is used to iterate over all possible combinations of
#' adjoining neighbors of a selected colony from the outer loop.
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#' @param colony_indices A vector of indices. If the value of \code{is_middle}
#' is \code{TRUE}, then middle colony indices must be used. If the value of
#' \code{is_middle} is \code{FALSE}, then peripheral colony indices must be used
#' @param colony_area_raw_data A numeric vector of raw data representing the area
#' yeast grown at a specific location on a nutrient medium agar plate
#' @param empty_indices A numeric vector of indexed empty locations. It can also
#' be the indices of control colonies grown on a plate, which may be used for
#' normalizing the colony area of a plate
#' @param excluded_colonies A numeric vector of indices that are part of excluded
#' colonies
#' @param plate_median_threshold This threshold value is computed by multiplying
#' the value of \code{percent_median_thresh} with the value from
#' \code{plate_median(empty_indices,colony_area_raw_data)} and dividing it by 100
#' @param param1_threshold The numeric value which can be one from \code{1:8}.
#' This exclusion criteria-1 of neighborful algorithm represents the number of
#' adjoining neighbors of the selected colony from \code{colony_indices} that
#' has colony area less than or equal to \code{plate_median_threshold}. If the
#' selected colony is surrounded by at least \code{param1_threshold} neighbors,
#' then it will be considered as excluded colony by the neighborful algorithm
#' @param param2_threshold The numeric value which can be one from \code{1:8}.
#' This exclusion criteria-2 of neighborful algorithm represents the number of
#' adjoing neighbors of the selected colony from \code{colony_indices} are
#' excluded by the previous iteration of neighborful algorithm of variation-F.
#' If the selected colony is surrounded by at least \code{param2_threshold}
#' neighbors, then it will be considered as excluded colony by the neighborful
#' algorithm
#' @param is_middle logical. If \code{TRUE}, then \code{colony_indices} must have
#' indices from the middle of the plate. If \code{FALSE}, then \code{colony_indices}
#' must have indices from the periphery of the plate
#'
#' @return The numeric vector of excluded colonies by neighborful algorithm
#'  of variation-F
#' @export
#'
#' @examples
#' plateformat <- 1536
#' data_area <- simulated_data_1536(data_384 = colonyarea$data_subtypes,
#'                                  in_data_flow = "across",
#'                                  out_data_flow = "down",
#'                                  is_plate_coords = TRUE)
#' empty_indices <- which(convert_small_to_large(plate_from = 384,
#'                                               plate_to = plateformat,
#'                                               data_from = colonyarea$data_subtypes,
#'                                               in_data_flow = 'across',
#'                                               out_data_flow = "down",
#'                                               is_plate_coords = FALSE)$y %in% 'Empty')
#' p_median <- plate_median(empty_indices = empty_indices,
#'                          colony_area_raw_data = data_area$y)
#' plate_median_threshold <- p_median/4
#' all_indices <- 1:plateformat
#' middle_colony_indices <- all_indices[-(not_middle_indices(plateformat))]
#' excluded_coloniesF(plateformat = plateformat,
#'                    colony_indices = middle_colony_indices,
#'                    colony_area_raw_data = data_area$y,
#'                    empty_indices = empty_indices,
#'                    excluded_colonies = c(),
#'                    plate_median_threshold = plate_median_threshold,
#'                    param1_threshold = 1,
#'                    param2_threshold = 1,
#'                    is_middle = TRUE)
excluded_coloniesF <- function(plateformat,
                               colony_indices,
                               colony_area_raw_data,
			                         empty_indices,
                               excluded_colonies,
                               plate_median_threshold,
                               param1_threshold,
                               param2_threshold,
                               is_middle)
{
  # pick a colony
  for (colony in colony_indices) {
    if(colony_area_raw_data[colony] <= plate_median_threshold){
      if(! colony %in% empty_indices){

        # get neighbors for selected colony
        if(is_middle){
          neighbors_selected_colony <- middle_neighbors(plateformat, colony)
        } else {
          neighbors_selected_colony <- not_middle_neighbors(plateformat, colony)
        }

        # generate combinations of ncoordinates by sampling
        # param1_threshold out of maximum kernel density
        max_kernel_density <- length(neighbors_selected_colony)

        if(max_kernel_density >= param1_threshold) {
          combin_coords <- combn(max_kernel_density, param1_threshold)

          # apply variation-2
          excluded_colonies <- variation2(colony = colony,
                                          combin_coords = combin_coords,
                                          neighbors_selected_colony = neighbors_selected_colony,
                                          excluded_colonies = excluded_colonies,
                                          colony_area_raw_data = colony_area_raw_data,
                                          plate_median_threshold = plate_median_threshold,
                                          param1_threshold = param1_threshold,
                                          param2_threshold = param2_threshold)
        }
      }
    }
  }
  return(excluded_colonies)
}

