#' Variation-3
#'
#' Inner loop of neighborful algorithm used by its variations C and G
#'
#' @param colony selected colony by the outer loop of the neighborful algorithm
#' @param combin_coords combinations of neighbors adjoining the selected colony
#' @param neighbors_selected_colony neighbors adjoining the selected colony
#' @param excluded_colonies A numeric vector of indices that are part of excluded
#' colonies
#' @param colony_area_raw_data A numeric vector of raw data representing the area
#' yeast grown at a specific location on a nutrient medium agar plate
#' @param plate_median_threshold This threshold value is computed by multiplying
#' the value of \code{percent_median_thresh} with the value from
#' \code{plate_median(empty_indices,colony_area_raw_data)} and dividing it by 100
#' @param empty_indices A numeric vector of indexed empty locations. It can also
#' be the indices of control colonies grown on a plate, which may be used for
#' normalizing the colony area of a plate
#' @param param1_threshold The numeric value which can be one from \code{1:8}.
#' This exclusion criteria-1 of neighborful algorithm represents the number of
#' adjoining neighbors of the selected colony from \code{colony} that
#' has colony area less than or equal to \code{plate_median_threshold}. If the
#' selected colony is surrounded by at least \code{param1_threshold} neighbors,
#' then it will be considered as excluded colony by the neighborful algorithm
#'
#' @return A vector of excluded colonies by variation3
#'
#' @examples
#' plateformat <- 1536
#' middle_colony <- 34
#' param1_threshold <- 6
#' neighbors_selected_colony <- middle_neighbors(plateformat, middle_colony)
#' combin_coords <- combn(length(neighbors_selected_colony), param1_threshold)
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
#' variation3(colony = middle_colony,
#'            combin_coords = combin_coords,
#'            neighbors_selected_colony = neighbors_selected_colony,
#'            excluded_colonies = c(2),
#'            colony_area_raw_data = data_area$y,
#'            plate_median_threshold = plate_median_threshold,
#'            empty_indices = empty_indices,
#'            param1_threshold = param1_threshold)
#'
variation3 <- function(colony,
                       combin_coords,
                       neighbors_selected_colony,
                       excluded_colonies,
                       colony_area_raw_data,
                       plate_median_threshold,
                       empty_indices,
                       param1_threshold)
{
  for(combin_coords_index in 1:ncol(combin_coords)){
    combinations <- combin_coords[,combin_coords_index]

    # 1. get neighbor indices
    combinations <- neighbors_selected_colony[combinations]

    # 2. remove indexed empty spots from the neighbors of selected colony
    combinations <- combinations[which(! combinations %in% empty_indices)]

    if(length(combinations) == 0){
      next
    }

    # 3. Apply parameter 1
    excluded_colonies_param1 <- parameter1(colony = colony,
                                           combinations = combinations,
                                           colony_area_raw_data = colony_area_raw_data,
                                           plate_median_threshold = plate_median_threshold,
                                           excluded_colonies = excluded_colonies,
                                           param1_threshold = param1_threshold)
    if(excluded_colonies_param1[1] != -1){
      excluded_colonies <- excluded_colonies_param1
      break()
    }
  }

  # 4. return excluded colonies
  return(excluded_colonies)
}
