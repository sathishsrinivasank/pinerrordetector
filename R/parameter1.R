#' Exclusion Criteria-1 of Neighborful Algorithm
#'
#' Exclusion Criteria-1 used in neighborful algorithm on combinations of
#' neighbors adjoining the selected colony
#'
#' @param colony selected colony by the outer loop of the neighborful algorithm
#' @param combinations combinations of neighbors adjoining the selected colony
#' @param colony_area_raw_data A numeric vector of raw data representing the area
#' yeast grown at a specific location on a nutrient medium agar plate
#' @param plate_median_threshold Threshold used in the plate median
#' @param excluded_colonies A numeric vector of indices that are part of excluded
#' colonies
#' @param param1_threshold The numeric value which can be one from \code{1:8}.
#' This exclusion criteria-1 of neighborful algorithm represents the number of
#' adjoining neighbors of the selected colony from \code{colony_indices} that
#' has colony area less than or equal to \code{plate_median_threshold}. If the
#' selected colony is surrounded by at least \code{param1_threshold} neighbors,
#' then it will be considered as excluded colony by the neighborful algorithm
#'
#' @return If the selected colony passes the exclusion criteria - 1, then a
#' vector of excluded colonies with the selected colony will be returned.
#' Else, it will return -1.
#'
#' @examples
#' middle_colony <- 34
#' param1_threshold <- 1
#' neighbors_selected_colony <- middle_neighbors(plateformat, middle_colony)
#' combin_coords <- combn(length(neighbors_selected_colony), 1)
#' combinations <- neighbors_selected_colony[(combin_coords[,1])]
#' data_area <- simulated_data_1536(colonyarea$data_subtypes)
#' empty_indices <- which(convert_384_to_1536_data(colonyarea$data_subtypes)
#'                        %in% 'Empty')
#' p_median <- plate_median(empty_indices, data_area)
#' plate_median_threshold <- p_median/4
#' parameter1(colony = middle_colony,
#'            combinations = combinations,
#'            colony_area_raw_data = data_area,
#'            plate_median_threshold = plate_median_threshold,
#'            excluded_colonies = c(2),
#'            param1_threshold = param1_threshold)
parameter1 <- function(colony,
                       combinations,
                       colony_area_raw_data,
                       plate_median_threshold,
                       excluded_colonies,
                       param1_threshold)
{
  if (length(which(colony_area_raw_data[combinations] <=
                   plate_median_threshold)) >= param1_threshold) {

    return(unique(c(excluded_colonies, colony)))

  } else {
    return(-1)
  }
}
