#' Exclusion Criteria-2 of Neighborful Algorithm
#'
#' Exclusion Criteria-2 used in neighborful algorithm on combinations of
#' neighbors adjoining the selected colony
#'
#' @param colony selected colony by the outer loop of the neighborful algorithm
#' @param combinations combinations of neighbors adjoining the selected colony
#' @param excluded_colonies A numeric vector of indices that are part of excluded
#' colonies
#' @param param2_threshold The numeric value which can be one from \code{1:8}.
#' This exclusion criteria-2 of neighborful algorithm represents the number of
#' adjoing neighbors of the selected colony from \code{colony} are
#' excluded by the previous iteration of neighborful algorithm. If the selected
#' colony is surrounded by at least \code{param2_threshold} neighbors, then it
#' will be considered as excluded colony by the neighborful algorithm.
#'
#' @return If the selected colony passes the exclusion criteria - 2, then a
#' vector of excluded colonies along with the selected colony will be returned.
#' Else, it will return -1.
#'
#' @examples
#' middle_colony <- 34
#' param1_threshold <- 6
#' param2_threshold <- 2
#' neighbors_selected_colony <- middle_neighbors(plateformat, middle_colony)
#' combin_coords <- combn(length(neighbors_selected_colony), param1_threshold)
#' combinations <- neighbors_selected_colony[(combin_coords[,1])]
#' parameter2(colony = middle_colony,
#'            combinations = combinations,
#'            excluded_colonies = c(2),
#'            param2_threshold = param2_threshold)
parameter2 <- function(colony,
                       combinations,
                       excluded_colonies,
                       param2_threshold)
{
  if (length(intersect(excluded_colonies, combinations)) >= param2_threshold) {
    return(unique(c(excluded_colonies, colony)))
  } else {
    return(-1)
  }
}
