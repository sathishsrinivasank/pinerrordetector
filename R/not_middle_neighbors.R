#' Neighbors adjoining the selected colony from the periphery of the given
#' plateformat
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#' @param colony An integer which can be one from
#' \code{not_middle_indices(plateformat)}
#'
#' @return A numeric vector with neighbors adjoining the given \code{colony}
#' @export
#'
#' @examples
#' not_middle_neighbors(96, 1)
#' #' not_middle_neighbors(1536, 1)
#'
not_middle_neighbors <- function(plateformat, colony)
{
  p_nrow <- plate_nrow(plateformat)

  # Top left corner: Eg: 1
  if (colony == 1) {
    neighbors_selected_colony <- topleft_corner_neighbors(plateformat)
    # bottom left corner: Eg: 32
  } else if (colony == p_nrow) {
    neighbors_selected_colony <- bottomleft_corner_neighbors(plateformat)
    # top right corner: Eg: 1505
  } else if (colony == (plateformat - p_nrow + 1)) {
    neighbors_selected_colony <- topright_corner_neighbors(plateformat)
    # bottom right corner: Eg: 1536
  } else if (colony == plateformat) {
    neighbors_selected_colony <- bottomright_corner_neighbors(plateformat)
    # top line  neigbours
  } else if (colony %in% top_line_indices(plateformat)) {
    neighbors_selected_colony <- top_line_neighbors(plateformat, colony)
    # bottom line  neigbours
  } else if (colony %in% bottom_line_indices(plateformat)) {
    neighbors_selected_colony <- bottom_line_neighbors(plateformat, colony)
    # left line  neigbours
  } else if (colony %in% left_line_indices(plateformat)) {
    neighbors_selected_colony <- left_line_neighbors(plateformat, colony)
    # right line neigbours
  } else if (colony %in% right_line_indices(plateformat)) {
    neighbors_selected_colony <- right_line_neighbors(plateformat, colony)
  } else {
    stop('Enter the colony index from the periphery of the plate which can be
         one from not_middle_indices(plateformat)')
  }
  return(neighbors_selected_colony)
}
