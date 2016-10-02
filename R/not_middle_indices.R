#' Colony indices at the periphery of a given plateformat
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#'
#' @return numeric vector of colony indices at the periphery of the
#' \code{plateformat}
#' @export
#'
#' @examples
#' not_middle_indices(1536)
not_middle_indices <-  function(plateformat)
{
  return(sort(c((right_line_indices(plateformat)),
                (left_line_indices(plateformat)),
                (top_line_indices(plateformat)),
                (bottom_line_indices(plateformat)),
                1,
                (plate_nrow(plateformat)),
                (plateformat - (plate_nrow(plateformat)) + 1),
                plateformat)))
}
