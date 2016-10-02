#' Get the right line indices of a given plateformat
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#'
#' @return The numeric vector with right line indices of  a given
#' \code{plateformat}
#' @export
#'
#' @examples
#' right_line_indices(384)
#' right_line_indices(1536)
right_line_indices <- function(plateformat)
{
  p_nrow <- plate_nrow(plateformat)

  n <- plateformat - p_nrow + 2

  x <- seq(n, (plateformat - 1), 1)

  return(sort(x))
}
