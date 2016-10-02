#' Get top line indices of a given plateformat
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#'
#' @return The numeric vector with top line indices of  a given
#' \code{plateformat}
#' @export
#'
#' @examples
#' top_line_indices(384)
#' top_line_indices(1536)
top_line_indices <- function(plateformat)
{
  p_nrow <- plate_nrow(plateformat)

  n <- p_nrow + 1

  x <- seq(n, (plateformat - p_nrow + 1), p_nrow)

  return(sort(x))
}
