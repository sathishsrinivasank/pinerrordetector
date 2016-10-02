#' Get left line indices of a given plateformat
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#'
#' @return The numeric vector with left line indices of  a given
#' \code{plateformat}
#' @export
#'
#' @examples
#' left_line_indices(384)
#' left_line_indices(1536)
left_line_indices <- function(plateformat)
{
  p_nrow <- plate_nrow(plateformat)

  n <- 2

  x <- seq(n, (p_nrow - 1), 1)

  return(sort(x))
}
