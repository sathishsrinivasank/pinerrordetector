#' Get bottom line indices of a given plateformat
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#'
#' @return The numeric vector with bottom line indices of  a given
#' \code{plateformat}
#' @export
#'
#' @examples
#' bottom_line_indices(384)
#' bottom_line_indices(1536)
bottom_line_indices <- function(plateformat)
{
  p_nrow <- plate_nrow(plateformat)

  x <- seq((2*p_nrow), (plateformat - p_nrow), p_nrow)

  return(sort(x))
}
