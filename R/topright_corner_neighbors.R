#' Get indices of three neighbors for the top right corner index of a plate
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#'
#' @return numeric vector of three neighbors for a given \code{plateformat}
#' @export
#'
#' @examples
#' topright_corner_neighbors(96)
#' topright_corner_neighbors(1536)
topright_corner_neighbors <- function(plateformat)
{
  p_nrow <- plate_nrow(plateformat)

  n <- plateformat - p_nrow + 1

  x <- c(n + 1,
         n - p_nrow,
         n - p_nrow + 1)

  return(sort(x))
}
