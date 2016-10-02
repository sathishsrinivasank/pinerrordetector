#' Get indices of three neighbors for the top left corner index of a plate
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#'
#' @return numeric vector of three neighbors for a given \code{plateformat}
#' @export
#'
#' @examples
#' topleft_corner_neighbors(96)
#' topleft_corner_neighbors(1536)
topleft_corner_neighbors <- function(plateformat)
{
  n <- 1

  p_nrow <- plate_nrow(plateformat)

  x <- c(n + 1,
         n + p_nrow,
         n + p_nrow + 1)

  return(sort(x))
}
