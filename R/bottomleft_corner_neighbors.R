#' Get indices of three neighbors for the bottom left corner index of a plate
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#'
#' @return numeric vector of three neighbors for a given \code{plateformat}
#' @export
#'
#' @examples
#' bottomleft_corner_neighbors(96)
#' bottomleft_corner_neighbors(1536)
bottomleft_corner_neighbors <- function(plateformat)
{
  p_nrow <- plate_nrow(plateformat)

  x <- c(p_nrow - 1,
         p_nrow + p_nrow,
         p_nrow + p_nrow - 1)

  return(sort(x))
}
