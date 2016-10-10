#' Neighbors adjoining the colony index from middle of the plate
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#' @param n A colony index from the middle of the plate. If \code{n} is not from
#' the middle of the plate, an exception is thrown.
#'
#' @return numeric vector  of colony indices that are neighbors adjoining the
#' selected colony \code{n}
#' @export
#'
#' @examples
#' plateformat <- 1536
#' all_indices <- 1:plateformat
#' middle_colony_indices <- all_indices[-(not_middle_indices(plateformat))]
#' middle_colony <- middle_colony_indices[1]
#' middle_neighbors(plateformat, middle_colony)
middle_neighbors <- function(plateformat, n)
{
  if (n %in% not_middle_indices(plateformat) || n < 1) {
    stop(paste('The value of n must not be less than 1 or it must not be ',
               'one returned by not_middle_indices(', plateformat, ')', sep=''))
  } else {
    p_nrow <- plate_nrow(plateformat)

    x <- c(n + 1,
           n - 1,
           n + p_nrow - 1,
           n + p_nrow,
           n + p_nrow + 1,
           n - p_nrow + 1,
           n - p_nrow,
           n - p_nrow - 1)

    return(sort(x))
  }
}
