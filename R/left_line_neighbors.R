#' Get indices of five neighbors for an index at the left line of the plate
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#' @param n An integer which can be one of \code{left_line_indices(plateformat)}
#'
#' @return numeric vector of five neighbors for the given \code{plateformat} and
#' \code{n}
#' @export
#'
#' @examples
#' left_line_neighbors(384, 2)
#' left_line_neighbors(1536, 2)
left_line_neighbors <- function(plateformat, n)
{
  if (n %in% left_line_indices(plateformat)) {
    p_nrow <- plate_nrow(plateformat)

    x <- c(n - 1,
           n + 1,
           n + p_nrow - 1,
           n + p_nrow,
           n + p_nrow + 1)

    return(sort(x))
  } else {
    stop(paste('The value of n should be one returned by left_line_indices(',
               plateformat, ')', sep=''))
  }
}
