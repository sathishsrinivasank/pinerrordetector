#' Number of columns for a given plateformat
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#'
#' @return An integer with the number of columns for a given \code{plateformat}
#' @export
#'
#' @examples
#' plate_ncol(1536)
#' plate_ncol(384)
plate_ncol <- function(plateformat)
{
  plateformat <- as.character(plateformat)

  if (! plateformat %in% c('96', '384', '1536', '6144')) {
    stop(paste('The only supported plate formats are 96, 384, 1536, 6144',
               sep = ''))
  } else {
    return(switch(plateformat,
                  "96"   = 12,
                  "384"  = 24,
                  "1536" = 48,
                  "6144" = 96))
  }
}
