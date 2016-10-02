#' Number of rows for a given plateformat
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#'
#' @return An integer with the number of rows for a given \code{plateformat}
#' @export
#'
#' @examples
#' plate_nrow(1536)
#' plate_nrow(96)
plate_nrow <- function(plateformat)
{
	plateformat <- as.character(plateformat)

	if (! plateformat %in% c('96', '384', '1536', '6144')) {
	  stop(paste('The only supported plate formats are 96, 384, 1536, 6144',
	             sep = ''))
	} else {
	  return(switch(plateformat,
	                "96"   = 8,
	                "384"  = 16,
	                "1536" = 32,
	                "6144" = 64))
	}
}
