# @method plate.nrow(plate) get number of rows in a plate
# @param int plate type of plate format (Eg: 1536)
# @return int
plate.nrow <- function(plate)
{
	plate <- as.character(plate)

	if (! plate %in% c('96', '384', '1536')) {
	  stop(paste('The only supported plate formats are 96, 384, 1536', sep = ''))
	} else {
	  return(switch(plate,
	                "96" = 8,
	                "384" = 16,
	                "1536" = 32))
	}
}
