# @method topright.corner.neighbors(plate, plate.nrow) get coordinates of neighbouring colonies in the top right corner
# @param plate int type of plate format (Eg: 1536)
# @param function plate.nrow(plate) get maximum number of rows in a plate
# @return numeric x
topright.corner.neighbors <- function(plate)
{
  n = plate - plate.nrow(plate) + 1

  x = c(n + 1,
        n - plate.nrow(plate),
        n - plate.nrow(plate) + 1)

  return(sort(x))
}
