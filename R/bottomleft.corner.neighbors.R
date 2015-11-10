# @method bottomleft.corner.neighbors(plate, plate.nrow) get coordinates of neighbouring colonies in the bottom left corner
# @param plate int type of plate format (Eg: 1536)
# @param function plate.nrow(plate) get maximum number of rows in a plate
# @return numeric x

bottomleft.corner.neighbors <- function(plate)
{
  n <- plate.nrow(plate)

  x <- c(n - 1,
         n + plate.nrow(plate),
         n + plate.nrow(plate) - 1)

  return(sort(x))
}
