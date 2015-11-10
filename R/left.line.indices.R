# @method left.line.indices(plate, plate.nrow) get coordinates of all 5 neighbors colonies in the left boundary excluding 3 neighbors
# @param plate int type of plate format (Eg: 1536)
# @param function plate.nrow(plate) get maximum number of rows in a plate
# @return numeric x
left.line.indices <- function(plate)
{
  plate_nrow <- plate.nrow(plate)

  n <- 2

  x <- seq(n, (plate_nrow - 1), 1)

  return(sort(x))
}
