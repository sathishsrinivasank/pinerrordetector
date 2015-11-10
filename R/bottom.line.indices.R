# @method bottom.line.indices(plate, plate.nrow) get coordinates of all 5 neighbors colonies in the bottom boundary excluding 3 neighbors
# @param plate int type of plate format (Eg: 1536)
# @param function plate.nrow(plate) get maximum number of rows in a plate
# @return numeric x
bottom.line.indices <- function(plate)
{
  plate_nrow <- plate.nrow(plate)

  n <- plate_nrow + plate_nrow

  x <- seq(n, (plate - plate_nrow), plate_nrow)

  return(sort(x))
}
