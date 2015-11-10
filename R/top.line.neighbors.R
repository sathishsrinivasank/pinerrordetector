# @method top.line.neighbors(plate, n, plate.nrow) get coordinates of neighbouring colonies of n (only 5 neighbors in the top boundary)
# @param int n coordinate of one of colonies at the 4 boundaries of the plate (both 3 & 5 neighbors)
# @param plate int type of plate format (Eg: 1536)
# @param function plate.nrow(plate) get maximum number of rows in a plate
# @return numeric x
top.line.neighbors <- function(plate, n)
{
  if (n %in% top.line.indices(plate)) {
    x <- c(n + 1,
           n - plate.nrow(plate) + 1,
           n - plate.nrow(plate),
           n + plate.nrow(plate),
           n + plate.nrow(plate) + 1)

    return(sort(x))
  } else {
    stop(paste('The value of n should be one returned by top.line.indices(',
               plate, ')', sep=''))
  }
}
