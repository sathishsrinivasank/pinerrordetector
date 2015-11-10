# @method middle.neighbors(plate, n, plate.nrow) get coordinates of neighbouring colonies of n (only 8 neighbors in the middle)
# @param int n selected colony in the middle of the plate (only 8 neighbors)
# @param plate int type of plate format (Eg: 1536)
# @param function plate.nrow(plate) get maximum number of rows in a plate
# @return numeric vector  x with colony indices that are neighbors of a selected colony
middle.neighbors <- function(plate, n)
{
  if (n %in% not.middle.indices(plate) || n < 1) {
    stop(paste('The value of n must not be less than 1 or it must not be ',
               'one returned by not.middle.indices(', plate, ')', sep=''))
  } else {
    x <- c(n + 1,
           n - 1,
           n + plate.nrow(plate) - 1,
           n + plate.nrow(plate),
           n + plate.nrow(plate) + 1,
           n - plate.nrow(plate) + 1,
           n - plate.nrow(plate),
           n - plate.nrow(plate) - 1)

    return(sort(x))
  }
}
