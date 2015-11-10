# @method convert.1536.to.384.indices()
# @return matrix Raw Indices from 1536 (32,48) format to 384 (16,24) format with 4 replicates;
convert.1536.to.384.indices <- function()
{
  # @var m int total rows of 1536 plate
  # @var n int total columns of 1536 plate
  m <- 32
  n <- 48
  x <- c()
  index_384 <- matrix(nrow = 384, ncol = 4)
  j <- 0
  l <- 0
  p <- 0

  while (m > j) {
    for (i in 0:(n - 1)) {
      for (k in ((j + 1):(j + 2))) {
        x <- c(x,(i * m) + k)
        l <- l + 1
      }
      if (l == 4) {
        p <- p + 1
        index_384[p,] <- x
        x <- c()
        l <- 0
      }
    }
    j <- j + 2
  }
  return(index_384)
}
