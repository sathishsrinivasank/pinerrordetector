convert.384.to.1536.data <- function(data_384)
{
  data_1536 <- matrix(nrow = 1536, ncol = 1)

  indices_1536_to_384 <- convert.1536.to.384.indices()

  for(index_384 in 1:384){
    data_1536[indices_1536_to_384[index_384,],] <- data_384[index_384]
  }

  return(data_1536)
}
