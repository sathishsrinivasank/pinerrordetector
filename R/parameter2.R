# parameter2(colony, combinations, excluded_colonies, param2_threshold)
parameter2 <- function(colony,
                       combinations,
                       excluded_colonies,
                       param2_threshold)
{
  if (length(which(excluded_colonies %in% combinations)) >= param2_threshold) {
    return(unique(c(excluded_colonies, colony)))
  } else {
    return(-1)
  }
}
