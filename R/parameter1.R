# parameter1(colony, combinations, colony_area_raw_data, plate_median_threshold,
#            excluded_colonies, neighbor_length_threshold)

parameter1 <- function(colony,
                       combinations,
                       colony_area_raw_data,
                       plate_median_threshold,
                       excluded_colonies,
                       neighbor_length_threshold)
{
  if (length(which(colony_area_raw_data[combinations, 1] <=
                   plate_median_threshold)) >= neighbor_length_threshold) {

    return(unique(c(excluded_colonies, colony)))

  } else {
    return(-1)
  }
}
