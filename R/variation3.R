# Variation: 3
# variation3(colony, combin_coords, neighbors_selected_colony, excluded_colonies, colony_area_raw_data, plate_median_threshold, empty_spots_1536, neighbor_length_threshold, parameter1)
variation3 <- function(colony,
                       combin_coords,
                       neighbors_selected_colony,
                       excluded_colonies,
                       colony_area_raw_data,
                       plate_median_threshold,
                       empty_spots_1536,
                       neighbor_length_threshold)
{
  for(combin_coords_index in 1:ncol(combin_coords)){
    combinations <- combin_coords[,combin_coords_index]

    # get neighbor indices
    combinations <- neighbors_selected_colony[combinations]

    # remove indexed empty spots
    combinations <- combinations[which(! combinations %in% empty_spots_1536)]

    if(length(combinations) == 0){
      next
    }

    # Apply parameter 1
    excluded_colonies_param1 <- parameter1(colony,
                                           combinations,
                                           colony_area_raw_data,
                                           plate_median_threshold,
                                           excluded_colonies,
                                           neighbor_length_threshold)
    if(excluded_colonies_param1[1] != -1){
      excluded_colonies <- excluded_colonies_param1

      break()
    }
  }
  return(excluded_colonies)
}
