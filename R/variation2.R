# Variation: 2
# variation2(colony, combin_coords, neighbors_selected_colony,
#            excluded_colonies, colony_area_raw_data, plate_median_threshold,
#            neighbor_length_threshold, param2_threshold)

variation2 <- function(colony,
                       combin_coords,
                       neighbors_selected_colony,
                       excluded_colonies,
                       colony_area_raw_data,
                       plate_median_threshold,
                       neighbor_length_threshold,
                       param2_threshold)
{
  for(combin_coords_index in 1:ncol(combin_coords)){
    combinations <- combin_coords[,combin_coords_index]

    # get neighbor indices
    combinations <- neighbors_selected_colony[combinations]

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

    # Apply parameter 2
    excluded_colonies_param2 <- parameter2(colony,
                                           combinations,
                                           excluded_colonies,
                                           param2_threshold)

    if(excluded_colonies_param2[1] != -1){
      excluded_colonies <- excluded_colonies_param2

      break()
    }
  }
  return(excluded_colonies)
}
