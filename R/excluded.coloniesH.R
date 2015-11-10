excluded.coloniesH <- function(plate,
                               colony_indices,
                               colony_area_raw_data,
                               empty_spots_1536,
                               excluded_colonies,
                               plate_median_threshold,
                               neighbor_length_threshold,
                               is_middle)
{
  #pick a colony
  for (colony in colony_indices) {
    if(colony_area_raw_data[colony, 1] <= plate_median_threshold){
      if(! colony %in% empty_spots_1536){

        #get neighbors for selected colony
        if(is_middle){
          neighbors_selected_colony <- middle.neighbors(plate, colony)
        } else {
          neighbors_selected_colony = not.middle.neighbors(plate, colony)
        }

        #generate combinations of ncoordinates by sampling neighbor_length_threshold out of maximum kernel density
        max_kernel_density <- length(neighbors_selected_colony)

        if(max_kernel_density >= neighbor_length_threshold) {
          combin_coords <- combn(max_kernel_density, neighbor_length_threshold)

          #apply variation-4
          excluded_colonies <- variation4(colony,
                                          combin_coords,
                                          neighbors_selected_colony,
                                          excluded_colonies,
                                          colony_area_raw_data,
                                          plate_median_threshold,
                                          neighbor_length_threshold)
        }
      }
    }
  }
  return(excluded_colonies)
}
