# @method plate.median(plate_index, bioneer_samples, convert.1536.to.384.indices, get.384.emptyspot.index, empty.spots.1536, colony_area_raw_data) compute plate median by excluding known empty spots - 1536 plate format
# @param int plate_index suffix of file names indicating plate number
# @param list empty_spots_1536
# @param dataframe colony_area_raw_data obtained from raw data file
# @return plate_median for one plate
plate.median <- function(plate_index, empty_spots_1536, colony_area_raw_data)
{
  # first column of colony_area_raw_data: colony area
  # second column of colony_area_raw_data: circularity
  # raw2median: data frame of raw colony area for all except empty spots

  raw2median <- colony_area_raw_data[-(empty_spots_1536[[plate_index]]), 1, drop = FALSE]

  return(median(raw2median[,1]))
}
