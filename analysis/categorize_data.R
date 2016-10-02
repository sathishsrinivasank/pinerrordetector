categorize_data <- function(data_area, empty_indices)
{
  if(missing(data_area) || !is.numeric(data_area)){
    stop('data_area must be specified as numeric')
  }

  if(missing(empty_indices) || !is.numeric(empty_indices)){
    stop('empty_indices must be specified as numeric')
  }

  plate_median <- median(data_area[-empty_indices],  na.rm = TRUE)

  zero <- which(data_area <= 0)

  less <- which(data_area <= plate_median)

  more <- which(data_area > plate_median)

  less25 <- which(data_area <= ((plate_median*25)/100))

  more90 <- which(data_area >= (((plate_median*90)/100) + plate_median))

  #pinning error
  pe <- zero[which(! zero %in% empty_indices)]

  # less than 25% plate median
  lessig <- less25[which(! less25 %in% zero)]

  # less than plate median
  less <- less[which(! less %in% less25)]

  # more than 90% plate median
  moresig <- more90

  # more than plate median
  more <- more[which(! more %in% more90)]

  data_area[empty_indices] <- 'empty'
  data_area[pe] <- 'pe'
  data_area[lessig] <- 'lessig'
  data_area[less] <- 'less'
  data_area[moresig] <- 'moresig'
  data_area[more] <- 'more'

  return(data_area)
}
