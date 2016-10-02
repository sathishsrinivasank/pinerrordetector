#' Simulated data based on data subtypes from 384 to 1536 plateformat
#'
#' Uniformly distributed data with median of 600 is simulated and returned
#' for  1536 plateformat configuration from 384 plateformat configuration.
#'
#' @param data_subtypes_384 A character vector with data subtypes of length 384
#' @param out_data_flow A string which tells the function to output the
#' converted data to a plate configuration which can be down or across
#' @param is_plate_coords logical returns plate coordinates, if its value is
#' \code{TRUE}
#'
#' @return A numeric vector with uniformly distributed data having median of 600
#' @export
#'
#' @examples
#' simulated_data_1536(colonyarea$data_subtypes)
simulated_data_1536 <- function(data_subtypes_384, out_data_flow = "across", is_plate_coords = TRUE)
{
  # check the plate median is 600
  set.seed(31118)

  unif_dis_data_1536 <- c(rep(runif(334, 100, 1200), 4), rep(0, 132))

  plate_median <- ceiling(median(unif_dis_data_1536))

  if(plate_median != 600){
    stop('The plate median used to create template data must be 600')
  }

  # generate simulated data
  set.seed(31118)

  unif_dis_data_384 <- c(runif(334, 100, 1200), rep(0, 33))

  pinning_error <- unif_dis_data_384[which(unif_dis_data_384 %in% 0)]

  less <- unif_dis_data_384[which(unif_dis_data_384 < plate_median)]
  less <- less[which(! less < (plate_median/4) )]

  lessig <- unif_dis_data_384[which(unif_dis_data_384 < (plate_median/4))]
  lessig <- lessig[which(lessig > 0)]

  more <- unif_dis_data_384[which(unif_dis_data_384 > plate_median)]
  more <- more[which(! more > (2*plate_median*0.9) )]

  moresig <- unif_dis_data_384[which(unif_dis_data_384 > (2*plate_median*0.9))]

  # assign data to the template
  data_384 <- data_subtypes_384
  data_384[which(data_384 %in% 'Lessthan Plate Median')] <- less
  data_384[which(data_384 %in% 'Lessthan 25% Plate Median')] <- lessig
  data_384[which(data_384 %in% 'Morethan Plate Median')] <- more
  data_384[which(data_384 %in% 'Morethan 90% Plate Median')] <- moresig
  data_384[which(data_384 %in% 'Pinning Error')] <- pinning_error
  data_384[which(data_384 %in% 'Empty')] <- 0

  return(convert_small_to_large(384, 1536, data_384, out_data_flow, is_plate_coords))
}
