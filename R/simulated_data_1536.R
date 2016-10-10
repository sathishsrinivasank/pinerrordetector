#' Simulated data based on data subtypes from 384 to 1536 plateformat
#'
#' Uniformly distributed data with median of 600 is simulated and returned
#' for  1536 plateformat configuration from 384 plateformat configuration.
#' The data arrangement will be based on \code{data_384}.
#'
#' @param data_384 A character vector with data subtypes of length 384.
#' See \code{example(simulated_data_1536)}
#' @param in_data_flow A string indicating the plate format of
#' \code{data_384}. It can take a value either across or down.
#' @param out_data_flow A string which tells the function to output the
#' converted data to a plate configuration which can be down or across
#' @param is_plate_coords logical returns plate coordinates, if its value is
#' \code{TRUE}
#'
#' @return A numeric vector with uniformly distributed data having median of 600
#' @export
#'
#' @examples
#' simulated_data_1536(data_384 = colonyarea$data_subtypes,
#'                     in_data_flow = "across",
#'                     out_data_flow = "down",
#'                     is_plate_coords = FALSE)
#'
simulated_data_1536 <- function(data_384,
                                in_data_flow = "across",
                                out_data_flow = "down",
                                is_plate_coords = FALSE)
{
  # 1. sanity checks
  stopifnot(is.vector(data_384) || is.data.frame(data_384))
  stopifnot(length(data_384) == 384 ||
            (nrow(data_384) == 384 && length(data_384) == 1))

  # 2. check the plate median is ~ 600
  set.seed(31118)
  unif_dis_data_1536 <- ceiling(c(rep(runif(334, 100, 1200), 4), rep(0, 132)))
  p_median <- plate_median(empty_indices = NULL, colony_area_raw_data = unif_dis_data_1536)
  if(p_median != 600){
    stop('The plate median in the simulated data must be 600')
  }

  # 3. generate simulated data
  set.seed(31118)
  unif_dis_data_384 <- ceiling(c(runif(334, 100, 1200), rep(0, 33)))
  pinning_error <- unif_dis_data_384[unif_dis_data_384 <= 0]
  lessig <- unif_dis_data_384[(unif_dis_data_384 < (p_median/4)) & (unif_dis_data_384 > 0)]
  less <- unif_dis_data_384[(unif_dis_data_384 <= p_median) & (unif_dis_data_384 >= (p_median/4))]
  moresig <- unif_dis_data_384[unif_dis_data_384 > (2*p_median*0.9)]
  more <- unif_dis_data_384[(unif_dis_data_384 > p_median) & (unif_dis_data_384 <= (2*p_median*0.9))]

  # 4. assign simulated data to template data
  if(is.vector(data_384)){
    data_384 <- data.frame(y = data_384, stringsAsFactors = FALSE)
  }

  colnames(data_384) <- 'y'

  data_384[data_384 == 'Lessthan Plate Median', 'y'] <- less
  data_384[data_384 == 'Lessthan 25% Plate Median', 'y'] <- lessig
  data_384[data_384 == 'Morethan Plate Median', 'y'] <- more
  data_384[data_384 == 'Morethan 90% Plate Median', 'y'] <- moresig
  data_384[data_384 == 'Pinning Error', 'y'] <- pinning_error
  data_384[data_384 == 'Empty', 'y'] <- 0
  data_384$y <- as.numeric(data_384$y)

  # 5. return data_384 converted into 1536 plate format
  return(convert_small_to_large(plate_from = 384,
                                plate_to = 1536,
                                data_from = data_384,
                                in_data_flow = in_data_flow,
                                out_data_flow = out_data_flow,
                                is_plate_coords = is_plate_coords))
}
