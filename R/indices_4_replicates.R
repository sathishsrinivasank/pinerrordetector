#' Indices of four replicates from large to small plateformat having across or
#' down configuration
#'
#' @param plate_from An integer of large plateformat
#' @param plate_to An integer of small plateformat
#' @param out_data_flow A string which tells the function to output the
#' converted data to a plate configuration which can be down or across
#'
#' @return dataframe with indices of four replicates converted from large to
#' small plateformat with the configuration specified in \code{out_data_flow}
#'
#' @export
#'
#' @examples
#' indices_384_96_across <- indices_4_replicates(plate_from    = 384,
#'                                               plate_to      = 96,
#'                                               out_data_flow = 'across')
#' head(indices_384_96_across)
#'
#' indices_384_96_down <- indices_4_replicates(plate_from    = 384,
#'                                             plate_to      = 96,
#'                                             out_data_flow = 'down')
#'
indices_4_replicates <- function(plate_from,
                                 plate_to,
                                 out_data_flow)
{
  # 1. initiate the data frame to be returned
  data_to <- data.frame(matrix(NA, nrow = plate_to, ncol = 4),
                        stringsAsFactors = FALSE)
  names(data_to) <- c('X1', 'X2', 'X3', 'X4')

  # 2. get plate_from dimensions for the given out_data_flow
  if(out_data_flow == 'down'){
    plate_from_row <- plate_ncol(plate_from)
    plate_from_col <- plate_nrow(plate_from)
  } else if(out_data_flow == 'across'){
    plate_from_row <- plate_nrow(plate_from)
    plate_from_col <- plate_ncol(plate_from)
  }

  # 3. construct indices using plate_from indices (384) to a dimension of
  # plate_to (96)
  x <- 1:plate_from
  j <- 1
  k <- 1
  counter <- 1
  while (k <= plate_from_row){
    b1 <- x[j:(k * plate_from_col)]
    j <- (k * plate_from_col) + 1

    k <- k + 1
    b2 <- x[j:(k * plate_from_col)]
    j <- (k * plate_from_col) + 1

    k <- k + 1
    i <- 1

    if(length(b1) != length(b2)){
      stop('The length of two rows b1 & b2 extracted in this loop must be
           equal')
    }

    while(i <= length(b1)){
      data_to[counter, 1:4] <- c(b1[c(i, (i+1))], b2[c(i, (i+1))])
      i <- i + 2
      counter <- counter + 1
    }
  }

  return(data_to)
}
