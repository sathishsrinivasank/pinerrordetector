#' Check plate configuration \code{in_data_flow} is specified appropriately
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536 or 6144
#' @param row_coords vector with row coordinates of a \code{plateformat}
#' @param in_data_flow A string which tells the function that the given
#' \code{plateformat} is having the specified plate configuration which can have
#' values either down or across.
#'
#' @return logical \code{TRUE}, if \code{row_coords} is properly specified as
#' given in \code{in_data_flow}
#' @export
#'
#' @examples
#' plateformat <- 384
#' p_nrow <- plate_nrow(plateformat)
#' p_ncol <- plate_ncol(plateformat)
#'
#' p_letters <- plate_letters()[1:p_nrow]
#'
#' check_data_flow(plateformat  = 384,
#'                 row_coords   = rep(p_letters, p_ncol),
#'                 in_data_flow = 'down')
#'
#' check_data_flow(plateformat  = 384,
#'                 row_coords   = rep(p_letters, p_ncol),
#'                 in_data_flow = 'across')
check_data_flow <- function(plateformat, row_coords, in_data_flow){
  p_nrow <- plate_nrow(plateformat)

  if(in_data_flow == 'down'){
    l_check <- length(unique(row_coords[1:p_nrow]))
    if(l_check == 1){
      stop('The value of in_data_flow must be specified as "across"')
    } else if(l_check == p_nrow){
      return(TRUE)
    } else {
      stop('There is a problem with row_coords values, check it')
    }
  } else if(in_data_flow == 'across'){
    l_check <- length(unique(row_coords[1:p_nrow]))
    if(l_check == p_nrow){
      stop('The value of in_data_flow must be specified as "down"')
    } else if(l_check == 1){
      return(TRUE)
    } else {
      stop('There is a problem with row_coords values, check it')
    }
  } else {
    stop('The argument in_data_flow must have value either down or across')
  }
}
