#' Convert data into rowwise(across) or coloumnwise(down) within a plate format
#'
#' Convert data into rowwise(across) or coloumnwise(down) within a plate format
#'
#' Make sure the length of the given data in \code{data_from} argument matches
#' the value in \code{plateformat}
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536 or 6144
#' @param data_from data vector required for conversion
#' @param is_plate_coords logical returns plate coordinates, if its \code{TRUE}
#' @param in_data_flow A string indicating the format of the given data in
#' \code{data_from}. It can take a value of either down or across.
#' @param out_data_flow A string indicating the function to output the data to
#' a desired format. It can take a value either of down or across.
#'
#' @return dataframe of converted data from across to down or vice versa.
#'
#' @export
#'
#' @examples
#' legend_txt_bg_col <- c('Empty'                     = 'red',
#'                        'Pinning Error'             = 'black',
#'                        'Morethan Plate Median'     = '#660066',
#'                        'Lessthan Plate Median'     = 'green',
#'                        'Morethan 90% Plate Median' = 'cyan',
#'                        'Lessthan 25% Plate Median' = 'yellow',
#'                        'Excluded Colonies'         = 'blue')
#' plateformat <- 384
#' across_384 <- convert_down_across(plateformat = plateformat,
#'                                   data_from = colonyarea$data_subtypes,
#'                                   is_plate_coords = TRUE,
#'                                   in_data_flow = 'across',
#'                                   out_data_flow = 'across')
#'
#' plot_platemap(plateformat = plateformat,
#'               plot_data = across_384,
#'               legend_txt_bg_col = legend_txt_bg_col)
#'
#' # Note the data structure is not disturbed after converting
#' # rowwise(across) to columnwise(down). Also, try in_data_flow = 'down'
#'
#' across_down_384 <- convert_down_across(plateformat = plateformat,
#'                                        data_from = colonyarea$data_subtypes,
#'                                        is_plate_coords = TRUE,
#'                                        in_data_flow = 'across',
#'                                        out_data_flow = 'down')
#'
#' plot_platemap(plateformat = plateformat,
#'               plot_data = across_down_384,
#'               legend_txt_bg_col = legend_txt_bg_col)
#'
convert_down_across <- function(plateformat,
                                data_from,
                                is_plate_coords = TRUE,
                                in_data_flow,
                                out_data_flow)
{
  stopifnot(is.numeric(plateformat) && length(plateformat) == 1)
  stopifnot((is.vector(data_from) && length(data_from) == plateformat) ||
             (is.data.frame(data_from) && nrow(data_from) == plateformat))
  stopifnot(length(in_data_flow) == 1 || length(out_data_flow) == 1)
  stopifnot(in_data_flow %in% c('down', 'across'))
  stopifnot(out_data_flow %in% c('down', 'across'))
  stopifnot(is.logical(is_plate_coords))

  df1 <- plate_coords(plate_to = plateformat,
                      data_from = data_from,
                      data_format = in_data_flow)

  if(in_data_flow != out_data_flow){
    if(out_data_flow == 'across'){
      df1$row <- factor(df1$row, levels = unique(df1$row), ordered = TRUE)
      df1 <- df1[with(df1, order(row, decreasing = FALSE)), ]
      df1$row <- as.character(df1$row)

    } else if(out_data_flow == 'down'){
      df1 <- df1[with(df1, order(column, decreasing = FALSE)), ]
    }
  }

  if(!is_plate_coords){
    df1$row <- NULL
    df1$column <- NULL
  }

  return(df1)
}
