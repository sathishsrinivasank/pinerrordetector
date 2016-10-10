#' Data converted to appropriate format for \code{plot_platemap} function
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#' @param plate_data A dataframe with one column representing the data subtypes
#' of plate or a dataframe with three columns such as first column representing
#' the text characters used to label the y-axis, second column representing the
#' character used to label the x-axis of platemap, third column representing the
#' data subtypes.
#' @param data_flow A character that must be either down or across.
#' 'down' representing data organized as columnwise as shown in
#' \code{data(colonyarea)}. 'across' representing data organized as rowwise. See
#' examples of this function.
#' @param plate_letters A character vector for labelling y-axis of platemap
#'
#' @return A dataframe with three columns such as rowvar representing the text
#' characters used to label the y-axis, columnvar representing the character
#' used to label the x-axis of platemap, colorvar representing the data subtypes.
#' If the \code{data_flow} is across, then this function converts it into 'down'.
#' @export
#'
#' @examples
#' # dataframe with one column is converted to three columns
#' plateformat <- 1536
#' data_subtypes_384 <- colonyarea$data_subtypes
#' data_area <- simulated_data_1536(data_subtypes_384,
#'                                  in_data_flow = "across",
#'                                  out_data_flow = "across",
#'                                  is_plate_coords = FALSE)
#' plate_letters <- c(LETTERS,
#'                    c(t(outer(LETTERS[1], LETTERS[1:6], paste, sep = ''))))
#' p_nrow <- plate_nrow(plateformat)
#' plate_letters <- plate_letters[1:p_nrow]
#' platemap_data(plateformat   = plateformat,
#'               plate_data    = data_area,
#'               data_flow     = 'down',
#'               plate_letters = plate_letters)
#'

platemap_data <- function(plateformat, plate_data, data_flow, plate_letters)
{
  # plateformat must be an integer
  # x must be a vector indicating data subtypes

  if(! as.character(plateformat) %in% c('96', '384', '1536')){
    stop('The supported plateformats are 96, 384, 1536!')
  }
  # check for number of rows
  if (nrow(plate_data) == plateformat) {

    p_ncol = plate_ncol(plateformat)

    p_nrow = plate_nrow(plateformat)

    # check for number of columns
    if(ncol(plate_data) == 3){
      # convert  plate_data across to down
      if(data_flow == 'across'){
        plate_data_2 <- data.frame(stringsAsFactors = FALSE)

        for(i in (p_ncol-1):0){
          for(j in 1:p_nrow){
            plate_data_2 <- do.call('rbind', list(plate_data_2,
                                                  (plate_data[((j*p_ncol)-i),])))
          }
        }
        plate_data <- plate_data_2
        rm(plate_data_2)
      } else if(data_flow == 'down'){
        plate_data <- plate_data
      }

      if(is.character(plate_data[,1]) && is.numeric(plate_data[,2]) &&
         is.character(plate_data[,3])){
        return(plate_data)
      } else {
        stop('The classes of 3 columns of plate_data must be character, numeric,
             and character respectively. See str(colonyarea)')
      }
    } else if(ncol(plate_data) == 1){
      # convert plate_data across to down
      if(data_flow == 'across'){
        plate_data_2 <- data.frame(stringsAsFactors = FALSE)
        incr <- 1
        for(i in (p_ncol-1):0){
          for(j in 1:p_nrow){
            plate_data_2[incr, 1] <- plate_data[((j*p_ncol)-i),]
            incr <- incr + 1
          }
        }
        plate_data <- plate_data_2
        rm(plate_data_2)
      } else if(data_flow == 'down'){
        plate_data <- plate_data
      }

      if(is.character(plate_data[,1])){

        # create rowvar
        rowvar <- rep(plate_letters, p_ncol)

        # create columnvar
        columnvar <- rep(1:p_ncol, each = p_nrow)

        all_color <- plate_data[, 1]

        # convert rowvar, columnvar, and all_color into dataframe and return
        return(data.frame(rowvar = rowvar,
                          columnvar = columnvar,
                          colorvar = all_color,
                          stringsAsFactors = FALSE))
      }else {
        stop('The class of column 1 of plate_data must be a character')
      }
    } else {
      stop('If plate coordinates are unknown, then plate_data must be of one
           column as shown in colonyarea[,3]. If plate coordinates are known,
           then plate_data must have 3 columns as shown in data(colonyarea)')
    }
  } else {
    stop(paste('The total rows of plate_data must be equal to plateformat: ',
               plateformat, sep = ''))
  }
}
