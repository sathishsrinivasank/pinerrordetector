#' Get Plate letters for row coordinates of a plate
#'
#' @return character vector of row coordinates for a plate
#'
#' @examples
#' plate_letters()[1:plate_nrow(1536)]
#' plate_letters()[1:plate_nrow(6144)]
plate_letters <- function(){
 return(c(LETTERS,
          c(t(outer(LETTERS[1], LETTERS, paste, sep = ''))),
          c(t(outer(LETTERS[2], LETTERS, paste, sep = ''))),
          c(t(outer(LETTERS[3], LETTERS, paste, sep = '')))))
}
