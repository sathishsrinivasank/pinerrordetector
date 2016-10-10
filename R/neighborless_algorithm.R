#' Neighborless Algorithm
#'
#' Neighborless algorithm detects the pinning error of a plate by first excluding
#' the empty indices and then checks for colony area. If the colony area is
#' zero, then the algorithm concludes it as one of excluded colonies.
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536.
#' @param colony_area_raw_data A numeric vector of raw data representing the area
#' yeast grown at a specific location on a nutrient medium agar plate. The data can
#' be either rowwise or columnwise and its arrangement does not influence the
#' outcome of this algorithm.
#' @param empty_indices A numeric vector of indexed empty locations. It can also
#' be the indices of control colonies grown on a plate, which may be used for
#' normalizing the colony area of a plate
#' @param excluded_colonies A numeric vector of indices that are part of excluded
#' colonies
#' @param is_save logical. If \code{TRUE}, then this function will save the
#' list of excluded colonies obtained from neighborless algorithm as .RDS format
#' in the specified \code{excluded_file}.
#' @param excluded_file If \code{is_save} is \code{TRUE}, then the file name
#' with full path for saving the list of excluded colonies must be specified.
#'
#' @return Invisible numeric vector of excluded colonies from neighborless
#' algorithm.
#' @export
#'
#' @examples
#' plateformat <- 1536
#' data_area <- simulated_data_1536(data_384 = colonyarea$data_subtypes,
#'                                  in_data_flow = "across",
#'                                  out_data_flow = "down",
#'                                  is_plate_coords = TRUE)
#' empty_indices <- which(convert_small_to_large(plate_from = 384,
#'                                               plate_to = 1536,
#'                                               data_from = colonyarea$data_subtypes,
#'                                               in_data_flow = 'across',
#'                                               out_data_flow = "down",
#'                                               is_plate_coords = FALSE)$y %in% 'Empty')
#' (neighborless_algorithm(plateformat          = 1536,
#'                         colony_area_raw_data = data_area$y,
#'                         empty_indices        = empty_indices,
#'                         excluded_colonies    = c(),
#'                         is_save              = FALSE,
#'                         excluded_file        = NULL))
#'
neighborless_algorithm <- function(plateformat,
                                   colony_area_raw_data,
                                   empty_indices,
                                   excluded_colonies,
                                   is_save = FALSE,
                                   excluded_file = NULL)
{
  # 1. Apply neighborless algorithm to colony area raw data
  all_indices <- 1:plateformat

  for(colony in all_indices){
    if(! colony %in% empty_indices){
      if(colony_area_raw_data[colony] == 0){
        excluded_colonies <- c(excluded_colonies, colony)
      }
    }
  }

  # 2. save excluded colonies as RData file
  if(is_save){
    excluded_colonies_I <- list()

    excluded_colonies_I[[1]] <- list(excluded_colonies)

    names(excluded_colonies_I)[[1]] <- 'excluded_colonies_I'

    saveRDS(excluded_colonies_I,
            file = excluded_file,
            ascii = FALSE,
            compress = 'xz')
  }

  # 3. return excluded colonies
  invisible(excluded_colonies)
}
