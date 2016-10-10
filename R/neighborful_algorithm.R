#' Neighborful Algorithm
#'
#' Neighborful algorithm is used to detect pinning errors from other types of
#' data in an array of yeast colonies grown on a plate. The other types of data
#' may be empty spots, locations where yeast colonies have growth area less than
#' or greater than the given plate median threshold. All variations of neighborful
#' algorithms A-H are implemented here. The differences between these variations
#' of neighborful algorithm are based on
#' \itemize{
#'  \item{Which exclusion criteria is applied}
#'  \item{Whether \code{empty_indices} are removed in the outer or inner loops of
#'        neighborful algorithm}
#' }
#'
#' Two exclusion criteria are used to consider a colony as excluded colony. The
#' two parameters used in the global exclusion algorithm of Dittmar et al., 2010
#' are "If at least six out of eight neighboring colonies fall below 25% of
#' the plate growth median (parameter 1) or at least two neighboring colonies
#' have already been excluded (parameter 2), the program highlights the colony
#' in red for exclusion". In addition, the excluded colony must have colony area
#' less than the defined plate median threshold.
#'
#' The exclusion criteria-1 of neighborful algorithm represents the number of
#' adjoining neighbors of the selected colony that have colony area less than or
#' equal to threshold value of median colony area of the plate. The threshold
#' value of median colony growth area of the plate is determined as 25\% of
#' plate median, which is an arbitrary number used in the literature. The
#' exclusion criteria-1 can take one of any value of \code{1:8}, because of the
#' possible adjoining neighbors for a selected colony will fall in this range.
#' If the selected colony is surrounded by at least specified number of threshold
#' neighbors, then it will be considered as excluded colony by the neighborful
#' algorithm
#'
#' The exclusion criteria-2 of neighborful algorithm represents the number of
#' adjoining neighbors of the selected colony are excluded by the previous
#' iteration of the neighborful algorithm by using either or both exclusion
#' criteria 1 & 2. The exclusion criteria-2 can take one of any value of
#' \code{1:8}, because of the possible adjoining neighbors for a selected colony
#' will fall in this range. If the selected colony is surrounded by at least
#' specified number of threshold neighbors, then it will be considered as
#' excluded colony by the neighborful algorithm
#'
#' Usually, an experiment may have empty locations where no yeast cells are grown
#' and these locations are called as indexed empty spots. These locations can also
#' have control yeast strains used for normalization of all colonies in that plate.
#' Since pinning error and empty spots have zero growth area, the influence of
#' including or excluding empty spots on the sensivity, specificity, and predictive
#' values of the algorithm for detecting pinning errors may be studied by adding
#' or removing the known empty spots in the outer or inner loops of the algorithm.
#' The outer loop of the algorithm is used to iterate over the list of colonies
#' of a plate. The inner loop of the algorithm is used to iterate over all possible
#' combinations of adjoining neighbors of the selected colony from the outer loop
#' of the algorithm. Then either or both exclusion criteria 1 or 2 are applied
#' for the selected colony and its adjoining neighbors.
#'
#' The differences in the variations of neighborful algorithm are tabulated below.
#' \tabular{rlll}{
#' [, Variation] \tab Selected colony as known empty spot in outer loop
#'  \tab Adjoing neighbor(s) of the selected colony as known empty spot in inner loop
#'    \tab Exclusion criteria applied\cr
#' [, A] \tab Do not remove  \tab Remove        \tab 1 & 2\cr
#' [, B] \tab Do not remove  \tab Do not remove \tab 1 & 2\cr
#' [, C] \tab Do not remove  \tab Do not remove \tab 1\cr
#' [, D] \tab Do not remove  \tab Remove        \tab 1\cr
#' [, E] \tab Remove         \tab Remove        \tab 1 & 2\cr
#' [, F] \tab Remove         \tab Do not remove \tab 1 & 2\cr
#' [, G] \tab Remove         \tab Do not remove \tab 1\cr
#' [, H] \tab Remove         \tab Remove        \tab 1\cr
#' }
#'
#' References: 1. ScreenMill: A freely available software suite for growth measurement,
#'                analysis and visualization of high-throughput screen data
#'                John C Dittmar, Robert JD Reid and Rodney Rothstein
#'                Bioinformatics 2010 11:353
#'                DOI: 10.1186/1471-2105-11-353
#'
#' @param plateformat An integer which can be one of 96 or 384 or 1536
#' @param colony_area_raw_data A numeric vector of raw data representing the area
#' yeast grown at a specific location on a nutrient medium agar plate. This data
#' must be arranged columnwise. See \code{simulated_data_1536(data_384=
#' colonyarea$data_subtypes, in_data_flow = "down", out_data_flow   = "down",
#' is_plate_coords = TRUE)}. If the data is arranged rowwise, then use
#' \code{convert_down_across()} function to convert rowwise data into
#' columnwise data. For more information on this function, read \code{
#' help("convert_down_across")}.
#'
#' @param empty_indices A numeric vector of indexed empty locations. It can also
#' be the indices of control colonies grown on a plate, which may be used for
#' normalizing the colony area of a plate
#' @param excluded_colonies A numeric vector of indices that are part of excluded
#' colonies
#' @param percent_median_thresh The integer representing the percentage used to
#' compute the threshold value of plate median of colony area
#'
#' @param param1_threshold The numerical value which can be one from \code{1:8}.
#' This exclusion criteria-1 of neighborful algorithm represents the number of
#' adjoining neighbors of the selected colony from \code{colony_indices} that
#' has colony area less than or equal to \code{plate_median_threshold}. If the
#' selected colony is surrounded by at least \code{param1_threshold} neighbors,
#' then it will be considered as excluded colony by the neighborful algorithm
#' @param param2_threshold The numerical value which can be one from \code{1:8}.
#' This exclusion criteria-2 of neighborful algorithm represents the number of
#' adjoing neighbors of the selected colony from \code{colony_indices} are
#' excluded by the previous iteration of neighborful algorithm. If the selected
#' colony is surrounded by at least \code{param2_threshold} neighbors, then it
#' will be considered as excluded colony by the neighborful algorithm.
#' @param is_save logical. If \code{TRUE}, then this function will save the
#' list of excluded colonies obtained from all variations of neighborful algorithm
#' as .RDS format in the specified \code{excluded_file}
#' @param excluded_file If \code{is_save} is \code{TRUE}, then the file name
#' with full path for saving the list of excluded colonies must be specified
#'
#' @return Invisible named list of excluded colonies from all variations A-H of
#' neighborful algorithm for the given exclusion criteria 1 or 2.
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
#' (neighborful_algorithm(plateformat           = 1536,
#'                        colony_area_raw_data  = data_area$y,
#'                        empty_indices         = empty_indices,
#'                        excluded_colonies     = c(),
#'                        percent_median_thresh = 25,
#'                        param1_thresh         = 6,
#'                        param2_thresh         = 2,
#'                        is_save               = FALSE,
#'                        excluded_file         = NULL))
neighborful_algorithm <- function(plateformat,
                                  colony_area_raw_data,
                                  empty_indices = NULL,
                                  excluded_colonies = NULL,
                                  percent_median_thresh,
                                  param1_threshold,
                                  param2_threshold,
                                  is_save = FALSE,
                                  excluded_file = NULL)
{
  # sanity checks
  stopifnot(length(plateformat) == 1 && (plateformat %in% c(96, 384, 1536, 6144)))
  stopifnot(is.vector(colony_area_raw_data) &&
              (is.numeric(colony_area_raw_data) || is.integer(colony_area_raw_data)))
  stopifnot(is.null(empty_indices) ||
              (is.numeric(empty_indices) || is.integer(empty_indices)))
  stopifnot(is.null(excluded_colonies) ||
              (is.numeric(excluded_colonies) || is.integer(excluded_colonies)))
  stopifnot((is.numeric(percent_median_thresh) || is.integer(percent_median_thresh)) &&
              length(percent_median_thresh) == 1)
  stopifnot((is.numeric(param1_threshold) || is.integer(param1_threshold)) &&
              length(param1_threshold) == 1)
  stopifnot((is.numeric(param2_threshold) || is.integer(param2_threshold)) &&
              length(param2_threshold) == 1)
  stopifnot(is.logical(is_save) && length(is_save) == 1)
  stopifnot( is.null(excluded_file) ||
              (length(excluded_file) == 1 && is.character(excluded_file)))

  # 1. Apply neighborful algorithm to colony area raw data
  all_indices <- 1:plateformat
  not_middle_colony_indices <- not_middle_indices(plateformat)
  middle_colony_indices <- all_indices[-(not_middle_colony_indices)]

  # middle colonies
  is_middle <- TRUE

  plate_median_threshold <- plate_median(empty_indices, colony_area_raw_data
                                         )/(100/percent_median_thresh)

  mid_excluded_colonies_A <- excluded_coloniesA(plateformat = plateformat,
                                                colony_indices = middle_colony_indices,
                                                colony_area_raw_data = colony_area_raw_data,
                                                empty_indices = empty_indices,
                                                excluded_colonies = excluded_colonies,
                                                plate_median_threshold = plate_median_threshold,
                                                param1_threshold = param1_threshold,
                                                param2_threshold = param2_threshold,
                                                is_middle = is_middle)

  mid_excluded_colonies_B <- excluded_coloniesB(plateformat = plateformat,
                                                colony_indices = middle_colony_indices,
                                                colony_area_raw_data = colony_area_raw_data,
                                                excluded_colonies = excluded_colonies,
                                                plate_median_threshold = plate_median_threshold,
                                                param1_threshold = param1_threshold,
                                                param2_threshold = param2_threshold,
                                                is_middle = is_middle)

  mid_excluded_colonies_C <- excluded_coloniesC(plateformat = plateformat,
                                                colony_indices = middle_colony_indices,
                                                colony_area_raw_data = colony_area_raw_data,
                                                empty_indices = empty_indices,
                                                excluded_colonies = excluded_colonies,
                                                plate_median_threshold = plate_median_threshold,
                                                param1_threshold = param1_threshold,
                                                is_middle = is_middle)

  mid_excluded_colonies_D <- excluded_coloniesD(plateformat = plateformat,
                                                colony_indices = middle_colony_indices,
                                                colony_area_raw_data = colony_area_raw_data,
                                                excluded_colonies = excluded_colonies,
                                                plate_median_threshold = plate_median_threshold,
                                                param1_threshold = param1_threshold,
                                                is_middle = is_middle)

  mid_excluded_colonies_E <- excluded_coloniesE(plateformat = plateformat,
                                                colony_indices = middle_colony_indices,
                                                colony_area_raw_data = colony_area_raw_data,
                                                empty_indices = empty_indices,
                                                excluded_colonies = excluded_colonies,
                                                plate_median_threshold = plate_median_threshold,
                                                param1_threshold = param1_threshold,
                                                param2_threshold = param2_threshold,
                                                is_middle = is_middle)

  mid_excluded_colonies_F <- excluded_coloniesF(plateformat = plateformat,
                                                colony_indices = middle_colony_indices,
                                                colony_area_raw_data = colony_area_raw_data,
                                                empty_indices = empty_indices,
                                                excluded_colonies = excluded_colonies,
                                                plate_median_threshold = plate_median_threshold,
                                                param1_threshold = param1_threshold,
                                                param2_threshold = param2_threshold,
                                                is_middle = is_middle)

  mid_excluded_colonies_G <- excluded_coloniesG(plateformat = plateformat,
                                                colony_indices = middle_colony_indices,
                                                colony_area_raw_data = colony_area_raw_data,
                                                empty_indices = empty_indices,
                                                excluded_colonies = excluded_colonies,
                                                plate_median_threshold = plate_median_threshold,
                                                param1_threshold = param1_threshold,
                                                is_middle = is_middle)

  mid_excluded_colonies_H <- excluded_coloniesH(plateformat = plateformat,
                                                colony_indices = middle_colony_indices,
                                                colony_area_raw_data = colony_area_raw_data,
                                                empty_indices = empty_indices,
                                                excluded_colonies = excluded_colonies,
                                                plate_median_threshold = plate_median_threshold,
                                                param1_threshold = param1_threshold,
                                                is_middle = is_middle)

  #peripheral colonies
  is_middle <- FALSE

  excluded_colonies_A <- excluded_coloniesA(plateformat = plateformat,
                                            colony_indices = not_middle_colony_indices,
                                            colony_area_raw_data = colony_area_raw_data,
                                            empty_indices = empty_indices,
                                            excluded_colonies = mid_excluded_colonies_A,
                                            plate_median_threshold = plate_median_threshold,
                                            param1_threshold = param1_threshold,
                                            param2_threshold = param2_threshold,
                                            is_middle = is_middle)

  excluded_colonies_B <- excluded_coloniesB(plateformat = plateformat,
                                            colony_indices = not_middle_colony_indices,
                                            colony_area_raw_data = colony_area_raw_data,
                                            excluded_colonies = mid_excluded_colonies_B,
                                            plate_median_threshold = plate_median_threshold,
                                            param1_threshold = param1_threshold,
                                            param2_threshold = param2_threshold,
                                            is_middle = is_middle)

  excluded_colonies_C <- excluded_coloniesC(plateformat = plateformat,
                                            colony_indices = not_middle_colony_indices,
                                            colony_area_raw_data = colony_area_raw_data,
                                            empty_indices = empty_indices,
                                            excluded_colonies = mid_excluded_colonies_C,
                                            plate_median_threshold = plate_median_threshold,
                                            param1_threshold = param1_threshold,
                                            is_middle = is_middle)

  excluded_colonies_D <- excluded_coloniesD(plateformat = plateformat,
                                            colony_indices = not_middle_colony_indices,
                                            colony_area_raw_data = colony_area_raw_data,
                                            excluded_colonies = mid_excluded_colonies_D,
                                            plate_median_threshold = plate_median_threshold,
                                            param1_threshold = param1_threshold,
                                            is_middle = is_middle)

  excluded_colonies_E <- excluded_coloniesE(plateformat = plateformat,
                                            colony_indices = not_middle_colony_indices,
                                            colony_area_raw_data = colony_area_raw_data,
                                            empty_indices = empty_indices,
                                            excluded_colonies = mid_excluded_colonies_E,
                                            plate_median_threshold = plate_median_threshold,
                                            param1_threshold = param1_threshold,
                                            param2_threshold = param2_threshold,
                                            is_middle = is_middle)

  excluded_colonies_F <- excluded_coloniesF(plateformat = plateformat,
                                            colony_indices = not_middle_colony_indices,
                                            colony_area_raw_data = colony_area_raw_data,
                                            empty_indices = empty_indices,
                                            excluded_colonies = mid_excluded_colonies_F,
                                            plate_median_threshold = plate_median_threshold,
                                            param1_threshold = param1_threshold,
                                            param2_threshold = param2_threshold,
                                            is_middle = is_middle)

  excluded_colonies_G <- excluded_coloniesG(plateformat = plateformat,
                                            colony_indices = not_middle_colony_indices,
                                            colony_area_raw_data = colony_area_raw_data,
                                            empty_indices = empty_indices,
                                            excluded_colonies = mid_excluded_colonies_G,
                                            plate_median_threshold = plate_median_threshold,
                                            param1_threshold = param1_threshold,
                                            is_middle = is_middle)

  excluded_colonies_H <- excluded_coloniesH(plateformat = plateformat,
                                            colony_indices = not_middle_colony_indices,
                                            colony_area_raw_data = colony_area_raw_data,
                                            empty_indices = empty_indices,
                                            excluded_colonies = mid_excluded_colonies_H,
                                            plate_median_threshold = plate_median_threshold,
                                            param1_threshold = param1_threshold,
                                            is_middle = is_middle)

  excluded_colonies_A_H <- list()
  variations <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
  v_counter <- 1

  for(i in variations){
    variation_name <- paste('excluded_colonies_', i, sep = '')

    if(class(quote(variation_name)) == 'name'){
      excluded_colonies_A_H[[v_counter]] <- list(eval(parse(text=variation_name)))
    } else {
      stop('The eval object "variation_name" does not belong to name class')
    }

    names(excluded_colonies_A_H)[[v_counter]] <- variation_name
    v_counter <- v_counter + 1
  }
  # 2. save excluded colonies as .RDS file format
  if(is_save){
    saveRDS(excluded_colonies_A_H,
            file = excluded_file,
            ascii = FALSE,
            compress = 'xz')
  }

  # 3. return excluded colonies A to H as invisible named list
  invisible(excluded_colonies_A_H)
}
