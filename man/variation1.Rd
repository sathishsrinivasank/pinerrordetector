% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variation1.R
\name{variation1}
\alias{variation1}
\title{Variation-1}
\usage{
variation1(colony, combin_coords, neighbors_selected_colony, excluded_colonies,
  colony_area_raw_data, plate_median_threshold, empty_indices, param1_threshold,
  param2_threshold)
}
\arguments{
\item{colony}{selected colony by the outer loop of the neighborful algorithm}

\item{combin_coords}{combinations of neighbors adjoining the selected colony}

\item{neighbors_selected_colony}{neighbors adjoining the selected colony}

\item{excluded_colonies}{A numeric vector of indices that are part of excluded
colonies}

\item{colony_area_raw_data}{A numeric vector of raw data representing the area
yeast grown at a specific location on a nutrient medium agar plate}

\item{plate_median_threshold}{This threshold value is computed by multiplying
the value of \code{percent_median_thresh} with the value from
\code{plate_median(empty_indices,colony_area_raw_data)} and dividing it by 100}

\item{empty_indices}{A numeric vector of indexed empty locations. It can also
be the indices of control colonies grown on a plate, which may be used for
normalizing the colony area of a plate}

\item{param1_threshold}{The numeric value which can be one from \code{1:8}.
This exclusion criteria-1 of neighborful algorithm represents the number of
adjoining neighbors of the selected colony from \code{colony} that
has colony area less than or equal to \code{plate_median_threshold}. If the
selected colony is surrounded by at least \code{param1_threshold} neighbors,
then it will be considered as excluded colony by the neighborful algorithm}

\item{param2_threshold}{The numeric value which can be one from \code{1:8}.
This exclusion criteria-2 of neighborful algorithm represents the number of
adjoing neighbors of the selected colony from \code{colony_indices} are
excluded by the previous iteration of neighborful algorithm. If the selected
colony is surrounded by at least \code{param2_threshold} neighbors, then it
will be considered as excluded colony by the neighborful algorithm.}
}
\value{
A vector of excluded colonies by variation1
}
\description{
Inner loop of neighborful algorithm used by its variations A and E
}
\examples{
plateformat <- 1536
middle_colony <- 34
param1_threshold <- 6
param2_threshold <- 2
neighbors_selected_colony <- middle_neighbors(plateformat, middle_colony)
combin_coords <- combn(length(neighbors_selected_colony), param1_threshold)
data_area <- simulated_data_1536(data_384 = colonyarea$data_subtypes,
                                 in_data_flow = "across",
                                 out_data_flow = "down",
                                 is_plate_coords = TRUE)
empty_indices <- which(convert_small_to_large(plate_from = 384,
                                              plate_to = 1536,
                                              data_from = colonyarea$data_subtypes,
                                              in_data_flow = 'across',
                                              out_data_flow = "down",
                                              is_plate_coords = FALSE)$y \%in\% 'Empty')
p_median <- plate_median(empty_indices = empty_indices,
                         colony_area_raw_data = data_area$y)
plate_median_threshold <- p_median/4
variation1(colony = middle_colony,
           combin_coords = combin_coords,
           neighbors_selected_colony = neighbors_selected_colony,
           excluded_colonies = c(2),
           colony_area_raw_data = data_area$y,
           plate_median_threshold = plate_median_threshold,
           empty_indices = empty_indices,
           param1_threshold = param1_threshold,
           param2_threshold = param2_threshold)
}

