% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plate_coords.R
\name{plate_coords}
\alias{plate_coords}
\title{Generate Rectangular Plate Coordinates}
\usage{
plate_coords(plate_to, data_from, out_data_flow)
}
\arguments{
\item{plate_to}{An integer giving plateformat}

\item{data_from}{vector of data for which coordinates are created}

\item{out_data_flow}{A string which tells the function to output the
converted data to a plate configuration which can be down or across}
}
\value{
Data frame with plate coordinates
}
\description{
plate_coords generates rectangular plate coordinates
}
\examples{
plate_coords(1536, 1:1536, "across")

}
