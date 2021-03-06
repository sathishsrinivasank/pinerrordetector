% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bottomleft_corner_neighbors.R
\name{bottomleft_corner_neighbors}
\alias{bottomleft_corner_neighbors}
\title{Get indices of three neighbors for the bottom left corner index of a plate}
\usage{
bottomleft_corner_neighbors(plateformat)
}
\arguments{
\item{plateformat}{An integer which can be one of 96 or 384 or 1536}
}
\value{
numeric vector of three neighbors for a given \code{plateformat}
}
\description{
Get indices of three neighbors for the bottom left corner index of a plate
}
\examples{
bottomleft_corner_neighbors(96)
bottomleft_corner_neighbors(1536)
}

