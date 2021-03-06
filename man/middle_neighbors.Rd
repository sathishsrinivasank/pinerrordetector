% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/middle_neighbors.R
\name{middle_neighbors}
\alias{middle_neighbors}
\title{Neighbors adjoining the colony index from middle of the plate}
\usage{
middle_neighbors(plateformat, n)
}
\arguments{
\item{plateformat}{An integer which can be one of 96 or 384 or 1536}

\item{n}{A colony index from the middle of the plate. If \code{n} is not from
the middle of the plate, an exception is thrown.}
}
\value{
numeric vector  of colony indices that are neighbors adjoining the
selected colony \code{n}
}
\description{
Neighbors adjoining the colony index from middle of the plate
}
\examples{
plateformat <- 1536
all_indices <- 1:plateformat
middle_colony_indices <- all_indices[-(not_middle_indices(plateformat))]
middle_colony <- middle_colony_indices[1]
middle_neighbors(plateformat, middle_colony)
}

