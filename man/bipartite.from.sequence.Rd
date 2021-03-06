% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bipartite.from.sequence.R
\name{bipartite.from.sequence}
\alias{bipartite.from.sequence}
\title{Generates a bipartite graph from row and column degree sequences}
\usage{
bipartite.from.sequence(R, C, class = "matrix")
}
\arguments{
\item{R}{numeric vector: requested row degree sequence of positive integers}

\item{C}{numeric vector: requested column degree sequence of positive integers}

\item{class}{string: the class of the returned backbone graph, one of c("matrix", "Matrix", "sparseMatrix", "igraph", "network")}
}
\description{
`bipartite.from.sequence` returns a bipartite graph, as an object of the requested class,
 that has the given row and column degree sequences.
}
\examples{
B <- bipartite.from.sequence(R = c(1,1,2), C = c(1,1,2))
B <- bipartite.from.sequence(R = c(1,1,2), C = c(1,1,2), class = "igraph")
B <- bipartite.from.sequence(R = c(1,1,2), C = c(1,1,2), class = "network")
}
