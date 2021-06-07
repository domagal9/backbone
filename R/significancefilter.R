#' The significance filter for backbone probabilities
#'
#' `sigfilter` computes the probability of signed edge weights being
#'     above or below the observed edge weights using
#'     XXX. Once computed,
#'     use \code{\link{backbone.extract}} to return
#'     the backbone matrix for a given alpha value.
#'
#' @param G graph: An graph object of class matrix, sparse matrix, igraph, edgelist, or network object.
#'     Any rows and columns of the associated bipartite matrix that contain only zeros are automatically removed before computations.
#'
#' @return backbone, a list(positive, negative, summary). Here
#'     `positive` is a matrix of probabilities of edge weights being equal to or above the observed value,
#'     `negative` is a matrix of probabilities of edge weights being equal to or below the observed value, and
#'     `summary` is a data frame summary of the inputted matrix and the model used including: model name, number of rows, skew of row sums, number of columns, skew of column sums, and running time.
#' @references

#' @export
#'
#' @examples
sigfilter <- function(G){
  ### Run Time ###
  run.time.start <- Sys.time()

  #### Class Conversion ####
  convert <- tomatrix(G)
  class <- convert$summary[[1]]
  G <- convert$G

  if (convert$summary[[3]] == TRUE){undirected <- TRUE}
  else{undirected <- FALSE}
  #if (convert$summary[[4]]==FALSE){stop("Graph must be weighted.")}
  #if (convert$summary[[2]]==TRUE){warning("This object is being treated as a unipartite network.")}

  #### INSERT MODEL HERE ####



  ### Run Time ###
  run.time.end <- Sys.time()
  total.time = (round(difftime(run.time.end, run.time.start, units = "secs"), 2))

  #### Compile Summary ####
  a <- c("Model", "Input Class", "Bipartite", "Symmetric", "Weighted", "Number of Rows", "Number of Columns", "Running Time (secs)")
  b <- c("Stochastic Degree Sequence Model", convert$summary$class, convert$summary$bipartite, convert$summary$symmetric, convert$summary$weighted, dim(G)[1], dim(G)[2], as.numeric(total.time))
  model.summary <- data.frame(a,b, row.names = 1)
  colnames(model.summary)<-"Model Summary"

  #### Return Backbone Object ####
  bb <- list(positive = Positive, negative = Negative, summary = model.summary)
  class(bb) <- "backbone"
  return(bb)
}
