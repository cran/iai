% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iaitrees.R
\name{get_cluster_distances}
\alias{get_cluster_distances}
\title{Return the distances between the centroids of each pair of clusters, under
the clustering of a given number of trees}
\usage{
get_cluster_distances(stability, num_trees)
}
\arguments{
\item{stability}{The stability analysis to query}

\item{num_trees}{The number of trees to include in the clustering}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.1.1/IAITrees/reference/#IAI.get_cluster_distances}{\code{IAI.get_cluster_distances}}
}
\section{IAI Compatibility}{

Requires IAI version 2.2 or higher.
}

\examples{
\dontrun{iai::get_cluster_distances(stability, num_trees)}

}
