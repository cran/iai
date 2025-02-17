% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iaitrees.R
\name{get_cluster_assignments}
\alias{get_cluster_assignments}
\title{Return the indices of the trees assigned to each cluster, under the
clustering of a given number of trees}
\usage{
get_cluster_assignments(stability, num_trees)
}
\arguments{
\item{stability}{The stability analysis to query}

\item{num_trees}{The number of trees to include in the clustering}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.2.2/IAITrees/reference/#IAI.get_cluster_assignments}{\code{IAI.get_cluster_assignments}}
}
\section{IAI Compatibility}{

Requires IAI version 2.2 or higher.
}

\examples{
\dontrun{iai::get_cluster_assignments(stability, num_trees)}

}
