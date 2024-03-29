% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iaitrees.R
\name{get_classification_label.classification_tree_learner}
\alias{get_classification_label.classification_tree_learner}
\title{Return the predicted label at a node of a tree}
\usage{
\method{get_classification_label}{classification_tree_learner}(obj, node_index, ...)
}
\arguments{
\item{obj}{The learner to query.}

\item{node_index}{The node in the tree to query.}

\item{...}{Refer to the Julia documentation for available parameters.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.1.1/IAITrees/reference/#IAI.get_classification_label-Tuple\%7BClassificationTreeLearner\%2C\%20Int64\%7D}{\code{IAI.get_classification_label}}
}
\examples{
\dontrun{iai::get_classification_label(lnr, 1)}

}
