% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iaitrees.R
\name{get_regression_constant.regression_tree_learner}
\alias{get_regression_constant.regression_tree_learner}
\title{Return the constant term in the linear regression prediction at a node of a
regression tree}
\usage{
\method{get_regression_constant}{regression_tree_learner}(obj, node_index, ...)
}
\arguments{
\item{obj}{The learner to query.}

\item{node_index}{The node in the tree to query.}

\item{...}{Refer to the Julia documentation for available parameters.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.2.2/IAITrees/reference/#IAI.get_regression_constant-Tuple\%7BRegressionTreeLearner\%2C\%20Int64\%7D}{\code{IAI.get_regression_constant}}
}
\examples{
\dontrun{iai::get_regression_constant(lnr, 1)}

}
