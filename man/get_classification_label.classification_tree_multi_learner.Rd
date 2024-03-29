% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iaitrees.R
\name{get_classification_label.classification_tree_multi_learner}
\alias{get_classification_label.classification_tree_multi_learner}
\title{Return the predicted label at a node of a multi-task tree}
\usage{
\method{get_classification_label}{classification_tree_multi_learner}(obj, node_index, ...)
}
\arguments{
\item{obj}{The learner to query.}

\item{node_index}{The node in the tree to query.}

\item{...}{Refer to the Julia documentation for available parameters.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.1.1/IAITrees/reference/#IAI.get_classification_label-Tuple\%7BTreeLearner\%7BIAIBase.MultiTask\%7BIAIBase.ClassificationTask\%7D\%7D\%2C\%20Int64\%7D}{\code{IAI.get_classification_label}}
and
\href{https://docs.interpretable.ai/v3.1.1/IAITrees/reference/#IAI.get_classification_label-Tuple\%7BTreeLearner\%7BIAIBase.MultiTask\%7BIAIBase.ClassificationTask\%7D\%7D\%2C\%20Int64\%2C\%20Symbol\%7D}{\code{IAI.get_classification_label}}
}
\section{IAI Compatibility}{

Requires IAI version 3.2 or higher.
}

\examples{
\dontrun{iai::get_classification_label(lnr, 1)}

}
