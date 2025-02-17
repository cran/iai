% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iaibase.R
\name{multi_questionnaire.grid_search}
\alias{multi_questionnaire.grid_search}
\title{Construct an interactive tree questionnaire using multiple learners from the
results of a grid search}
\usage{
\method{multi_questionnaire}{grid_search}(obj, ...)
}
\arguments{
\item{obj}{The grid to visualize}

\item{...}{Additional arguments (unused)}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.2.2/IAIBase/reference/#IAI.MultiQuestionnaire-Tuple\%7BGridSearch\%7D}{\code{IAI.MultiQuestionnaire}}
}
\section{IAI Compatibility}{

Requires IAI version 2.0 or higher.
}

\examples{
\dontrun{iai::multi_questionnaire(grid)}

}
