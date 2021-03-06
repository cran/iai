% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iaitrees.R
\name{write_html}
\alias{write_html}
\title{Output a learner as an interactive browser visualization in HTML format}
\usage{
write_html(filename, lnr, ...)
}
\arguments{
\item{filename}{Where to save the output.}

\item{lnr}{The learner or grid to output.}

\item{...}{Refer to the Julia documentation for available parameters.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v2.2.0/IAITrees/reference/#IAI.write_html}{\code{IAI.write_html}}
}
\section{IAI Compatibility}{

Outputting a grid search requires IAI version 2.0 or higher.
}

\examples{
\dontrun{iai::write_html(file.path(tempdir(), "tree.html"), lnr)}

}
