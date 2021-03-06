% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reformat.compositional.data.R
\name{reformat.compositional.data}
\alias{reformat.compositional.data}
\title{Reformat Casal2 compositional observation so it is the same format as the legacy Casal observation.}
\usage{
reformat.compositional.data(model, report_label)
}
\arguments{
\item{model}{the r object that has been extracted using the extract() function.}

\item{report_label}{<string> the label of the report for the observation you want converted}
}
\description{
This function will take a compositional observation that has been generated by Casal2 and re-format it so that it has the same structure as a CASAL reported compositional observation. The purpose for this function is to reformat the Casal2 observations so we can then feed them into packages that have been tailored for Casal observations, such as Chris Francis's DataWeighting library.
I found this useful when comparing with CASAl model runs.
}
\author{
Craig Marsh
}
