% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viz-options.R
\name{selectVizUI}
\alias{selectVizUI}
\title{Select visualization type (table, ggplot, plotly)}
\usage{
selectVizUI(
  id,
  choices = c("ggplot", "plotly", "table"),
  selected = NULL,
  individual = FALSE,
  size = "normal",
  justified = FALSE,
  float_right = FALSE,
  ...
)
}
\arguments{
\item{id}{Unique identifier.}

\item{choices}{Choices must be some subset of "ggplot", "plotly", "table"}

\item{selected}{The initially selected value. If not specified, then it
defaults to the first item in \code{choices}. To start with no items selected,
use \code{character(0)}.}

\item{individual}{If TRUE, buttons are separated.}

\item{size}{Size of the buttons ('xs', 'sm', 'normal', 'lg')}

\item{justified}{If TRUE, fill the width of the parent div}

\item{float_right}{Logical. Whether or not to float the buttons to the right.}

\item{...}{Additional arguments to pass to radio_group_buttons}
}
\description{
Select visualization type (table, ggplot, plotly)
}
