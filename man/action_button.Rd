% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widgets.R
\name{action_button}
\alias{action_button}
\title{Action button}
\usage{
action_button(
  inputId,
  label,
  br = TRUE,
  style = "material-circle",
  color = "danger",
  ...
)
}
\arguments{
\item{inputId}{The \code{input} slot that will be used to access the value.}

\item{label}{The contents of the button, usually a text label.}

\item{br}{Logical indicating whether or not to add breaks above and below
button.}

\item{style}{Style of the button, to choose between \code{simple}, \code{bordered},
\code{minimal}, \code{stretch}, \code{jelly}, \code{gradient}, \code{fill},
\code{material-circle}, \code{material-flat}, \code{pill}, \code{float}, \code{unite}.}

\item{color}{Color of button.}

\item{...}{Additional arguments to pass to \verb{shinyWidgets::actionBttn(()}.}
}
\description{
Wrapper around \code{actionButton} with different defaults
}
