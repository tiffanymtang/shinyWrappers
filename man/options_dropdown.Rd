% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widgets.R
\name{options_dropdown}
\alias{options_dropdown}
\title{Wrapper around \code{dropdown()}}
\usage{
options_dropdown(
  ...,
  status = "primary",
  size = "sm",
  icon = shiny::icon("cog"),
  width = "300px",
  right = TRUE,
  style = "material-circle",
  tooltip = FALSE
)
}
\arguments{
\item{...}{List of tag to be displayed into the dropdown menu.}

\item{status}{Color of the button, see \code{\link[shinyWidgets:actionBttn]{actionBttn()}}.}

\item{size}{Size of the button : \code{xs},\code{sm}, \code{md}, \code{lg}.}

\item{icon}{An optional icon to appear on the button.}

\item{width}{Width of the dropdown menu content.}

\item{right}{Logical. The dropdown menu starts on the right.}

\item{style}{Style of the button, to choose between \code{simple}, \code{bordered},
\code{minimal}, \code{stretch}, \code{jelly}, \code{gradient}, \code{fill},
\code{material-circle}, \code{material-flat}, \code{pill}, \code{float}, \code{unite}.}

\item{tooltip}{Put a tooltip on the button, you can customize tooltip with \code{\link[shinyWidgets:tooltipOptions]{tooltipOptions()}}.}
}
\description{
Wrapper around \code{dropdown()} with different default settings
}
