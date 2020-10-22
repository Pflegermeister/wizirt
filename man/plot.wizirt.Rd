% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_methods.R
\name{plot.wizirt}
\alias{plot.wizirt}
\title{Plot method for wizirt objects}
\usage{
\method{plot}{wizirt}(
  wizirt_fit,
  type = "tinfo",
  items = NULL,
  persons = NULL,
  facets = TRUE,
  quads = 10,
  return_data = FALSE
)
}
\arguments{
\item{type}{Character string. Currently, can be 'trace' for item characteristic curves or 'info' for item information.
'obs', 'trace', 'info', 'resid', 'stand', 'tinfo', 'theta', 'diff', 'theta_diff', and 'np_prf'}

\item{items}{Which items to plot? Either a numeric vector of item positions in the column names of the data, or a vector of the item names to plot. If nothing is specifed all items will be plotted.}

\item{persons}{Which persons to plot? Either a numeric vector of person positions in the row names of the data, or a vector of the person ids to plot. If nothing is specifed all persons will be plotted.}

\item{facets}{Logical. Should the plots be faceted? Default is TRUE.}

\item{quads}{Numeric. For plots using residuals (i.e. resid, stand). How many quantiles should the data be broken into?}

\item{return_data}{Logical. Should the plot data be returned. If TRUE returns a list with the plot and the data.}
}
\description{
All plots in wizirt use ggplot2 as a backend. Type 'theta_diff' also uses gghalves. For the item-focused plots, the types can be included in the same string to overlay the plots (e.g. resid_trace).
For large numbers of persons and/or items, you may want to consider limiting the number printed at a time on some plots.
}