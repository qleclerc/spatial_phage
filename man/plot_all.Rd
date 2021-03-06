% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_all.R
\name{plot_all}
\alias{plot_all}
\title{Plots the bacteria and phage}
\usage{
plot_all(data_bac, data_pha, size = 100, legend = F, title = NULL)
}
\arguments{
\item{data_bac}{The dataset containing the bacteria information}

\item{data_pha}{The dataset containing the phage information}

\item{size}{The size of the space within which to place the bacteria and phage. Must be the same as the one used to place them.}

\item{legend}{Logical. If TRUE, will add a legend to the topright of the plots. Default is FALSE.}

\item{title}{Optional. If a string is provided, this will be rendered as the title of the plot.}
}
\value{
A plot
}
\description{
Plots the current position and status of the bacteria and phage
}
\examples{
plot_all(data_bac, data_pha, size=100)

}
