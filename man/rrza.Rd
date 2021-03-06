% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global_vars.R, R/make_RZA_plots.R
\docType{package}
\name{rrza}
\alias{rrza}
\alias{rrza-package}
\alias{make_rza_plots}
\title{Abundance Maps for Rapid Zooplankton Assesment taxa.}
\usage{
make_rza_plots(rza_path, region, facets = TRUE)
}
\arguments{
\item{rza_path}{The path to the directory where the .xlxs rza dataframe
is located along with the .xlxs file name. This is a dataframe specifically
formated for rza data entry.}

\item{region}{There are six geographic regions which produce standardized
maps. The six standardized maps can be set to Arctic, Northern BS, 70 meter,
BASIS, GOA, and MACE 2018.}

\item{facets}{By default facets is set to TRUE. When set to TRUE the
function will produce two sets of faceted maps. One of the 60 bongo rza
taxa and one of the 20 bongo rza taxa. When facets is set to FALSE the
function will produce a map for each rza taxa.}
}
\value{
Either faceted maps or individual maps of rza taxa abundance.
}
\description{
This function takes an RZA .xlxs dataframe and will make either
individual plots of each RZA taxa group or two faceted maps based on 150
and 500 micron mesh. The maps plot abundance per cubic meter of the
rza taxa for each station. The map regions are standardized for each of the
major cruises/grids for EcoFOCI.
}
