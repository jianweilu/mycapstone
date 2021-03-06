% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1capstoneproject2.R
\name{eq_data_read}
\alias{eq_data_read}
\title{Reading the NOAA earthquake data file}
\usage{
eq_data_read(filename)
}
\arguments{
\item{The}{filename of the NOAA earthquake data file}
}
\value{
tbl_df object (earthquake data)
}
\description{
Reading the NOAA earthquake data file
}
\note{
Stop if the filename does not exist (error message)
}
\examples{
\dontrun{
filename<-system.file("data","earthquakes-2021-02-17_20-53-07_-0500.tsv",package="mycapstone") #xx
eq_data_read(filename)
}

}
