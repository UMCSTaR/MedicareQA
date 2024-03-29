% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/observation_count.R
\name{observation_count}
\alias{observation_count}
\title{Count the number of observations in a set of files inside a target directory}
\usage{
observation_count(
  data_root_or_file,
  output_path = NA,
  file.pattern = ".csv",
  save_report = FALSE
)
}
\arguments{
\item{data_root_or_file}{Directory path to scan for files OR direct file path if single file}

\item{output_path}{Path for saving report. Path must include .csv name of report. Default is same directory + new folder called reports/obs_report.csv}

\item{file.pattern}{Pattern to use for searching files (default: .csv)}
}
\value{
data.frame with two columns where each row contains the file and number of observations in the file
}
\description{
Count the number of observations in a set of files inside a target directory
}
