\name{loadFromCsv}
\alias{loadFromCsv}

\title{
Attempts to load all files listed as ready to load in a csv listing of metadata/file names.
}

\description{
Recursively searches through all the folders in the specified directory and locates any files recognised as storing spatial data. Also searches through zip files to find files.
}

\usage{
loadFromCsv(fileIn, pgConnectionIn, silent = FALSE)
}

\arguments{
  \item{dirIn}{The directory to recursively search through.}
  \item{csvPath}{The path of the CSV to write results to.}
  \item{silent}{If errors/progress reports should be suppressed.}
}

\value{Does not return anything.}

\author{Joseph McGrath}
