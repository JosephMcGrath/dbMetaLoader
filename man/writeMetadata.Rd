\name{writeMetadata}
\alias{writeMetadata}

\title{
Writes the provided metadata into the metadata table of a database.
}

\description{
Takes a standard list of metadata provided and attempts to write a new record for it in the database.
}

\usage{
writeMetadata(pgConnectionIn, metadataIn, metadataTable = "\"metadata\".\"metainfo\"", silent = FALSE)
}

\arguments{
  \item{pgConnectionIn}{A standard list of connection parameters to use.}
  \item{metadataIn}{A standard list of metadata to be loaded.}
  \item{metadataTable}{The name of the metadata table contained in the database.}
  \item{silent}{If progress reports/errors should be reported.}
}

\value{This function returns nothing.}

\author{Joseph McGrath}
