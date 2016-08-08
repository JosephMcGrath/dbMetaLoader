\name{fetchMetadata}
\alias{fetchMetadata}

\title{
Extracts the metadata id's from the in-database metadata table.
}

\description{
Given a metadata object, this function finds the relevant metadata id that applies to that table, or NA if none match. In the (unlikely due to database setup) event of multiple results, the first is returned.
}

\usage{
fetchMetadata(pgConnectionIn, metadataIn, metadataTable = "\"metadata\".\"metainfo\"", silent = FALSE)
}

\arguments{
  \item{pgConnectionIn}{A list to connect to the PostGreSQL database with.}
  \item{metadataIn}{A list containing the metadata for the table to be found.}
  \item{metadataTable}{The name of the in-database metadata table that contains metadata.}
  \item{silent}{If errors/progress reports should be suppressed.}
}

\value{Integer of the metadata id or NA if the provided metadata turned up no results.}

\author{Joseph McGrath}
