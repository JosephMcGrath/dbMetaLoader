\name{loadSpatialData}
\alias{loadSpatialData}

\title{
Loads spatial data, along with it's metadata into a PostGIS database.
}

\description{
Performs the actual loading operation of data into the database after adding its metadata into the catalogue. Performs some cleanup after loading in the data.
}

\usage{
loadSpatialData(fileIn, pgConnectionIn, metadataIn, metadataTable = "\"metadata\".\"metainfo\"", append = FALSE, multiGeom = FALSE, silent = FALSE)
}

\arguments{
  \item{fileIn}{The location of the file to be loaded into the database.}
  \item{pgConnectionIn}{A standard list of connection parameters to use.}
  \item{metadataIn}{The metadata of the table to be loaded.}
  \item{metadataTable}{The name of the metadata table contained in the database.}
  \item{append}{If the table should be appened to an existing table.}
  \item{multiGeom}{If the geometry being loaded is of a multi type, e.g. multipoint or multipolygon.}
  \item{silent}{If progress reports/errors should be reported.}
}

\value{Does not return anything.}

\author{Joseph McGrath}
