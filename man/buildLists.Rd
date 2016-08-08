\name{buildLists}
\alias{buildPGConnection}
\alias{buildMetaData}

\title{
Builds standard lists of properties for later use.
}

\description{
At several points in the data loading process, information on PostGreSQL connection parameters or metadata is required. These functions provide a standard list of values that can be used throughout for both.
}

\usage{
buildPGConnection(dbNameIn, userNameIn, passwordIn, hostIn = 'localhost', portIn = "5432", silent = TRUE)
buildMetaData(tableIn, schemaIn, descriptionIn, providerIn, datasetIn, licenseIn, attributionIn, date_acquiredIn, date_acquiredIn, date_generatedIn, subsetIn = NA, silent = FALSE)
}

\arguments{
  \item{dbNameIn}{The name of the PostGreSQL database to connect to.}
  \item{userNameIn}{The user name to connect to the PostGreSQL database with.}
  \item{passwordIn}{The password to connect to the PostGreSQL database with.}
  \item{hostIn}{The address of the machine hosting the PostGreSQL database.}
  \item{portIn}{The port to connect to the PostGreSQL database on.}
  \item{silent}{If progress messages should be suppressed.}
  \item{tableIn}{The name of the table that the data will be written into.}
  \item{schemaIn}{The name of the schema that the output table will be contained in.}
  \item{descriptionIn}{A manual description of the data contained in the table.}
  \item{providerIn}{The organisation that provided the data.}
  \item{datasetIn}{The collection that the data belongs to.}
  \item{licenseIn}{The license that the data was provided under.}
  \item{attributionIn}{The attribution required to use the data.}
  \item{date_acquiredIn}{The date on which the data was obtained.}
  \item{date_generatedIn}{The date on which the data was created.}
  \item{subsetIn}{If the table has multiple parts, the name of the part being loaded.}
}

\value{A list/vector containing the data provided.}

\author{Joseph McGrath}

\examples{
pgConInfo <- buildPGConnection("SpatialDBName", "postgres", "postgres")
}
