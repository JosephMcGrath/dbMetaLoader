\name{tempUnzip}
\alias{tempUnzip}

\title{
Temporarily unzips a zip archive so that it can be accessed by another function for loading ect.
}

\description{
If there is a single spatial data set in the file, then it is unzipped to a temporary path, otherwise the function fails.
}

\usage{
tempUnzip(fileIn, tempPath = tempfile())
}

\arguments{
  \item{fileIn}{The zip file to process.}
  \item{tempPath}{The name of a folder to extract it to.}
}

\value{The name of the new temporary file.}

\author{Joseph McGrath}
