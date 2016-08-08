tempUnzip <- function(fileIn, tempPath = tempfile()){
#Temporarily extracts the spatial data out of a compressed zip file so that it
#   can be accessed to load data from. Currently requires some work, mainly
#   around zip files containing multiple spatial files.
#
#Returns the new name of the temporary file.
#
    #Use some delimeter to define the file in the zip file                      To do
        #Possibly | or $
        #If there's no delimeter, assume a single table.
    
    zipFiles <- unzip(fileIn, list = TRUE)$Name
    
    spatialFile <- grep(spatialTables(), zipFiles, value = TRUE)
    
    if(length(spatialFile) != 1){
        stop("Zip file must contain exactly one spatial file.")
    }
    
    if(length(grep("(?i)\\.shp$", zipFiles))){
        zipList <- grep("(?i)\\.shp$|\\.shx$|\\.dbf$|\\.prj$",
                        zipFiles,
                        value = TRUE
                        )
    } else if (length(grep("(?i)\\.tab$", zipFiles))){
        zipList <- grep("(?i)\\.tab$|\\.dat$|\\.map$|\\.id$",
                        zipFiles,
                        value = TRUE
                        )
    }
    
    #Unzip all the parts of it to a temporary folder
    dirOut <- tempdir()
    
    unzip(zipfile = fileIn,
          files = zipList,
          exdir = dirOut
          )
    
    tempFile <- list.files(dirOut, full.names = TRUE, pattern = spatialFile)
    
    tempFile <- unique(tempFile)
    
    if(length(tempFile) > 1){
        stop("Error - unzipping produced multiple spatial tables.", tempFile)
    }
    
    return(tempFile)
}
