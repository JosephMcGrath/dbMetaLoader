extractZip <- function(pathIn, delimiter = "$", cleanUp = TRUE){
    
    splitPath <- splitDelinitedPath(pathIn, delimiter)
    
    dCount <- length(splitPath) - 1
    
    if(dCount == 0){
        ret <- pathIn
    } else if(dCount > 0){
        
        pathOut <- tempExtractPath()
        
        #Ideally focus down the extraction process                              To do
        fileList <- unzip(splitPath[1], exdir = pathOut)
        
        newPath <- paste(tail(splitPath, dCount),
                         sep = delimiter,
                         collapse = delimiter
                         )
        newPath <- file.path(pathOut, newPath)
        
        ret <- extractZip(newPath, delimiter, cleanUp)
        
    }
    
    return(ret)
}
