extractZip <- function(pathIn, delimiter = "$", cleanUp = TRUE){
    
    splitPath <- splitDelinitedPath(pathIn, delimiter)
    
    dCount <- length(splitPath) - 1
    
    if(dCount == 0){
        ret <- pathIn
    } else if(dCount > 0){
        
        pathOut <- tempExtractPath()
        
        #Ideally focus down the extraction process                              To do
        target <- splitPath[2]
        target <- dropExtensions(target)
        
        fullList <- unzip(splitPath[1], list = TRUE)$Name
        
        target <- grep(target, fullList, fixed = TRUE, value = TRUE)
        
        fileList <- unzip(splitPath[1], exdir = pathOut, files = target)
        
        newPath <- paste(tail(splitPath, dCount),
                         sep = delimiter,
                         collapse = delimiter
                         )
        newPath <- file.path(pathOut, newPath)
        
        ret <- extractZip(newPath, delimiter, cleanUp)
        
    }
    
    return(ret)
}
