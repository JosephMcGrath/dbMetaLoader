readMetadataFromCsv <- function(pathIn){
    
    ret <- read.csv(pathIn, stringsAsFactors = FALSE)
    
    #Convert from 0/1 to logical
    ret$multigeom <- as.logical(ret$multigeom)
    ret$append <- as.logical(ret$append)
    
    ret$subset[ret$subset == ""] <- NA
    
    return(ret)
    
}
