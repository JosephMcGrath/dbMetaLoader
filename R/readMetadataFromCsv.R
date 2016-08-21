readMetadataFromCsv <- function(pathIn){
    
    ret <- read.csv(pathIn, stringsAsFactors = FALSE)
    
    ret$multigeom <- as.logical(ret$multigeom)
    ret$append <- as.logical(ret$append)
    
    return(ret)
    
}
