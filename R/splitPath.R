splitPath <- function(pathIn, delimiter = "$"){
    
    ret <- strsplit(pathIn, delimiter)[[1]]
    
    return(ret)
}
