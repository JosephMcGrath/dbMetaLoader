splitDelinitedPath <- function(pathIn, delimiter = "$"){
    
    ret <- strsplit(pathIn, "$", fixed = TRUE)[[1]]
    
    return(ret)
    
}
