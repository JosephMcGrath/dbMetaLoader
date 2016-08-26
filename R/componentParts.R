componentParts <- function(type = NA){
    
    #ESRI shape
    extensions <- data.frame(primary = "shp",
                             components = c("shp", "shx", "dbf", "prj", "cpg"),
                             stringsAsFactors  = FALSE
                             )
    #MapInfo TAB
    extensions <- rbind(extensions,
                        data.frame(primary = "tab",
                                   components = c("tab", "dat", "map", "id"),
                                   stringsAsFactors  = FALSE
                                   )
                        )
    #MapInfo MIF
    extensions <- rbind(extensions,
                        data.frame(primary = "mif",
                                   components = c("mif", "mid"),
                                   stringsAsFactors  = FALSE
                                   )
                        )
    
    if(is.na(type)){
        ret <- extensions
    } else {
        ret <- extensions[extensions$primary == type, "components"]
        
        if(length(ret) == 0){
            ret <- type
        }
    }
    
    return(ret)
}

getComponents <- function(type){
    
    fileExtensions <- componentParts(type)
    
    ret <- paste0(fileExtensions, sep = "$", collapse = "|")
    ret <- sprintf("(?i)%s", ret)
    
    return(ret)
    
}
  
getExtensions <- function(pathIn){
    ret <- unlist(lapply(strsplit(pathIn, "\\."), tail, 1))
    
    return(ret)
}

dropExtensions <- function(pathIn){
    
    #Drops everything from the first dot, want everything from the last         To do    
    ret <- gsub("\\..*", "", pathIn)
    
    return(ret)
}
