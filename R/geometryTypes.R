geometryTypes <- function(pathIn, geometryColumn = "Geometry"){
    library("gdalUtils")
    
    sqlName <- head(unlist(strsplit(tail(unlist(strsplit(pathIn, "\\\\|/")), 1),
                                    "\\."
                                    )
                           ),
                           1
                    )
    
    sqlString <- sprintf("SELECT DISTINCT
                              ST_GeometryType(%s) AS GeomTypeOut
                          FROM
                              %s",
                         geometryColumn,
                         sqlName
                         )
    
    #The rgdal package also has an ogrinfo function.
    test <- gdalUtils::ogrinfo(pathIn,
                               q = TRUE,
                               ro = TRUE,
                               sql = sqlString,
                               dialect = "SQLITE"
                               )
    
    testOut <- grep("GeomType", test, value = TRUE)
    testOut <- grep("GeomTypeOut",
                    unlist(strsplit(testOut, "=")),
                    invert = TRUE,
                    value = TRUE
                    )
    
    testOut <- gsub(" ", "", testOut)
    
    testOut <- testOut
    
    return(testOut)
}

geometryString <- function(pathIn, geometryColumn = "Geometry"){
    ret <- geometryTypes(pathIn, geometryColumn)
    
    ret <- paste(ret, collapse = ":")
    
    return(ret)
}
