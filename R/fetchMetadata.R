fetchMetadata <- function(pgConnectionIn,
                          metadataIn,
                          metadataTable = "\"metadata\".\"metainfo\"",
                          silent = FALSE
                          ){
#Extracts the unique id for the metadata relating to an individual table from
#   PostGreSQL. Assumes the metadata table exists.
#
#Returns the metadata id to use for the row or NA if the row is not present.
#
    library("RPostgreSQL")
    
    if(!silent){
        cat("Fetching metadata.\n")
    }
    
    if(!checkMetadata(metadataIn, silent = silent)){
        stop("Invalid metadata provided - stopping.")
    }
    
    if(!silent){
        cat("Connecting to PostGreSQL database.\n")
    }
    
    dbCon <- postGresConnect(pgConnectionIn)
    on.exit(dbDisconnect(dbCon))
    #Assumes the table exists. Should instead check is it's there.              To do
    
    #May be able to use paste here, seeing as I can't use prepared queries.     To do
    #   e.g. paste(names(metadataUse),
    #              metadataUse,
    #              sep = " = ",
    #              collapse = " AND\n\t"
    #              )
    if(is.na(metadataIn["subset"])){
        res <- dbGetQuery(dbCon,
                          sprintf("SELECT
                                       id
                                   FROM
                                       %s
                                   WHERE
                                      subset IS NULL AND
                                      tablename = E'%s' AND
                                      date_generated = '%s'
                                   ;",
                                   metadataTable,
                                   metadataIn["tablenameDq"],
                                   metadataIn["date_generated"]
                                   )
                          )
    } else {
        res <- dbGetQuery(dbCon,
                          sprintf("SELECT
                                       id
                                   FROM
                                       %s
                                   WHERE
                                      tablename = '%s' AND
                                      date_generated = '%s' AND
                                      subset = E'%s'
                                   ;",
                                   metadataTable,
                                   metadataIn["tablenameDq"],
                                   metadataIn["date_generated"],
                                   metadataIn["subset"]
                                   )
                          )
    }
    
    if(nrow(res) > 0){
        ret <- res[1, "id"]
        if(!silent){
            cat(sprintf("Metadata found - id of: %s\n", ret))
        }
    } else {
        ret <- NA
        if(!silent){
            cat("Metadata not found.\n")
        }
    }
    
    return(ret)
}
