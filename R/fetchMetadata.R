fetchMetadata <- function(pgConnectionIn,
                          metadataIn,
                          metadataTable = "\"metadata\".\"metainfo\""
                          ){
#Extracts the unique id for the metadata relating to an individual table from
#   PostGreSQL. Assumes the metadata table exists.
#
#Returns the metadata id to use for the row or NA if the row is not present.
#
    library("RPostgreSQL")
    
    cat("Fetching metadata.\n")
    
    #Doesn't actually stop if there's a problem!                                To do
    checkMetadata(metadataIn)
    
    dbCon <- postGresConnect(pgConnectionIn)
    on.exit(dbDisconnect(dbCon))
    #Assumes the table exists.                                                  To do
    
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
    } else {
        ret <- NA
    }
    
    return(ret)
}
