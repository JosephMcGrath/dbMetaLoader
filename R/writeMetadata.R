writeMetadata <- function(pgConnectionIn,
                          metadataIn,
                          metadataTable = "\"metadata\".\"metainfo\""
                          ){
#Writes the metadata record for a table into a PostGreSQL database from a set of
#   standard lists. 
#
#Returns nothing.
#
    library("RPostgreSQL")
    
    cat("Writing metadata.\n")
    
    #Doesn't actually stop if there's a problem!                                To do
    checkMetadata(metadataIn)
    
    dbCon <- postGresConnect(pgConnectionIn)
    on.exit(dbDisconnect(dbCon))
    
    #Assumes the table exists.                                                  To do
    if(is.na(metadataIn$subset)){
        queryIn <- sprintf("INSERT INTO %s (
                                tablename,
                                description,
                                provider,
                                dataset,
                                date_generated,
                                license,
                                attribution,
                                date_acquired
                            ) VALUES (
                                E'%s',
                                E'%s',
                                E'%s',
                                E'%s',
                                '%s',
                                E'%s',
                                E'%s',
                                '%s'
                                
                            );",
                           metadataTable,
                           metadataIn["tablenameDq"],
                           metadataIn["description"],
                           metadataIn["provider"],
                           metadataIn["dataset"],
                           metadataIn["date_generated"],
                           metadataIn["license"],
                           metadataIn["attribution"],
                           metadataIn["date_acquired"]
                           )
    } else {
        queryIn <- sprintf("INSERT INTO %s (
                                tablename,
                                description,
                                provider,
                                dataset,
                                date_generated,
                                license,
                                attribution,
                                date_acquired,
                                subset
                            ) VALUES (
                                E'%s',
                                E'%s',
                                E'%s',
                                E'%s',
                                '%s',
                                E'%s',
                                E'%s',
                                '%s',
                                E'%s'
                                
                            );",
                           metadataTable,
                           metadataIn["tablenameDq"],
                           metadataIn["description"],
                           metadataIn["provider"],
                           metadataIn["dataset"],
                           metadataIn["date_generated"],
                           metadataIn["license"],
                           metadataIn["attribution"],
                           metadataIn["date_acquired"],
                           metadataIn["subset"]
                           )
    }
    
    cat(queryIn)
    
    dbSendQuery(dbCon, queryIn)
}
