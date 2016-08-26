writeMetadata <- function(pgConnectionIn,
                          metadataIn,
                          metadataTable = "\"metadata\".\"metainfo\"",
                          silent = FALSE
                          ){
#Writes the metadata record for a table into a PostGreSQL database from a set of
#   standard lists. 
#
#Returns nothing.
#
    library("RPostgreSQL")
    
    if(!checkMetadata(metadataIn, silent = silent)){
        stop("Invalid metadata provided - stopping.")
    }
    
    dbCon <- postGresConnect(pgConnectionIn)
    on.exit(dbDisconnect(dbCon))
    
    if(!silent){
        cat("Writing metadata to database.\n")
    }
    
    #Assumes the table exists.                                                  To do
    
    #May be cleaner using paste to construct these, though would open up some   To do
    #   security issues if this was open to external users.
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
    
    dbSendQuery(dbCon, queryIn)
    
    if(!silent){
        cat("Metadata written.\n")
    }
}
