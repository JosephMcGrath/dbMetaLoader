loadingCleanup <- function(pgConnectionIn, metadataIn){
#Performs general clean-up operations on a table. Intended to be performed after
#   GIS data has been loaded into the database, but could be used elsewhere.
#
#Returns nothing.
#
    library("RPostgreSQL")

    dbCon <- postGresConnect(pgConnectionIn)
    on.exit(dbDisconnect(dbCon))
    
    vacuumStatement <- sprintf("VACUUM FULL %s;", metadataIn$tablenameDq)
    
    dbSendQuery(dbCon, vacuumStatement)
}
