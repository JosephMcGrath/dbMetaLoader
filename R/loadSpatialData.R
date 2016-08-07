loadSpatialData <- function(fileIn, pgConnectionIn,
                            metadataIn,
                            metadataTable = "\"metadata\".\"metainfo\"",
                            append = FALSE,
                            multiGeom = FALSE){
#Loads a set of tables into the PostgreSQL database, attaching a metadata id to
#   each row to tie back to the metadata table.
#
    library("gdalUtils")
    
    mdId <- fetchMetadata(pgConnectionIn,
                          metadataIn,
                          metadataTable = "\"metadata\".\"metainfo\""
                          )
    
    if(is.na(mdId)){
        writeMetadata(pgConnectionIn,
                      metadataIn,
                      metadataTable = "\"metadata\".\"metainfo\""
                      )
        mdId <- fetchMetadata(pgConnectionIn,
                              metadataIn,
                              metadataTable = "\"metadata\".\"metainfo\""
                              )
    }
    
    print(mdId)
    
    dsnString <- sprintf("PG: host=%s port='%s' dbname=%s user='%s' password='%s'",
                         pgConnectionIn["host"],
                         pgConnectionIn["port"],
                         pgConnectionIn["dbname"],
                         pgConnectionIn["user"],
                         pgConnectionIn["password"]
                         )
    
    #Pulls apart the table name to get the file's name without an extension to
    #   use as the table name in SQL.
    sqlName <- head(unlist(strsplit(tail(unlist(strsplit(fileIn, "\\\\|/")), 1),
                                    "\\."
                                    )
                           ),
                           1
                    )
    
    temp <- list(src_datasource_name = fileIn,
                 dst_datasource_name = dsnString,
                 nln = metadataIn$tablenameDq,
                 f = "PostgreSQL",
                 sql = sprintf("SELECT *, %s AS metainfo_id FROM '%s'",
                               mdId,
                               sqlName
                               ),
                 dialect = "SQLITE",
                 verbose = TRUE
                 )
    
    #Add option to append to the main function
    if(append){
        temp$append <- TRUE
    }
    
    #Is there a way to automatically promote to multi geometry?                 To do
    if(multiGeom){
        temp$nlt <- "PROMOTE_TO_MULTI"
    }
    
    #DEFINITELY should not be assuming British Grid, should add a projection    To do
    #   override option to the CSV.
    if(TRUE){
        temp$a_srs = "EPSG:27700"
    }
    
    print(temp)
    #Does not seem to report error messages properly like this. Fails silently  To do
    do.call(ogr2ogr, temp)
    
    loadingCleanup(pgConnectionIn,metadataIn)
}
