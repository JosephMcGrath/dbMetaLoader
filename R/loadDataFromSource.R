loadDataFromSource <- function(metadataIn,
                               pgConnectionIn,
                               metadataTable = "\"metadata\".\"metainfo\""
                               ){
    
    for(i in 1:nrow(metadataIn)){
        
        curRow <- metadataIn[i, ]
        
        if(curRow$loaded == 0){
            #A more efficient approach would be to evaluate all the rows that   To do
            #   need some form of preprocessing/extraction and do that all at
            #   one time.
            #   Could load all the data from a single source at once.
            #Might be best to output progress more regularly.                   To do
            #   Maybe load up to X lines at a time?
            #   Or break after loading each row?
            newRow <- loadRow(curRow, pgConnectionIn, metadataTable)
            
            metadataIn[i, ] <- newRow
            
        } else {
            next
        }
    }
    
    return(metadataIn)
    
}

loadRow <- function(rowIn,
                    pgConnectionIn,
                    metadataTable = "\"metadata\".\"metainfo\"",
                    fullScan = TRUE
                    ){
    
    metaTemp <- buildMetaData(tableIn          = rowIn$table,
                              schemaIn         = rowIn$schema,
                              subsetIn         = rowIn$subset,
                              descriptionIn    = rowIn$description,
                              providerIn       = rowIn$provider,
                              datasetIn        = rowIn$dataset,
                              date_generatedIn = rowIn$date_generated,
                              licenseIn        = rowIn$license,
                              attributionIn    = rowIn$attribution,
                              date_acquiredIn  = rowIn$date_acquired#,
                              #silent = TRUE
                              )
    
    tempPath <- extractZip(rowIn$path)
    
    rowOut <- rowIn
    
    if(fullScan){
        rowOut$geom_type <- geometryString(tempPath)
        rowOut$multigeom <- isMulti(rowOut$geom_type)
    }
    
    #May want this returning something?                                         To do
    loadSpatialData(fileIn         = tempPath,
                    pgConnectionIn = pgConnectionIn,
                    metadataIn     = metaTemp,
                    multiGeom      = rowOut$multigeom,
                    srsIn          = rowIn$assignSRS,
                    append         = rowIn$append
                    )
    
    rowOut$loaded <- 1
    
    return(rowOut)
    
}

isMulti <- function(geomIn){
    
   ret <- as.logical(length(grep("(?i)MULTI", geomIn)))
   
   return(ret)
    
}

loadDataFromCsv <- function(csvPath,
                            pgConnectionIn,
                            metadataTable = "\"metadata\".\"metainfo\""){
    
    metadataUse <- readMetadataFromCsv(csvPath)
    
    ret <- loadDataFromSource(metadataUse, pgConnectionIn, metadataTable)
    
    writeMetadataCsv(ret, csvPath, TRUE)
    
}

writeMetadataCsv <- function(dataIn, pathIn, overwrite = FALSE){
    
    #Force boolean data types into 0/1                                          To do  
    dataIn$multigeom <- as.numeric(dataIn$multigeom)
    dataIn$append <- as.numeric(dataIn$append)      
    
    #Unless specified, append to an existing file rather than replacing it.
    newFile <- file.exists(pathIn) & !overwrite
    
    write.table(x = dataIn,
                file = pathIn,
                sep = ",",
                append = newFile,
                na = "",
                row.names = FALSE,
                col.names = !newFile
                )
    
    return(pathIn)
}
