loadFromCsv <- function(fileIn, pgConnectionIn, silent = FALSE){
#Extracts metadata information from a csv and uses that, along with a file path
#   to load GIS data from external tables into the PostGreSQL database.
#
#Does not return anything, though updates the CSV file.
#
#To do:
# * Split out into separate parts to 1 - fetch the metadata.
#                                    2 - load the data.
#                                    3 - update the external table.
#
    metaTableFull <- read.csv(pathIn, stringsAsFactors = FALSE)
    metaTable <- metaTableFull[metaTableFull$loaded == 0, ]
    
    #Not a huge fan of using return like this
    if(nrow(metaTable) <= 0){
        return(TRUE)
    }
    
    #Bud out the actual processing part so it can be embedded in a try block.   To do    
    for(i in 1:nrow(metaTable)){
        metaTemp <- buildMetaData(tableIn = metaTable[i, "table"],
                                  schemaIn = metaTable[i, "schema"],
                                  subsetIn = metaTable[i, "subset"],
                                  descriptionIn = metaTable[i, "description"],
                                  providerIn = metaTable[i, "provider"],
                                  datasetIn = metaTable[i, "dataset"],
                                  date_generatedIn = metaTable[i, "date_generated"],
                                  licenseIn = metaTable[i, "license"],
                                  attributionIn = metaTable[i, "attribution"],
                                  date_acquiredIn = metaTable[i, "date_acquired"]
                                  )
        
        #If table already exists and shouldn't be appended - raise and error    To do
        appendData <- metaTable[i, "append"]
        if(appendData == 1){
            appendData <- TRUE
        } else {
            appendData <- FALSE
        }
        multiGeom <- metaTable[i, "multigeom"]
        if(multiGeom == 1){
            multiGeom <- TRUE
        } else {
            multiGeom <- FALSE
        }
        
        if(!silent){
            cat(sprintf("Loading table: %s\n", metaTemp$tablenameDq))
        }
        
        #Allow the use of a multi-part zip file via a delimiter                 To do
        if(length(grep("(?i)\\.zip$", metaTable[i, "path"])) > 0){
            if(!silent){
                cat(sprintf("Extracting zip file from: %s\n",
                            metaTable[i, "path"]
                            )
                    )
            }
            
            metaTable[i, "path"] <- tempUnzip(metaTable[i, "path"])
            
            if(!silent){
                cat(sprintf("Zip file now located at: %s\n",
                            metaTable[i, "path"])
                    )
            }
            
            loadSpatialData(fileIn = metaTable[i, "path"],
                            pgConnectionIn = pgCon,
                            metadataIn = metaTemp,
                            append = appendData,
                            multiGeom = multiGeom
                            )
            
            #May just be quicker to use tempdir()?
            tempFolder <- gsub(tail(strsplit(metaTable[i, "path"],
                                             "\\\\|/"
                                             )[[1]],
                                    1
                                    ),
                               "",
                               metaTable[i, "path"]
                               )
            unlink(list.files(tempFolder, full.name = TRUE))
            
            metaTableFull[metaTableFull["table"] == metaTable[i, "table"], "loaded"] <- 1
        } else {
            if(!silent){
                cat(sprintf("Loading file from: %s\n", metaTable[i, "path"]))
            }
            
            loadSpatialData(fileIn = metaTable[i, "path"],
                            pgConnectionIn = pgCon,
                            metadataIn = metaTemp,
                            append = appendData
                            )
            
            metaTableFull[metaTableFull["table"] == metaTable[i, "table"], "loaded"] <- 1
        }
        
        #Vacuum the result & any other clean-up needed.
        write.csv(metaTableFull, pathIn, row.names = FALSE, na = "")
    }
}
