extractMetadata <- function(dirIn,
                            maxLevel = 1,
                            delimiter = "$",
                            tmpdir = tempdir(),
                            cleanUp = TRUE
                            ){
    
    #Use on.exit() for cleanUp?
    
    #Create list of all files
    dataList <- listTables(dirIn = dirIn,
                           maxLevel = maxLevel,
                           delimiter = "$",
                           tmpdir = tempdir(),
                           cleanUp = FALSE
                           )
    
    toResolve <- !dataList[, "resolved"] &
                 dataList[, "type"] == "table" &
                 dataList[, "level"] <= maxLevel
    
    #Cycle through each, extracting the metadata from them
    for(i in 1:nrow(dataList)){
        if(!toResolve[i]){
            next
        }
        
        #Also pull over rowid for each so it can be traced back
        temp <- data.frame(scan_row = i,
                           path = delimitPath(dataList[i, "container"],
                                              dataList[i, "dsn"]
                                              ),
                           schema = NA,
                           table = NA,
                           subset = NA,
                           description = NA,
                           provider = NA,
                           dataset = NA,
                           date_generated = NA,
                           license = NA,
                           attribution = NA,
                           date_acquired = NA,
                           multigeom = 0,
                           append = 0,
                           loaded = -1,
                           assignSRS = "EPSG:27700",
                           stringsAsFactors  = FALSE
                           )
        
        #Pull out the time generated
        if(length(grep("(?i)\\.zip$", dataList[i, "tempCon"]))){
            contents <- unzip(dataList[i, "tempCon"], list = TRUE)
            contents <- contents[contents$Name == dataList[i, "dsn"], "Date"]
            temp$date_generated <- format(contents, format = "%Y/%m/%d")
        }
        
        if(dir.exists(dataList[i, "tempCon"])){
            temp$date_generated <- format(file.info(temp$path)$ctime,
                                          format = "%Y/%m/%d"
                                          )
        }
        
        if(exists("ret")){
            ret <- rbind(ret, temp)
        } else {
            ret <- temp
        }
        
    }
    
    #Pull out most of the metadata
    topPaths <- unlist(lapply(strsplit(ret$path,
                                       delimiter,
                                       fixed = TRUE
                                       ),
                              head, 1
                              )
                       )
    
    acquiredDates <- format(file.info(topPaths)$ctime, format = "%Y/%m/%d")
    
    noExtension <- gsub(spatialTables(c("table", "container")), "", topPaths)
    
    tableNames <- unlist(lapply(strsplit(noExtension, "\\\\|/"), tail, 1))
    tableNames <- tolower(tableNames)
    tableNames <- gsub(" ", "_", tableNames)
    
    schemaNames <- lapply(strsplit(noExtension, "\\\\|/"), tail, 2)
    schemaNames <- unlist(lapply(schemaNames, head, 1))
    schemaNames <- tolower(schemaNames)
    schemaNames <- gsub(" ", "_", schemaNames)
    
    providerNames <- providerFormat(schemaNames)
    
    ret$date_acquired <- acquiredDates
    ret$table <- tableNames
    ret$schema <- schemaNames
    ret$provider <- providerNames
    
    #Remove all temporary files
    if(cleanUp){
        toRemove <- grep("(\\\\||/)temp_extract_",
                         list.files(tmpdir,
                                    recursive = TRUE,
                                    full.name = TRUE
                                    ),
                         value = TRUE
                         )
        
        file.remove(toRemove)
    }
    
    return(ret)
}

providerFormat <- function(textIn){
    textUse  <- gsub("_", " ", textIn)
    
    textList <- strsplit(textUse, " ")
    
    ret <- rep("", length(textList))
    for(i in 1:length(textList)){
        ret[i] <- paste(toupper(substring(textList[[i]], 1, 1)),
                        tolower(substring(textList[[i]], 2)),
                        sep = "",
                        collapse = " "
                        )
    }
    
    return(ret)
}

extractToCsv <- function(dirIn, pathOut, maxLevel = 1, overwrite = FALSE){
    
    outputRows <- extractMetadata(dirIn, maxLevel = maxLevel, cleanUp = TRUE)
    
    newFile <- file.exists(pathOut) & !overwrite
    
    write.table(x = outputRows,
                file = pathOut,
                sep = ",",
                append = newFile,
                na = "",
                row.names = FALSE,
                col.names = !newFile
                )
    
    return(pathOut)
}
