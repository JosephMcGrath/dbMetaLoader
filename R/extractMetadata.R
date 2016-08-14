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
                           path = delimitPath(dataList[i, "container"], dataList[i, "dsn"]),
                           schema = NA,
                           table = NA,
                           subset = NA,
                           description = NA, #Can't automate this reasonably.
                           provider = NA, #Similar to the schema.
                           dataset = NA, #Can't really automate this one? Maybe one layer down from the provider?
                           date_generated = NA, #Date of the file within the zip.
                           #format(tempContents[spatialData, ]$Date, format = "%Y/%m/%d")
                           license = NA,
                           attribution = NA,
                           date_acquired = NA,
                           multigeom = 0, #Would be great to be able to automate this.
                           append = 0,
                           loaded = -1,
                           assignSRS = "EPSG:27700",
                           stringsAsFactors  = FALSE
                           )
        
        if(exists("ret")){
            ret <- rbind(ret, temp)
        } else {
            ret <- temp
        }
        
    }
    
    #Pull out most of the metadata
    topPaths <- unlist(lapply(strsplit(ret$path, delimiter, fixed = TRUE), head, 1))
    
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
        ret[i] <- paste(toupper(substring(textList[[i]], 1, 1)), tolower(substring(textList[[i]], 2)), sep = "", collapse = " ")
    }
    
    return(ret)
}
