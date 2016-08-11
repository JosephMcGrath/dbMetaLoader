extractMetadata <- function(dirIn,
                            maxLevel = 1,
                            delimiter = "$",
                            tmpdir = tempdir(),
                            cleanUp = TRUE
                            ){
    
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
        
        print(dataList[i, ])
        
        temp <- data.frame(path = delimitPath(dataList[i, "container"], dataList[i, "dsn"]),
                           schema = NA, #The containing folder. Maybe go for the last non-zip?
                           table = NA, #Name of the file itself.
                           subset = NA, #Maybe assign one after several layers of zip files?
                           description = NA, #Can't automate this reasonably.
                           provider = NA, #Similar to the schema.
                           dataset = NA, #Can't really automate this one? Maybe one layer down from the provider?
                           date_generated = NA, #Date of the file within the zip.
                           #format(tempContents[spatialData, ]$Date, format = "%Y/%m/%d")
                           license = NA,
                           attribution = NA,
                           date_acquired = NA, #Date of the highest level zip?
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
