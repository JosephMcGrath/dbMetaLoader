listTables <- function(dirIn,
                       maxLevel = 1,
                       delimiter = "$",
                       tmpdir = tempdir(),
                       cleanUp = TRUE
                       ){
    
    #Manually work this out later if it's starting another level down?          To do
    initialLevel <- 0
    
    if(class(dirIn) == "character"){
        tableUse <- data.frame(dsn = dirIn,
                               type = "container",
                               container = NA,
                               tempCon = NA,
                               level = initialLevel,
                               resolved = FALSE,
                               stringsAsFactors  = FALSE
                               )
    } else {
        stop("dirIn must be a character vector.")
    }
    
    toResolve <- !tableUse[, "resolved"] &
                 tableUse[, "type"] == "container" &
                 tableUse[, "level"] <= maxLevel
    
    while(any(toResolve)){
        
        for(i in 1:nrow(tableUse)){
            if(!toResolve[i]){
                next
            }
            
            tableUse <- resolveRow(tableUse, i, tmpdir = tmpdir)
        }
        
        toResolve <- !tableUse[, "resolved"] &
                     tableUse[, "type"] == "container" &
                     tableUse[, "level"] <= maxLevel
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
    
    return(tableUse)
}

resolveRow <- function(tableIn, rowIn, tmpdir = tempdir()){
    tableOut <- tableIn
    
    #1 If the source of the current data set is a zip file, extract it to a
    #   temporary location.
    if(length(grep("(?i)\\.zip$", tableIn[rowIn, "tempCon"]))){
        
        dirOut <- tempfile(pattern = "temp_extract_", tmpdir = tmpdir)
        
        cat("\n", dirOut, "\n\n")
        
        toUnzip <- unzip(tableOut[rowIn, "tempCon"], list = TRUE)$Name
        toUnzip <- grep(spatialTables(c("table", "tempCon")),
                        toUnzip,
                        value = TRUE
                        )
        
        unzip(tableOut[rowIn, "tempCon"], files = toUnzip, exdir = dirOut)
        
        #Update the tempCon to represent the folder it's being stored in.
        sharedSource <- tableOut[, "tempCon"] == tableOut[rowIn, "tempCon"]
        sharedSource <- sharedSource & !is.na(sharedSource)
        
        tableOut[sharedSource, "tempCon"] <- dirOut
        
        print(tableOut[rowIn, ])
        
    }
    
    #2 Produce a list of all items in the current data set.
    
    if(!is.na(tableOut[rowIn, "tempCon"])){
        pathUse <- file.path(tableOut[rowIn, "tempCon"], tableOut[rowIn, "dsn"])
    } else {
        pathUse <- tableOut[rowIn, "dsn"]
    }
    print(pathUse)
    
    #If it's a directory
    if(dir.exists(pathUse)){
        filesIn <- list.files(pathUse,
                              recursive = TRUE#,
                              #full.name = TRUE
                              )
    #If it's a zip files
    } else if(grep("(?i)\\.zip$", pathUse)){
        filesIn <- unzip(pathUse, list = TRUE)$Name
    }
    
    #3 Use that list to get a full list of all tables and containers.
    #Get all of the tables
    tablesOut <- grep(spatialTables("table"), filesIn, value = TRUE)
    
    tablesOut <- tablesOut[!tablesOut %in% tableOut[, "dsn"]]
    
    #Get all the containers
    containersOut <- grep(spatialTables("container"), filesIn, value = TRUE)
    
    containersOut <- containersOut[!containersOut %in% tableOut[, "dsn"]]
    
    #4 Add those to the output table.
    if(length(tablesOut) > 0){
        tableUse <- data.frame(dsn = tablesOut,
                               type = "table",
                               container = delimitPath(tableOut[rowIn,
                                                                "container"
                                                                ],
                                                       tableOut[rowIn, "dsn"]
                                                       ),
                               tempCon = delimitPath(tableOut[rowIn, "tempCon"],
                                                     tableOut[rowIn, "dsn"]
                                                     ),
                               level = tableOut[rowIn, "level"],
                               resolved = FALSE,
                               stringsAsFactors  = FALSE
                               )
        tableOut <- rbind(tableOut, tableUse)
    }
    
    if(length(containersOut) > 0){
        tableUse <- data.frame(dsn = containersOut,
                               type = "container",
                               container = delimitPath(tableOut[rowIn,
                                                                "container"
                                                                ],
                                                       tableOut[rowIn, "dsn"]
                                                       ),
                               tempCon = delimitPath(tableOut[rowIn, "tempCon"],
                                                     tableOut[rowIn, "dsn"]
                                                     ),
                               level = tableOut[rowIn, "level"] + 1,
                               resolved = FALSE,
                               stringsAsFactors  = FALSE
                               )
        tableOut <- rbind(tableOut, tableUse)
    }
    
    
    tableOut[rowIn, "resolved"] <- TRUE
    
    return(tableOut)
}

delimitPath <- function(currentPath, newPath, delimiter = "$"){
    if(is.na(currentPath)){
        ret <- newPath
    } else {
        if(length(grep(spatialTables("container"),
                       currentPath,
                       invert = TRUE
                       )
                  )
           ){
            ret <- file.path(currentPath, newPath)
        } else {
            ret <- paste(currentPath, newPath, sep = delimiter)
        }
    }
    
    return(ret)
}
