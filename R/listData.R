resolveZip <- function(tableIn, rowIn){
    contents <- unzip(tableIn[rowIn, "dsn"], list = TRUE)$Name
    
    tablesOut <- grep(spatialTables(type = "table"),
                      contents,
                      value = TRUE
                      )
    
    tablesOut <- tablesOut[!tablesOut %in% tableIn[, "dsn"]]
    
    if(length(tablesOut) > 0){
            newTables <- data.frame(dsn = paste(tableIn[rowIn, "dsn"],
                                                tablesOut,
                                                sep = "$"
                                                ),
                                    type = "table",
                                    level = tableIn[rowIn, "level"],
                                    resolved = FALSE,
                                    stringsAsFactors  = FALSE
                                    )
            tableIn <- rbind(tableIn, newTables)
    }
    
    return(tableIn)
}

resolveRow <- function(tableIn, rowIn){
    tableIn[rowIn, "resolved"] <- TRUE
    
    #Simple file systems
    if(dir.exists(tableIn[rowIn, "dsn"])){
        contents <- list.files(tableIn[rowIn, "dsn"],
                               full.name = TRUE,
                               recursive = TRUE
                               )
        
        #Deal with simple tables first
        tablesOut <- grep(spatialTables(type = "table"),
                          contents,
                          value = TRUE
                          )
        
        tablesOut <- tablesOut[!tablesOut %in% tableIn[, "dsn"]]
        
        if(length(tablesOut) > 0){
            newTables <- data.frame(dsn = tablesOut,
                                    type = "table",
                                    level = tableIn[rowIn, "level"],
                                    resolved = FALSE,
                                    stringsAsFactors  = FALSE
                                    )
            tableIn <- rbind(tableIn, newTables)
        }
        
        #Then deal with containers
        tablesOut <- grep(spatialTables(type = "container"),
                          contents,
                          value = TRUE
                          )
        
        tablesOut <- tablesOut[!tablesOut %in% tableIn[, "dsn"]]
        
        if(length(tablesOut) > 0){
            newTables <- data.frame(dsn = tablesOut,
                                    type = "container",
                                    level = tableIn[rowIn, "level"] + 1,
                                    resolved = FALSE,
                                    stringsAsFactors  = FALSE
                                    )
            tableIn <- rbind(tableIn, newTables)
        }
    } else {
        #Zip archives
        if(grep("(?i)\\.zip$", tableIn[rowIn, "dsn"])){
            tableIn <- resolveZip(tableIn, rowIn)
        }
    }
    
    return(tableIn)
}

listTables <- function(dataIn, maxLevel = 1){
    #Not quite sure the best way to handle a PostGreSQL data source yet         To do
    
    initialLevel <- rep(0, length(dataIn))
    
    #Convert the input to a data frame
    if(class(dataIn) == "character"){
        tableUse <- data.frame(dsn = dataIn,
                               type = rep("container", length(dataIn)),
                               level = initialLevel,
                               resolved = rep(FALSE, length(dataIn)),
                               stringsAsFactors  = FALSE
                               )
    } else if (class(dataIn) == "data.frame"){
        stop("Data frame support not yet implemented.")
    }
    
    toResolve <- !tableUse[, "resolved"] &
                 tableUse[, "type"] == "container" &
                 tableUse[, "level"] <= maxLevel
    
    #Filter out any non-unique rows                                             To do
    
    while(any(toResolve)){
        
        for(i in 1:nrow(tableUse)){
            if(!toResolve[i]){
                next
            }
            print(tableUse[i, ])
            
            tableUse <- resolveRow(tableUse, i)
        }
        
        toResolve <- !tableUse[, "resolved"] &
                     tableUse[, "type"] == "container" &
                     tableUse[, "level"] <= maxLevel
    }
    return(tableUse)
}
