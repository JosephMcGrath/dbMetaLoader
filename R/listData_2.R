listTables <- function(dirIn, maxLevel = 1, delimiter = "$"){
    
    #Manually work this out later if it's starting another level down?          To do
    initialLevel <- 0
    
    if(class(dirIn) == "character"){
        tableUse <- data.frame(dsn = dirIn,
                               type = "container",
                               container = NA,
                               tempContainer = NA,
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
            
            tableUse <- resolveRow(tableUse, i)
        }
        
        toResolve <- !tableUse[, "resolved"] &
                     tableUse[, "type"] == "container" &
                     tableUse[, "level"] <= maxLevel
        
    }
    
    return(tableUse)
}

resolveRow <- function(tableIn, rowIn){
    tableOut <- tableIn
    
    #1 If the source of the current data set is a zip file, extract it to a     To do
    #   temporary location.
    
    #2 Produce a list of all items in the current data set.                     To do
    #If it's a directory
    if(dir.exists(tableIn[rowIn, "dsn"])){
        filesIn <- list.files(tableIn[rowIn, "dsn"],
                              recursive = TRUE,
                              full.name = TRUE
                              )
    #If it's a zip files
    } else if(grep("(?i)\\.zip$", tableIn[rowIn, "dsn"])){
        filesIn <- unzip(tableIn[rowIn, "dsn"], list = TRUE)$Name
    }
    
    #3 Use that list to get a full list of all tables and containers.           To do
    #Get all of the tables
    tablesOut <- grep(spatialTables("table"), filesIn, value = TRUE)
    
    tablesOut <- tablesOut[!tablesOut %in% tableIn[, "dsn"]]
    
    #Get all the containers
    containersOut <- grep(spatialTables("container"), filesIn, value = TRUE)
    
    containersOut <- containersOut[!containersOut %in% tableIn[, "dsn"]]
    
    #4 Add those to the output table.                                           To do
    if(length(tablesOut) > 0){
        tableUse <- data.frame(dsn = tablesOut,
                               type = "table",
                               container = tableIn[rowIn, "dsn"],
                               tempContainer = tableIn[rowIn, "dsn"],
                               level = tableIn[rowIn, "level"],
                               resolved = FALSE,
                               stringsAsFactors  = FALSE
                               )
        tableOut <- rbind(tableOut, tableUse)
    }
    
    if(length(containersOut) > 0){
        tableUse <- data.frame(dsn = containersOut,
                               type = "container",
                               container = tableIn[rowIn, "dsn"],
                               tempContainer = tableIn[rowIn, "dsn"],
                               level = tableIn[rowIn, "level"] + 1,
                               resolved = FALSE,
                               stringsAsFactors  = FALSE
                               )
        tableOut <- rbind(tableOut, tableUse)
    }
    
    
    tableOut[rowIn, "resolved"] <- TRUE
    
    return(tableOut)
}
