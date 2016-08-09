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
    
    #3 Use that list to get a full list of all tables and containers.           To do
    
    #4 Add those to the output table.                                           To do
    
    tableOut[rowIn, "resolved"] <- TRUE
    
    return(tableOut)
}
