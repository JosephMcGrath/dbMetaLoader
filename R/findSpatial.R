findSpatial <- function(dirIn, csvPath, silent = FALSE){
#Searches for spatial data in the path provided and extracts relevant metadata
#   into a csv for storage.
#
#Returns nothing
#
#To do:
# * Return a data frame that could be passed to another function for writing so
#   that it could go to a csv or a database table.
#
    filesIn <- list.files(dirIn, full.name = TRUE, recursive = TRUE)
    
    #Currently not actually doing anything with non-zipped files.
    rawTables <- grep(spatialTables(), filesIn, value = TRUE)
    zipPaths <- grep("(?i)\\.zip$", filesIn, value = TRUE)
    
    n <- length(append(rawTables, zipPaths))
    
    outputRows <- data.frame(path = append(rawTables, zipPaths),
                             schema = character(n),
                             table = character(n),
                             subset = rep(NA, n),
                             description = rep(NA, n),
                             provider = character(n),
                             dataset = rep(NA, n),
                             date_generated = character(n),
                             license = rep(NA, n),
                             attribution = rep(NA, n),
                             date_acquired = character(n),
                             append = rep(0, n),
                             multigeom = rep(0, n),
                             loaded = rep(-1, n),
                             stringsAsFactors = FALSE
                             )
    
    for(i in 1:length(zipPaths)){
        tempContents <- unzip(zipPaths[i], list = TRUE)
        
        spatialData <- grep(spatialTables(), tempContents$Name)
        if(length(spatialData) != 1){
            next
        }
        
        providerTry <- tail(strsplit(zipPaths[i], "\\\\|/")[[1]], 2)[1]
        schemaTry <- gsub(" ", "_", tolower(providerTry))
        
        tableTry <- tail(strsplit(zipPaths[i], "\\\\|/")[[1]], 1)
        tableTry <- gsub("(?i)\\.zip$", "", tolower(tableTry))
        
        temp <- c(path = zipPaths[i],
                  schema = schemaTry,
                  table = tableTry,
                  subset = NA,
                  description = NA,
                  provider = providerTry,
                  dataset = NA,
                  date_generated = format(tempContents[spatialData, ]$Date,
                                          format = "%Y/%m/%d"
                                          ),
                  license = NA,
                  attribution = NA,
                  date_acquired = format(file.info(zipPaths[i])$ctime,
                                         format = "%Y/%m/%d"
                                         ),
                  multigeom = 0,
                  append = 0,
                  loaded = -1
                  )
        outputRows[i, ] <- temp
    }
    
    outputRows <- outputRows[outputRows[, 2] != "", ]
    
    newFile <- file.exists(csvPath)
    
    #If the file exists, make sure all the 
    if(newFile){
        prevCSV <- read.csv(csvPath, stringsAsFactors = FALSE)$path
        outputRows <- outputRows[!outputRows$path %in% prevCSV, ]
    }
    
    write.table(x = outputRows,
                file = csvPath,
                sep = ",",
                append = newFile,
                na = "",
                row.names = FALSE,
                col.names = !newFile
                )
}
