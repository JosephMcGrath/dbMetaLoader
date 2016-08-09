spatialTables <- function(type = "table"){
#Builds a regular expression to filter out known spatial data formats.
#
#Returns the regular expression as a string.
#
#To do:
# * Extend the capabilities to find all parts of a multipart file?
#   I'm thinking along the lines of a findPartsOf argument that creates a regex
#   string that would find the relevant other files (maybe have an option to
#   only pull out data files, leaving indexes ect behind to save space).
#
    fileExtensions <- c()
    
    if("table" %in% type){
        fileExtensions <- append(fileExtensions,
                                 c("\\.shp$", #ESRI shapefiles
                                   "\\.tab$", #MapInfo tab files
                                   "\\.gml$", #GML files
                                   "\\.kml$", #Google Earth KML
                                   "\\.gpkg$",#GeoPackage
                                   "\\.mif$"  #MapInfo interchange format
                                   )
                                  )
    }

    if("container" %in% type){
        fileExtensions <- append(fileExtensions,
                                 c("\\.zip$" #Windows Zip archive
                                   )
                                 )
    }
    
    ret <- paste0(fileExtensions, collapse = "|")
    ret <- sprintf("(?i)%s", ret)
    
    return(ret)
}
