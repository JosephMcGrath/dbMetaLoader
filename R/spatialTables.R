spatialTables <- function(){
#Builds a regular expression to filter out known spatial data formats.
#
#Returns the regular expression as a string.
#
    fileExtensions <- c("\\.shp$", #ESRI shapefiles
                        "\\.tab$", #MapInfo tab files
                        "\\.gml$", #GML files
                        "\\.kml$", #Google Earth KML
                        "\\.gpkg$",#GeoPackage
                        "\\.mif$"  #MapInfo interchange format
                        )
    
    ret <- paste0(fileExtensions, collapse = "|")
    ret <- sprintf("(?i)%s", ret)
    
    return(ret)
}