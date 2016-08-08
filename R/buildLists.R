buildPGConnection <- function(dbNameIn,
                              userNameIn,
                              passwordIn,
                              hostIn = 'localhost',
                              portIn = "5432",
                              silent = TRUE
                              ){
#Puts together a list of values to define a PostGreSQL connection in a format
#   that can be used by other functions (mainly through RPostgreSQL and ogr2ogr)
#
#Returns a list containing the connection info.
#
    ret <- c(dbname   = as.character(dbNameIn),
             host     = as.character(hostIn),
             port     = as.character(portIn),
             user     = as.character(userNameIn),
             password = as.character(passwordIn)
             )
    
    if(!silent){
        cat(sprintf("Creating PostGreSQL connection object as: %s\n",
                    paste(names(ret), ret, sep = " = ", collapse = ", ")
                    )
            )
    }
    
    return(ret)
}

buildMetaData <- function(tableIn,
                          schemaIn,
                          descriptionIn,
                          providerIn,
                          datasetIn,
                          licenseIn,
                          attributionIn,
                          date_acquiredIn,
                          date_generatedIn,
                          subsetIn = NA,
                          silent = FALSE
                          ){
#Puts together the metadata for a table in a standardised format, including some
#   information derived from the inputs.
#
#Returns a list containing the metadata for later use.
#
    ret <- list(table          = tableIn,
                schema         = schemaIn,
                subset         = subsetIn,
                description    = descriptionIn,
                provider       = providerIn,
                dataset        = datasetIn,
                date_generated = date_generatedIn,
                license        = licenseIn,
                attribution    = attributionIn,
                date_acquired  = date_acquiredIn,
                tablenameDq    = paste0("\"", schemaIn, "\".\"",tableIn,"\""),
                tablenameSq    = paste0("'", schemaIn, "'.'",tableIn,"'")
                )
    
    if(!silent){
        cat(sprintf("Creating metadata object as: %s\n",
                    paste(names(ret), ret, sep = " = ", collapse = ", ")
                    )
            )
    }
    
    return(ret)
}
