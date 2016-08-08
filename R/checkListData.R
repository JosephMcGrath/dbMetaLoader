checkListData <- function(dataName, dataIn, requiredFields, silent = FALSE){
#Checks that all of the required fields in the provided list exists. Designed to
#   be called by other functions to check that all the required attributes for a
#   given purpose e.g. the required information for a PostGres connection are
#   present.
#
#Returns TRUE if all attributes are present, FALSE if they are not.
#
    if(!(all(requiredFields %in% names(dataIn)) | silent)){
        cat(paste0(sprintf("The following %s is missing: ", dataName),
                   paste(requiredFields[!requiredFields %in% names(dataIn)],
                         collapse = ", "
                         ),
                   ".\n"
                   )
            )
    }
    
    return(all(requiredFields %in% names(dataIn)))
}

checkMetadata <- function(metadataIn,
                          requiredMeta = c("tablenameSq", "tablenameDq",
                                           "subset", "description",
                                           "provider", "dataset",
                                           "date_generated", "license",
                                           "attribution", "date_acquired"
                                           ),
                          silent = FALSE
                          ){
#Uses checkListData to make sure that all the metadata attributes that are to be
#   loaded into the database are present.
#
#Returns TRUE if all attributes are present, FALSE if they are not.
#
    return(checkListData("metadata",
                         metadataIn,
                         requiredMeta,
                         silent
                         )
           )
}

checkPGConnection <- function(pgConnectionIn,
                              requiredData = c("dbname", "host", "port", "user",
                                               "password"
                                               ),
                              silent = FALSE
                              ){
#Uses checkListData to make sure that all of the information required to form a
#   connection to PostGres is present. Doesn't actually check that the provided
#   information works, just that it is labeled correctly.
#
#Returns TRUE if all attributes are present, FALSE if they are not.
#
    return(checkListData("postgresql connection info",
                         pgConnectionIn,
                         requiredData,
                         silent
                         )
           )
}
