postGresConnect <- function(pgConnectionIn, silent = FALSE){
#Produces and RPostgreSQL connection from a standard list of connection
#   information.
#
#Returns the connection object.
#
    library("RPostgreSQL")
    
    if(!checkPGConnection(pgConnectionIn, silent = silent)){
        #How is an error at this level handled in the containing function?      To do
        stop("Incorrect connection info supplied - stopping.")
    }
    
    #How are errors in the actual connection handled?                           To do
    drv <- dbDriver("PostgreSQL")
    dbCon <- dbConnect(drv,
                       dbname =   pgConnectionIn["dbname"],
                       host =     pgConnectionIn["host"],
                       port =     pgConnectionIn["port"],
                       user =     pgConnectionIn["user"],
                       password = pgConnectionIn["password"]
                       )
    return(dbCon)
}
