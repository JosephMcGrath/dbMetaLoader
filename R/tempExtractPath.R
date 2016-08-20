tempExtractPath <- function(code = "temp", diruse = tempdir()){

    ret <- tempfile(pattern = code, tmpdir = diruse)
    
    return(ret)
    
}
