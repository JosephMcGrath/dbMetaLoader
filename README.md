dbMetaLoader
=

A set of scripts to automate and extend the loading of data into PostGIS using ogr2ogr. Adds the capability to load data in alongside metadata from an external list of files such as a CSV.

This packages is mainly intended for my own use and as a learning tool for myself, but is open for anyone to use/modify if they want.

Installation
-

    install.packages(c("devtools", "gdalUtils", "RPostgreSQL"))
    library("devtools")
    install_github("JosephMcGrath/dbMetaLoader")
    library("dbMetaLoader")