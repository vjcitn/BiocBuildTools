library(BiocBuildTools)
library(RSQLite)
con = RSQLite::dbConnect(RSQLite::SQLite(), system.file(
   "sqlite/vjc3.sqlite", package="BiocBuildTools"))
#suppressWarnings({
#pk = readRDS(system.file("pkgnet/pnet.rds", package="BiocBuildTools"))
#})
BiocBuildTools::browse_checks(con, NULL)
