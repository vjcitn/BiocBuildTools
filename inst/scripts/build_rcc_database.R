
library(BiocBuildTools)
library(rcmdcheck)

   checks.destination = "~/BBS_space/chks316"

allc = lapply(dir(checks.destination, full=TRUE), readRDS)
pn = dir(checks.destination)

iserr = which(sapply(allc, inherits, "try-error"))
print(pn[iserr])

rccdfs = rcc_to_dataframes(allc[-iserr])

status_db_init("chks_3.16a.sqlite", rccdfs)
