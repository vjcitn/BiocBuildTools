library(BiocBuildTools)
myset = readRDS("PackageSet_RELEASE_3_15.rds")
populate_local_gits(myset, "/media/volume/sdb/vince/REL_315_srcs")

