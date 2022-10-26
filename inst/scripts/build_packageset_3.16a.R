
library(BiocBuildTools)

branch = "master"  # no RELEASE_3_16 yet Oct 25
versionTag = "3.16"
extraTag = "a" # turn off when mature

stopifnot(BiocManager::version() == versionTag)

td = tempdir()

od = getwd()
setwd(td)

system("git clone https://git.bioconductor.org/admin/manifest.git")
setwd("manifest")

system(paste0("git checkout ",  branch))

allsoft = readLines("software.txt")
kp = grep("Package:", allsoft)
allsoft = allsoft[kp]
pks = gsub("Package: ", "", allsoft)

init = PackageSet(pks, versionTag, branchname=branch)
wdeps = add_dependencies(init)
print(wdeps)
setwd(od)
target = paste0("PackageSet_", versionTag)
saveRDS(wdeps, paste0(target, extraTag, ".rds"))
