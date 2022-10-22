library(BiocBuildTools)

version = "RELEASE_3_15"
versionTag = "3.15"

td = tempdir()

od = getwd()
setwd(td)

system("git clone https://git.bioconductor.org/admin/manifest.git")
setwd("manifest")

system(paste0("git checkout ", version))

allsoft = readLines("software.txt")
kp = grep("Package:", allsoft)
allsoft = allsoft[kp]
pks = gsub("Package: ", "", allsoft)

init = PackageSet(pks, versionTag)
wdeps = add_dependencies(init)
print(wdeps)
setwd(od)
target = paste0("PackageSet_", version)
saveRDS(wdeps, paste0(target, ".rds"))
