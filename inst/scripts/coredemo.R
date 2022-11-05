

packs = c("SummarizedExperiment", "SingleCellExperiment", "GenomicRanges", "IRanges",
  "Rsamtools", "DelayedArray", "BiocParallel", "rhdf5", "AnnotationHub", "BiocFileCache",
  "ExperimentHub", "curatedTCGAData", "VariantAnnotation", "HDF5Array", "GenomeInfoDb")

library(BiocBuildTools)
civ = Sys.getenv("CI")
stopifnot(civ=="true")

ps = PackageSet(packs, biocversion="3.16", branchname="master")
ps = add_dependencies(ps)

td = tempfile("litdemo")
dir.create(td)

populate_local_gits(ps, td)

chkdest = tempfile("corerchks")
dir.create(chkdest)

bdest = tempfile("corebcchks")
dir.create(bdest)

bobdest = tempfile("corebobjs")
dir.create(bobdest)

library(BiocParallel)
spar = SnowParam(20)
register(spar)

get_checks(ps, sources.folder=td, checks.destination=chkdest)

get_bcc(sources.folder=td, bcchecks.destination=bdest, bcobj.destination=bobdest,
   BPOPTIONS=bpoptions(exports=c("chkdest", "bdest", "bobdest")))

build_sqlite_db("coredemo3.sqlite", rcmd=chkdest, bcc=bobdest)

