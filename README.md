# BiocBuildTools

This software and pkgdown web site was developed in conjunction with a
Chan-Zuckerberg Initiative Essential Open Source Software project,
"Bioconductor Build System", SVCF Grant 183436.

## Objectives

An overview can be found at the [Feb 2020 EOSS lightning talk slide set](https://docs.google.com/presentation/d/1EOrNomN7hVOfvZFR06bV6I4aZR9sQo9CCyDMlcg64Wk/edit?usp=sharing)

The project plan is


The basic goals of this work are (derived from EOSS grant text):
 
- G1: Source acquisition.  All Bioconductor packages are managed in git repositories owned by the Bioconductor project.  All contributors consent to this arrangement, which allows core developers to ensure that versioning protocols are fully implemented, and to provide solutions to systemic problems when such arise.  Contributors receive push privileges to the main repository and may use git and/or github or other repository management systems as they prefer.  We will obtain the full list of packages in current release (a fixed list) and devel (a rolling list as contributions are admitted) using the recently developed BiocPkgTools package.  With these lists, git repositories for sources are cloned to the BBS main node.
- G2: Builder infrastructure provisioning.  R, Bioconductor packages, and CRAN packages that they depend upon, make use of various runtime libraries for numerical analysis, graphics, text processing.  At present we count over 1600 Ubuntu packages required to build R, CRAN dependencies of Bioconductor packages, and the Bioconductor packages themselves.  This endowment process for the main build machine will be governed by ansible cookbooks.
- G3: R image for builder.  An instance of R capable of building all Bioconductor software packages will itself include several thousand CRAN packages.  As Bioconductor package dependencies are resolved, the R image for the builder will include an increasing number of Bioconductor packages.  The resulting image of R should be highly stable, in that relatively few packages will be changing each time the package set is built.
- G4: Tarball and log generation.  R CMD build is run on each source package and the build log is recorded for presentation on the build system platform.  R CMD check is run to ensure all examples, unit tests, and vignettes succeed, to verify that basic development guidelines are met, and to report on guideline non-compliance and on exceptional events.  The check logs are recorded for presentation on the build system platform.  See Figure 1 for an overview of the platform-specific package build/check event counts.
- G5: Failure analysis, communication, and repair.  Package failures may arise from developer error, problems propagated from faults in other packages, and from problems in BBS environment and infrastructure.  

