#### Purpose: Use BiocBuildTools, recording processing events for repository construction

We are using us.gcr.io/anvil-gcr-public/anvil-rstudio-bioconductor-devel:3.13.0 as the custom runtime
environment

We obtain Bioconductor/AnVIL package with BiocManager::install

We get a current image of all relevant packages using AnVIL::install

On April 22, we needed to get seandavi/BiocPkgTools as it was not available from Bioconductor.  

Then we installed vjcitn/BiocBuildTools using BiocManager.

##### Ensure usability of the environment

We pull the github repo vjcitn/BiocBuildTools into Rstudio as a new git project.  We can then
knit vignettes/cicd1.Rmd to HTML to produce

![basic figure](https://storage.googleapis.com/bioc-anvil-images/Screenshot%20from%202021-04-22%2017-18-43.png)

##### Obtain the sources of the set of packages in current 'master' branch

Use the terminal to ```gsutil cp gs://bioc-anvil-images/pklist.rda .​``` and load the
content into your R session.

```
dir.create("gits")
ps = PackageSet(pklist)
populate_local_gits(ps, "./gits")
```

As of April 22 2021, this process led to 12G of content in `./gits`.

##### Build packages from source, recording all relevant events
