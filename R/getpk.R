#' use HTTPS to clone a package from Bioconductor git
#' @param x character(1) package name expected to be a Bioconductor package in git
#' @return result of system()
#' @examples
#' td = tempdir()
#' wd = getwd()
#' setwd(td)
#' lk = getpk("parody")
#' if (FALSE) lk2 = try(getpk("parody_z")) # should fail
#' setwd(wd)
#' @export
getpk = function (x) 
system(sprintf("git clone --depth 1 https://git.bioconductor.org/packages/%s.git", 
    x))

#' get vector of Bioc software package names
#' @param manifest_repo_dir character(1) folder where git@git.bioconductor.org:admin/manifest has been cloned
#' @param release character(1) git checkout will be run for this tag
#' @return character vector
bioc_software_packagelist_old = function(manifest_repo_dir, release="master") {
	curd = getwd()
	on.exit(setwd(curd))
	setwd(manifest_repo_dir)
	system(paste0("git checkout ", release))
	system("git pull")
	txt = readLines("software.txt")[-1] # drop header
	bl = which(nchar(txt)==0)
	txt = txt[-bl]
	txt = gsub("Package: ", "", txt)
	txt = gsub(" ", "", txt)
	txt
}

bioc_software_packagelist_older = function() {
	ddf = BiocPkgTools::buildPkgDependencyDataFrame()
	unique(ddf$Package)
}



