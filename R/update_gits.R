#' attempt to identify packages that have had pushes since last collected
#' @param folder character(1) folder in which checked out packages reside
#' @note We use setwd(folder) because it seems we need to use system('git pull').
#' The principle is that `gert::git_pull` returns an error when there has been a change. (?)
#' When this happens, we enter the folder and use system('git pull') to get the changes.
#' @return a vector of package names for which changes were observed.
#' @export
update_gits = function(folder = "~/BBS_space/NOV5_FULL/fullgits_nov10") {
 owd = getwd()
 on.exit(setwd(owd))
 setwd(folder)
 allsrcs = dir(folder)
 npks = length(allsrcs)
 status = rep(0, npks)
 for (i in seq_len(npks)) {
   cat(allsrcs[i], "\n") 
   zz = try(gert::git_pull(repo=allsrcs[i])) 
   od=getwd() 
   if (inherits(zz, "try-error")) {
     status[i] = 1
     setwd(allsrcs[i]) 
     system("git pull") 
     setwd(od)
     }
   }
 allsrcs[which(status==1)]
}

