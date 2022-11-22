 

#' helper for bco2df
#' @param x the warning or error component of BiocCheck
#' @param lcomd last commit date
#' @param chkd date of check
#' @param pkv version of checked package
#' @return data.frame with checkType
.bco2df = function (x, lcomd, chkd, pkv)
{
    ns = sapply(x, function(x) length(unlist(x)))
    ty = rep(names(x), ns)
    ans = data.frame(type = ty, message = unlist(x))
    ans$commit_date = lcomd
    ans$check_date = chkd
    ans$version = pkv
    rownames(ans) = NULL
    ans
}

#' convert BiocCheck output (just error and warning) to list of data.frames
#' @param x BiocCheck result
#' @return list of data frames errors, warnings
#' @export
bco2df = function(x) {
 stopifnot(all(c("error", "warning") %in% names(x)))
 lcomd = date_string(attr(x, "last_commit_date"))
 chkd = date_string(attr(x, "check_date"))
 pkv = attr(x, "pkgversion")
 errdf = data.frame(type="error", message="none", commit_date=lcomd, check_date=chkd, version=pkv)
 if (length(x$error)>0)
   errdf = .bco2df(x$error, lcomd, chkd, pkv)
 wrndf = data.frame(type="warning", message="none", commit_date=lcomd, check_date=chkd, version=pkv)
 if (length(x$warning)>0)
   wrndf = .bco2df(x$warning, lcomd, chkd, pkv)
 list(errors=errdf, warnings=wrndf)
}

#' use bco2df to acquire limited data frames from BiocCheck applied to packages
#' @import BiocParallel
#' @param sources.folder character(1) path to github checkouts of Bioc packages
#' @param bcchecks.destination character(1) path to a writeable folder where BiocCheck logs are written
#' @param bcobj.destination character(1) path to a folder where RDS files with data frames are written
#' @param BPPARAM defaults to bpparam()
#' @param BPOPTIONS defaults to bpoptions()
#' @examples
#' ps = PackageSet("parody")
#' tf = tempfile()
#' td = dir.create(tf)
#' populate_local_gits(ps, tf)
#' dir.create(bcdest <- tempfile("bcd"))
#' dir.create(bcodest <- tempfile("bco"))
#' get_bcc(tf, bcdest, bcodest)
#' readRDS(dir(bcodest, full=TRUE))
#' \dontrun{
#' set.seed(1234) # we shuffle packages to avoid restarting on a bad one
#' bcchecks.destination = dir.create(tempfile("bccheck"))
#' bcobj.destination = dir.create(tempfile("bcobj"))
#' spar = BiocParallel::SnowParam(3, threshold="DEBUG")
#' BiocParallel::bplog(spar) = TRUE
#' # make 777 BiocParallel::bplogdir(spar) = "~/BBS_space/BPLOGS_bc"
#' BiocParallel::bptimeout(spar) = 600
#' BiocParallel::register(spar)
#' # get_bcc(  ...
#' }
#' @export
get_bcc = function (sources.folder, bcchecks.destination, bcobj.destination, 
    BPPARAM = bpparam(), BPOPTIONS = bpoptions()) 
{
    goal = dir(sources.folder)
    done = gsub(".BiocCheck$", "", dir(bcchecks.destination))
    todo = setdiff(goal, done)
    print(head(todo))
    print(length(todo))
    todopaths = paste0(sources.folder, "/", todo)
    shuffle = function(x) sample(x, size = length(x), replace = FALSE)
    allbc3 = bplapply(shuffle(todopaths), function(x) {
        print(x)
        futile.logger::flog.info(paste0("'x' = ", x))
        futile.logger::flog.error(paste0("'x' = ", x))
        tmpans = try(BiocCheck::BiocCheck(x, checkDir = bcchecks.destination))
        attr(tmpans, "last_commit_date") <- last_commit_date(x)
        attr(tmpans, "check_date") <- Sys.time()
        attr(tmpans, "pkgversion") <- tmpans$metadata$PackageVersion
        dest = paste0(bcobj.destination, "/", paste0(basename(x), 
            "_chk.rds"))
        if (inherits(tmpans, "try-error")) {
            saveRDS(tmpans, dest)
            return(NULL)
            }
        ans = try(bco2df(tmpans))
        saveRDS(ans, dest)
        NULL
    }, BPPARAM = BPPARAM, BPOPTIONS = BPOPTIONS)
}

get_bcc_old = function(sources.folder, bcchecks.destination, bcobj.destination,
     BPPARAM=bpparam(), BPOPTIONS=bpoptions()) {
   goal = dir(sources.folder)
   done = gsub(".BiocCheck$", "", dir(bcchecks.destination))
   todo = setdiff(goal, done)
   print(head(todo))
   print(length(todo))
   todopaths = paste0(sources.folder, "/", todo)
   shuffle = function(x) sample(x, size=length(x), replace=FALSE)
   allbc3 = bplapply(shuffle(todopaths), function(x) {
      print(x)
      futile.logger::flog.info(paste0("'x' = ", x))
      futile.logger::flog.error(paste0("'x' = ", x))
      tmpans = BiocCheck::BiocCheck(x, checkDir=bcchecks.destination)
      attr(tmpans, "last_commit_date") <- last_commit_date(x)
      attr(tmpans, "check_date") <- Sys.time()
      ans = try(bco2df(tmpans))
      dest = paste0(bcobj.destination, "/", paste0(basename(x), "_chk.rds"))
      saveRDS(ans, dest)
      NULL
      }, BPPARAM=BPPARAM, BPOPTIONS=BPOPTIONS)
}
