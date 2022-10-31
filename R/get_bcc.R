#' use bco2df to acquire limited data frames from BiocCheck applied to packages
#' @param sources.folder character(1) path to github checkouts of Bioc packages
#' @param bchecks.destination character(1) path to a writeable folder where BiocCheck logs are written
#' @param bcobj.destination character(1) path to a folder where RDS files with data frames are written
#' @examples
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
get_bcc = function(sources.folder, bcchecks.destination, bcobj.destination) {
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
      ans = try(bco2df(BiocCheck::BiocCheck(x, checkDir=bcchecks.destination))) 
      dest = paste0(bcobj.destination, "/", paste0(basename(x), "_chk.rds"))
      saveRDS(ans, dest)
      NULL
      })
}   
