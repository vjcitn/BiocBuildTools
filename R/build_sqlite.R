
#' use outputs of get_checks and get_bcc to build a database of all events of error or warning
#' @param dbname character(1) a sqlite database name
#' @param rcmdcheck_obj_dir character(1) path to a collection of rcmdcheck outputs
#' @param bccheck_obj_dir character(1) path to a collection of bco2df outputs, produced after running
#' BiocCheck on sources
#' @param exists_ok logical(1) passed to `status_db_init`
#' @export
build_sqlite_db = function( dbname = "chks.sqlite", rcmdcheck_obj_dir, bccheck_obj_dir,
    exists_ok = FALSE ) {
   
   allrcc = lapply(dir(rcmdcheck_obj_dir, full=TRUE), readRDS)
   bad = vapply(allrcc, function(x) inherits(x, "try-error"), logical(1))
   if (sum(bad)>0) allrcc = allrcc[-which(bad)]
   rcc_dfs = rcclist_to_dataframes(allrcc)
   status_db_init(dbname, rcc_dfs, exists_ok=exists_ok)
   
   allbcc = lapply(dir(bccheck_obj_dir, full=TRUE), readRDS)
   nms = gsub("_.*", "", dir(bccheck_obj_dir))
   bad = vapply(allbcc, function(x) inherits(x, "try-error"), logical(1))
   if (sum(bad)>0) {
     allbcc = allbcc[-which(bad)]
     nms = nms[-which(bad)]
   }
   names(allbcc) = nms
   
   bcclist_to_dataframes = function(bcclist) {
     nms = names(bcclist)
     allerrs = lapply(bcclist, "[[", "errors")
     erlens = vapply(allerrs, nrow, integer(1))
     allwrn = lapply(bcclist, "[[", "warnings")
     warens = vapply(allwrn, nrow, integer(1))
     ernms = rep(nms,erlens)
     wrnms = rep(nms,warens)
     errdf = do.call(rbind, allerrs)
     wrndf = do.call(rbind, allwrn)
     colnames(errdf) = c("type", "message")
     colnames(wrndf) = c("type", "message")
     errors = data.frame(package=ernms, type=errdf$type, message=errdf$message)
     warnings = data.frame(package=wrnms, type=wrndf$type, message=wrndf$message)
     list(errors=errors, warnings=warnings)
   }
   aa = bcclist_to_dataframes(allbcc)
   
    update_status_db(dbname, "BcChkERR", newdf=aa$errors)
    update_status_db(dbname, "BcChkWARN", newdf=aa$warnings)
}
   
#rcmdcheck_obj_dir = "./chks316"
#bccheck_obj_dir = "./bcobj"
#
#build_sqlite_db( "newchk2.sqlite", rcmdcheck_obj_dir = rcmdcheck_obj_dir,
#   bccheck_obj_dir = bccheck_obj_dir , exists_ok = TRUE)
