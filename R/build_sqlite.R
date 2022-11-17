#' helper function for extracting BiocCheck outputs
#' @param bcclist list of outputs of BiocCheck::BiocCheck
#' @export
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

#' use outputs of get_checks and get_bcc to build a database of all events of error or warning
#' @param dbname character(1) a sqlite database name
#' @param rcmdcheck_obj_dir character(1) path to a collection of rcmdcheck outputs
#' @param bccheck_obj_dir character(1) path to a collection of bco2df outputs, produced after running
#' BiocCheck on sources
#' @param exists_ok logical(1) passed to `status_db_init`
#' @export
build_sqlite_db = function( dbname = "chks.sqlite", rcmdcheck_obj_dir, bccheck_obj_dir,
    exists_ok = FALSE ) {
   
   allrcc = lapply(dir(rcmdcheck_obj_dir, full.names=TRUE), readRDS)
   bad = vapply(allrcc, function(x) inherits(x, "try-error"), logical(1))
   if (sum(bad)>0) allrcc = allrcc[-which(bad)]
   rcc_dfs = rcclist_to_dataframes(allrcc)
   status_db_init(dbname, rcc_dfs, exists_ok=exists_ok)
   
   allbcc = lapply(dir(bccheck_obj_dir, full.names=TRUE), readRDS)
   nms = gsub("_.*", "", dir(bccheck_obj_dir))
   bad = vapply(allbcc, function(x) inherits(x, "try-error"), logical(1))
   if (sum(bad)>0) {
     allbcc = allbcc[-which(bad)]
     nms = nms[-which(bad)]
   }
   names(allbcc) = nms
   
   aa = bcclist_to_dataframes(allbcc)
   
    update_status_db(dbname, "BcChkERR", newdf=aa$errors)
    update_status_db(dbname, "BcChkWARN", newdf=aa$warnings)
}
   
#rcmdcheck_obj_dir = "./chks316"
#bccheck_obj_dir = "./bcobj"
#
#build_sqlite_db( "newchk2.sqlite", rcmdcheck_obj_dir = rcmdcheck_obj_dir,
#   bccheck_obj_dir = bccheck_obj_dir , exists_ok = TRUE)

make_df_component = function(rcclist, component="notes") {
  pks = sapply(rcclist, "[[", "package")
  nol = lapply(rcclist, "[[", component)
  date_commit = vapply(rcclist, function(x) {
            date_string(attr(x, "last_commit_date")) }, character(1))
  date_check = vapply(rcclist, function(x) {
            date_string(attr(x, "check_date")) }, character(1))
  if (component == "description") {
   nol = lapply(nol, function(x) strsplit(x, "\n")[[1]])
  }
  nn = sapply(nol,length)
  zn = which(nn==0)
  if (length(zn)>0)
    nol[zn] = paste0("NO ", toupper(component))
  nls = sapply(nol, length)
  npk = rep(pks, nls)
  ndcom = rep(date_commit, nls)
  ndche = rep(date_check, nls)
  ans = data.frame(package=npk, tmp=unlist(nol), date_commit=ndcom, date_check=ndche, stringsAsFactors=FALSE)
  names(ans)[2] = component
  ans
}

date_string = function(x) as.character(as.Date(x, origin="1970-01-01"))

#' produce list of data.frames for storage of R CMD check results in SQLite
#' @param rcclist list of results of rcmdcheck::rcmdcheck
#' @examples
#' data(demo_rcmdcheck_out)
#' dfs = rcclist_to_dataframes(demo_rcmdcheck_out)
#' names(dfs)
#' @export
rcclist_to_dataframes = function(rcclist) {
  pks = vapply(rcclist, function(x) x$package, character(1))
  vers = vapply(rcclist, function(x) x$version, character(1)) # might need to coerce Nov 4 2022
  notes_df = make_df_component(rcclist, component="notes")
  err_df = make_df_component(rcclist, component="errors")
  warn_df = make_df_component(rcclist, component="warnings")
  inst_df = make_df_component(rcclist, component="install_out")
  desc_df = make_df_component(rcclist, component="description")
  date_commit = vapply(rcclist, function(x) {
            date_string(attr(x, "last_commit_date")) }, character(1))
  date_check = vapply(rcclist, function(x) {
            date_string(attr(x, "check_date")) }, character(1))
  list(basic = data.frame(package=pks, version=vers, date_commit=date_commit,
         date_check=date_check, stringsAsFactors=FALSE),
   notes = notes_df, warnings=warn_df, errors=err_df, inst=inst_df, desc=desc_df)
}

