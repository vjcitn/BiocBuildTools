#' initialize a status database
#' @param sqlite_filename character(1) typically new path
#' @param named_df_list a list of data.frame instances
#' @param exists_ok logical(1) if TRUE will work with existing db
#' @export
status_db_init = function(sqlite_filename, named_df_list, exists_ok=FALSE) {
  if (!exists_ok & file.exists(sqlite_filename)) stop(sqlite_filename, " exists and exists_ok = FALSE")
  con = dbConnect(RSQLite::SQLite(), sqlite_filename)
  on.exit(dbDisconnect(con))
  nms = names(named_df_list)
  for (i in nms)
    dbWriteTable(con, i, named_df_list[[i]], append=TRUE)
  invisible(nms)
}
