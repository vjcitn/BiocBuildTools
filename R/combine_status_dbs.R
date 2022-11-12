#' add records from 'incoming' database to 'target' database
#' @import RSQLite
#' @param target character(1) path to SQLite database to receive new records
#' @param incoming character(1) path to SQLite database with new records
#' @export
combine_status_dbs = function(target, incoming) {
  stopifnot(file.exists(target))
  stopifnot(file.exists(incoming))
  contarg = dbConnect(SQLite(), target)
  coninc = dbConnect(SQLite(), incoming)
  alldfsinc = lapply(dbListTables(coninc), function(x) dbReadTable(coninc, x))
  names(alldfsinc) = dbListTables(coninc)
  tnames = dbListTables(coninc)
  for (i in tnames) {
    BiocBuildTools::update_status_db(target, i, NULL,  newdf=alldfsinc[[i]]); Sys.sleep(2); cat(".") 
  }
  dbDisconnect(contarg)
  dbDisconnect(coninc)
}
