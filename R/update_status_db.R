#' add a dataframe, optionally purging related obsolete records, to a SQLite database
#' @import RSQLite
#' @param sqlite_filename character(1) filename of SQLite database; database must exist
#' @param tablename character(1) name of table to operate on
#' @param dropfield character(1) or NULL, the field used to select records to delete
#' @param dropvalue character(1) only used if `dropfield` is not NULL, value used to
#' select records for deletion
#' @param newdf data.frame conforming to the table in `tablename` if such exists
#' @note Connects to db with flag SQLITE_RW and disconnects on exit.
#' @export 
update_status_db = function (sqlite_filename, tablename, dropfield = NULL, dropvalue,
    newdf)
{
    wcon = RSQLite::dbConnect(SQLite(), sqlite_filename)
    on.exit(dbDisconnect(wcon))
    allt = dbListTables(wcon)
    if (length(allt) > 0) {
        if (tablename %in% allt) {
            chk = dbGetQuery(wcon, sprintf("SELECT * from %s limit 1",
                tablename))
            nn = names(newdf)
            stopifnot(all(sort(nn) == sort(names(chk))))
        }
        if (!is.null(dropfield)) {
            newq = sprintf("DELETE from %s where %s = '%s'",
                tablename, dropfield, dropvalue)
            dbExecute(wcon, newq)
        }
    }
    dbWriteTable(wcon, tablename, newdf, append = TRUE)
}

