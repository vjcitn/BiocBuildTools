#' process outputs of tbchecks and get_pnet to run browse_checks
#' @param dbpath character(1) a SQLite database will be produced here
#' @param startup logical(1) defaults to TRUE; passed to `status_db_init`
#' @param serialize_pnet logical(1) if TRUE emits list of pkgnet outputs as RDS
#' @param pnet_obj_name character(1) defaults to "pnet.rds"
#' @param run_app logical(1) defaults to TRUE; runs `browse_checks` on available data
#' @note Starts a SQLite connection.  The connection is closed in the call to dbConnect, this
#' should be conditioned with an on.exit here in case the connection was not closed.
#' If run_app is FALSE, the connection is closed on exit.
#' This function will create a SQLite database and list of pkgnet objects for use
#' with browse_checks.
#' @export
process_artifacts = function (dbpath = tempfile(), run_app=TRUE, 
      startup=TRUE, serialize_pnet = TRUE, pnet_obj_name = "pnet.rds") 
{
    bcc = lapply(dir(patt = "bcch"), readRDS)
    bn = lapply(dir(patt = "bcch"), function(x) gsub("_.*", "", 
        x))
    names(bcc) = unlist(bn)
    ccc = lapply(dir(patt = "_ch"), readRDS)
    names(ccc) = unlist(bn)
    ppp = lapply(dir(patt = "_pnet"), readRDS)
    names(ppp) = unlist(bn)
    chkdfs = rcc_to_dataframes(ccc)
    for (i in 1:length(bcc)) bcc[[i]]$package = names(bcc)[i]
    bchkdfs = bcc_to_dataframes(bcc)
    status_db_init(dbpath, chkdfs, !startup)
    status_db_init(dbpath, bchkdfs, TRUE)
    con = dbConnect(RSQLite::SQLite(), dbpath)
    if (serialize_pnet) 
        saveRDS(ppp, file = pnet_obj_name, compress = "xz")
    # run on.exit with a check for openness of con FIXME
    if (run_app) browse_checks(con, ppp)
    else dbDisconnect(con)
}

