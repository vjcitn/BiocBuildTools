#' developer report on package check process in shiny
#' @import shiny
#' @importFrom RSQLite dbConnect SQLite SQLITE_RO dbGetQuery dbDisconnect dbReadTable
#' @param con SQLiteConnection where tables of check results from rcmdcheck, etc., are stored
#' @param pnet_obj list as created by `make_pnet_obj`
#' @note dbDisconnect is run if the stop button is pressed to end shiny session.  If
#' stopped via ctrl-C or error, the connection may remain and require closure.  N.B.  The
#' source codes for this app are in inst/app1, to simplify contribution to shinyapps.io.
#' Interactive execution proceeds by copying the server and ui files to temporary folder
#' where `shiny::runApp()` is executed.  For some packages for which tests do not succeed
#' coverage statistics derived by pkgnet will be absent.
#' @examples
#' if (interactive()) {
#'  con = RSQLite::dbConnect(RSQLite::SQLite(), 
#'   system.file("sqlite/demo16.sqlite", package="BiocBuildTools"), flags=RSQLite::SQLITE_RO)
#'  print(RSQLite::dbListTables(con))
#'  demo_pnet2 = readRDS(system.file("pkgnet/demo16_pnet.rds", package="BiocBuildTools"))
#'  browse_checks(con, demo_pnet2)
#' }
#' @export
browse_checks = function(con, pnet_obj) {
 con <<- con
 pnet_obj <<- pnet_obj
 od = getwd()
 on.exit(setwd(od))
 uif = system.file("app1/ui.R", package="BiocBuildTools")
 servf = system.file("app1/server.R", package="BiocBuildTools")
 td = tempdir()
 setwd(td)
 file.copy(uif, ".", overwrite=TRUE)
 file.copy(servf, ".", overwrite=TRUE)
 shiny::runApp()
}

format_bcchk = function(txt, out_suffix=".html") {
 cur = readLines(txt)
 cur = gsub("(\\* WARNING..*|^Warning..*)", "</pre><mark>\\1</mark><pre>", cur) 
 cur = gsub("(\\* NOTE..*)", "</pre><mark>\\1</mark><pre>", cur) 
 cur = gsub("(^ERROR..*|^WARNING..*|^NOTE..*)", "</pre><mark>\\1</mark><pre>", cur) 
 writeLines(c("<pre>", cur, "</pre>"), paste0(txt, out_suffix))
}

add_blank = function(strm, where) {
  if (length(where)>1) {
    where = rev(where)  # work from back
    strm = c(strm[1:where[1]], " ", strm[-c(1:where[1])])
    where = where[-1]
    Recall(strm,rev(where))
  }
  else c(strm[1:where], " ", strm[-c(1:where)])
}



# find * NOTE, WARNING, ERROR and produce a list for markup
# intent is to allow do.call(helpText, process_log(txt)) to succeed
process_log = function(curtxt, 
    event_regexp = c("\\* NOTE..*|\\* WARNING..*|\\* ERROR..*"), ...) {
  nlines = length(curtxt)
  evlocs = grep(event_regexp, curtxt)
  dev = diff(evlocs)
  ntr = 0
  while (any(dev==1) & ntr < 10) {  # kludge .. jam a blank in to separate contiguous events before adding markup
     ntr = ntr+1
     wh = which(dev==1)
     curtxt = add_blank(curtxt,evlocs[wh])
     evlocs = grep(event_regexp, curtxt)
     dev = diff(evlocs)
   }
# stop("contiguous events -- code needs revision")
#
#
# curtxt divides into event and non-event text
# non-event chunks are marked pre(), events marked strong()
#
# assume first and last chunks free of events
  markedtxt = vector("list", 2*length(evlocs)+1)
  curch = 1
  cursor = 1
  for (i in seq_along(evlocs)) {
    markedtxt[[curch]] = pre(xx <- paste(curtxt[cursor:(evlocs[i]-1)],collapse="\n"))
    curch = curch+1
    markedtxt[[curch]] = strong(paste(curtxt[evlocs[i]], collapse="\n"))
    cursor = evlocs[i]+1
    curch = curch+1
    }
  markedtxt[[curch]] = pre(paste(curtxt[cursor:nlines], collapse="\n"))
  markedtxt
}


bcchk_to_df = function(chktxt, pkgname=NULL) {
 x = readLines(chktxt)
 if (is.null(pkgname)) pkgname = strsplit(chktxt, "_")[[1]][1]
 data.frame(package=pkgname, bcchk=x)
}
 
