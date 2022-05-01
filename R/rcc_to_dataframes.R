#> sapply(rr,names)
#      [,1]           [,2]           [,3]           [,4]          
# [1,] "package"      "package"      "package"      "package"     
# [2,] "version"      "version"      "version"      "version"     
# [3,] "notes"        "notes"        "notes"        "notes"       
# [4,] "warnings"     "warnings"     "warnings"     "warnings"    
# [5,] "errors"       "errors"       "errors"       "errors"      
# [6,] "platform"     "platform"     "platform"     "platform"    
# [7,] "checkdir"     "checkdir"     "checkdir"     "checkdir"    
# [8,] "install_out"  "install_out"  "install_out"  "install_out" 
# [9,] "description"  "description"  "description"  "description" 
#[10,] "session_info" "session_info" "session_info" "session_info"
#[11,] "cran"         "cran"         "cran"         "cran"        
#[12,] "bioc"         "bioc"         "bioc"         "bioc"        

make_df_component = function(rcclist, component="notes") {
  pks = sapply(rcclist, "[[", "package")
  nol = lapply(rcclist, "[[", component)
  if (component == "description") {
   nol = lapply(nol, function(x) strsplit(x, "\n")[[1]])
  }
  nn = sapply(nol,length)
  zn = which(nn==0)
  if (length(zn)>0)
    nol[zn] = paste0("NO ", toupper(component))
  npk = rep(pks, sapply(nol, length))
  ans = data.frame(package=npk, tmp=unlist(nol), stringsAsFactors=FALSE)
  names(ans)[2] = component
  ans
}

#' produce list of data.frames for storage of R CMD check results in SQLite
#' @param rcclist list of results of rcmdcheck::rcmdcheck
#' @examples
#' data(demo_rcmdchk_out)
#' dfs = rcc_to_dataframes(demo_rcmdchk_out)
#' names(dfs)
#' names(dfs[[1]])
#' @export
rcc_to_dataframes = function(rcclist) {
  pks = sapply(rcclist, "[[", "package")
  vers = sapply(rcclist, "[[", "version")
  notes_df = make_df_component(rcclist, component="notes")
  err_df = make_df_component(rcclist, component="errors")
  warn_df = make_df_component(rcclist, component="warnings")
  inst_df = make_df_component(rcclist, component="install_out")
  desc_df = make_df_component(rcclist, component="description")
  list(basic = data.frame(package=pks, version=vers, stringsAsFactors=FALSE),
   notes = notes_df, warnings=warn_df, errors=err_df, inst=inst_df, desc=desc_df)
}

#' produce list of data.frames for storage of BiocCheck::BiocCheck results in SQLite
#' @param bcclist list of results of BiocCheck::BiocCheck, must be named with names of
#' packages as element names
#' @examples
#' data(demo_bcchk_out)
#' dfs = bcc_to_dataframes(demo_bcchk_out)
#' names(dfs)
#' names(dfs[[1]])
bcc_to_dataframes_old = function (bcclist) 
{
    nel = length(bcclist)
    nms = names(bcclist)
    stopifnot(nel==length(nms))
    notes_df = make_df_component(bcclist, component = "note")
    err_df = make_df_component(bcclist, component = "error")
    warn_df = make_df_component(bcclist, component = "warning")
    list(basic = data.frame(package = nms, stringsAsFactors = FALSE), 
        bcnotes = notes_df, bcwarnings = warn_df, bcerrors = err_df)
}

