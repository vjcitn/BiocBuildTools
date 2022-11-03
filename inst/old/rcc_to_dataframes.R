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
#' dfs = rcclist_to_dataframes(demo_rcmdchk_out)
#' names(dfs)
#' names(dfs[[1]])
#' @export
rcclist_to_dataframes = function(rcclist) {
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

#> BiocBuildTools:::get_warnings
#function (z) 
#{
#    if (length(z$warning) == 0) 
#        return("No bioccheck warnings")
#    sapply(z$warning, function(x) unname(grep("WARNING", x, value = TRUE)))
#}
#<bytecode: 0x55fafa7d8660>
#<environment: namespace:BiocBuildTools>
#> BiocBuildTools:::get_errors
#function (z) 
#{
#    if (length(z$error) == 0) 
#        return("No bioccheck errors")
#    sapply(z$error, function(x) unname(grep("ERROR", x, value = TRUE)))
#}
#<bytecode: 0x55fafab3fc90>
#<environment: namespace:BiocBuildTools>
#> BiocBuildTools:::get_notes
#function (x) 
#{
#    noo = x$note
#    unname(sapply(noo, function(x) grep("NOTE:", x, value = TRUE)))
#}

#> allbc[[11]]$metadata$Package
#[1] "ACE"
#> allbc[[11]]$metadata$PackageVersion
#[1] "1.14.0"



#' operate on BiocCheck return values in a list
#' @param bcclist list() of outputs of BiocCheck::BiocCheck
#' @param clean_try_errors logical(1) if TRUE, scan for try-error and omit those elements
#' @return list of data.frame with basic (pkg, version), notes, warnings, errors from BiocCheck
#' @note If x$metadata$Package is not character, element is dropped.
#' @export
bcclist_to_dataframes = function (bcclist, clean_try_errors=TRUE) 
{
    bad = NULL
    pkok = vapply(bcclist, function(x) !is.null(x$metadata$Package) && nchar(x$metadata$Package)>0, logical(1))
    bcclist = bcclist[pkok]
    pks = vapply(bcclist, function(x) x$metadata$Package, character(1))
    if (clean_try_errors) {
      haserr = vapply(bcclist, function(x) inherits(x, "try-error"), logical(1))
      if (any(haserr)) {
        bcclist = bcclist[-which(haserr)]
        bad = pks[which(haserr)]
        pks = pks[-which(haserr)]
        }
      if (length(bad)>0) message(sprintf("%d packages could not finish BiocCheck; see attr(., 'lost')",
                  length(bad)))
      }
    vers = vapply(bcclist, function(x) x$metadata$PackageVersion, character(1))
    nel = seq_len(length(bcclist))
    notes_df = lapply(nel, function(x) data.frame(package = pks[x], 
        notes = unlist(BiocBuildTools:::get_notes(bcclist[[x]]))))
    err_df = lapply(nel, function(x) data.frame(package = pks[x], 
        errors = unlist(BiocBuildTools:::get_errors(bcclist[[x]]))))
    warn_df = lapply(nel, function(x) data.frame(package = pks[x], 
        warnings = unlist(BiocBuildTools:::get_warnings(bcclist[[x]]))))
    list(basic = data.frame(package = pks, version = vers, stringsAsFactors = FALSE), 
        notes = do.call(rbind, notes_df), warnings = do.call(rbind, 
            warn_df), errors = do.call(rbind, err_df), lost=bad)
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

