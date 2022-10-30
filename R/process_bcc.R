#' process BiocCheck logs into a single data.frame emphasizing non-OK lines
#' @param folder character(1) path to a collection of BiocCheck outputs (packagename/00BiocCheck.log)
#' @export
process_bcc = function(folder) {
 targs = dir(folder)
 pks = gsub(".BiocCheck", "", targs)
 alllogs = lapply(paste0(folder, "/", targs, "/", "00BiocCheck.log"), readLines)
 alldf = lapply(seq_len(length(alllogs)), function(x) data.frame(package=pks[x],
            log = alllogs[[x]]))
 out = do.call(rbind, alldf)
 out |> dplyr::filter(!grepl("Checking|OK$", log))
}

#' helper for bco2df
#' @param x the warning or error component of BiocCheck
#' @return data.frame with checkType
.bco2df = function (x)
{   
    ns = sapply(x, function(x) length(unlist(x)))
    ty = rep(names(x), ns)
    ans = data.frame(type = ty, message = unlist(x))
    rownames(ans) = NULL
    ans
}

#' convert BiocCheck output (just error and warning) to list of data.frames
#' @param x BiocCheck result
#' @return list of data frames errors, warnings
#' @export
bco2df = function(x) {
 stopifnot(all(c("error", "warning") %in% names(x)))
 errdf = data.frame(type="error", message="none")
 if (length(x$error)>0)
   errdf = .bco2df(x$error)
 wrndf = data.frame(type="warning", message="none")
 if (length(x$warning)>0)
   wrndf = .bco2df(x$warning)
 list(errors=errdf, warnings=wrndf)
}

