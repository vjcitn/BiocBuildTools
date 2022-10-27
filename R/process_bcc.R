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
