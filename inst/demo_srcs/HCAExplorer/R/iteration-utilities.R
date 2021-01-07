
#' @importFrom plyr rbind.fill
#' @importFrom tidygraph as_tibble
.parse_project_get <- function(project, results)
{
    upfront <- lapply(results[[1]], function(x) {
        temp <- x
#        temp["fileTypeSummaries"] <- NULL
#        temp["projectSummary"] <- NULL
        #temp <- unlist(temp)
        temp <- lapply(temp, function(z) lapply(z, function(x) lapply(x, function(y) paste(y, collapse=", "))))
        temp <- unlist(temp)
        df <- as.data.frame(t(matrix(temp)))
        names(df) <- names(temp)
#        names(df) <- names(temp)
#        df
        df
    })
    terms <- results[['termFacets']]
    if(is.null(terms))
        terms <- list()
    project@terms <- terms
    upfront <- do.call(rbind.fill, upfront)
    dd <- duplicated(names(upfront))
    upfront <- upfront[!dd]
    project@results <- as_tibble(upfront)
    project@searchAfter <- curl::curl_escape(results[[2]][["search_after"]])
    project@searchAfterUid <- curl::curl_escape(results[[2]][["search_after_uid"]])
    project@totalPages <- results[[2]][["pages"]]
    project
}

