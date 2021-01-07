## functions to build requests to HCA azul backend for HCAExplorer class



.project_content <- function(project, res)
{
    con <- httr::content(res)
    .parse_project_get(project, con)
}

#' @importFrom httr stop_for_status
.projectGet <- function(project, filter, per_page=15)
{
    url <- project@url
    project@perPage <- per_page
    project_url <- paste0(url, '/repository/projects?', filter, '&size=', per_page, '&sort=projectTitle&order=asc')
    project_res <- httr::GET(project_url)
    project <- .project_content(project, project_res)
    summary_url <- paste0(url, '/repository/summary?', filter)
    summary_res <- httr::GET(summary_url)
    httr::stop_for_status(summary_res)
    summary_res <- httr::content(summary_res)
    project@totalFileSize <- summary_res$totalFileSize
    project@fileCount <- summary_res$fileCount
    project@totalCellCount <- summary_res$totalCellCount
    project@donorCount <- summary_res$donorCount
    project@specimenCount <- summary_res$specimenCount
    project@projectCount <- summary_res$projectCount
    project
}

.nextResults_HCAExplorer <- function(x)
{
    url <- x@url
    page <- x@currentPage + 1
    url <- paste0(url, '/repository/projects?', x@currentFilter, '&size=', x@perPage,
        '&sort=projectTitle&order=asc&search_after=', x@searchAfter,
        '&search_after_uid=', x@searchAfterUid)
    res <- httr::GET(url)
    project <- .project_content(x, res)
    if (page > x@totalPages)
        page <- 1
    project@currentPage <- page
    project
}

#' Next Results
#'
#' @description Fetch the next set of entries from a HCAExplorer Object
#'
#' @param x An HCAExplorer object
#'
#' @return A HCAExplorer object that displays the next results
#'
#' @author Daniel Van Twisk
#'
#' @name nextResults
#' @aliases nextResults,HCAExplorer-method
#' @docType methods
#' 
#' @examples
#' ## Intitate an HCAExplorer object.
#' x <- HCAExplorer()
#' x
#' 
#' ## View the next set of results in the HCAExplorer object.
#' x <- nextResults(x)
#' x
#'
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class,
#'
#' @export
setMethod("nextResults", "HCAExplorer", .nextResults_HCAExplorer)
