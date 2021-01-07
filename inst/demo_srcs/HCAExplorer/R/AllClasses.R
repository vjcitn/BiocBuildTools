
setOldClass('quosure')
setOldClass('quosures')

#' The Project Browser Class
#'
#' @description A class that displays Human Cell Atlas(HCA) information obtained
#'  from a digested version of the Human Cell Atlas referred to as the Azul
#'  Backend. This class allows one to view, filter, and select entries in the
#'  to ultimately download metadata files and then other data such as matrices.
#'
#' @param per_page the number of results to display per request.
#' @param url character(1) The url of the Human Cell Atlas Backend to access.
#'
#' @author Daniel Van Twisk
#'
#' @importFrom methods new
#' @exportClass HCAExplorer
.HCAExplorer <- setClass("HCAExplorer",
    slots = c(
        url = "character",
        results = "tbl_df",
        activated = 'character',
        query = "quosures",
        selected = "character",
        searchTerm = "list",
        perPage = "numeric",
        totalPages = "numeric",
        currentPage = "numeric",
        currentFilter = 'character',
        terms = "list",
        totalCellCount = 'numeric',
        donorCount = 'numeric',
        specimenCount = 'numeric',
        projectCount = 'numeric',
        labCount = 'numeric',
        fileCount = 'numeric',
        totalFileSize = 'numeric',
        searchAfter = "character",
        searchAfterUid = "character"
    )
)

#' The Project Browser Class
#'
#' @description A class that displays Human Cell Atlas(HCA) information obtained
#'  from a digested version of the Human Cell Atlas referred to as the Azul
#'  Backend. This class allows one to view, filter, and select entries in the
#'  to ultimately download metadata files and then other data such as matrices.
#'
#' @param per_page the number of results to display per request.
#' @param url character(1) The url of the Human Cell Atlas Backend to access.
#'
#' @author Daniel Van Twisk
#'
#' @return a HCAExplorer object
#'
#' @examples
#' hca <- HCAExplorer(per_page = 15,
#'                    url = 'https://service.explore.data.humancellatlas.org')
#' hca
#'
#' @importFrom tibble tibble
#' @export
HCAExplorer <-
    function(url='https://service.explore.data.humancellatlas.org',
             per_page = 15)
{
    x <- .HCAExplorer(url=url, perPage = per_page, results = tibble(),
                               query = quos(), activated = 'projects',
                               terms = list(), currentPage = 1)
    activate(x, 'projects')
}

#' The ProjectView class
#' 
#' @description A class that displays all metadata information from a list of
#'  projects in an HCAExplorer object. Extends lists.
#'
#' @param ... Arguments to be passed to list constructor.
#'
#' @exportClass ProjectView
.ProjectView <- setClass("ProjectView",
    contains = 'list'
)

ProjectView <- function(...)
{
    new(ProjectView(...))
}

