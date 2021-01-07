
.fields <- function(x)
{
    names(x@terms)
}

#' @importFrom vctrs fields
#' @export
vctrs::fields

#' List supported fields of an HCAExplorer object
#'
#' @description This function is used to discover possible fields that can be
#'  queried upon in an HCAExplorer object.
#'
#' @param x An HCAExplorer object.
#'
#' @return A tibble indicating fields that can be queried upon.
#' 
#' @aliases fields,HCAExplorer-method
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class,
#'  \code{\link{values}} for how to obtain possible values of a field.
#'
#' @examples
#' ## Intiate an HCAExplorer object.
#' x <- HCAExplorer()
#'
#' ## Find which fields can be queried upon.
#' fields(x)
#'
#' @importFrom tidygraph as_tibble
#' @importFrom utils head
#' @export
setMethod('fields', 'HCAExplorer', .fields)

.project.values <- function(x, fields)
{
    term <- x@terms
    field <- term[[fields]]
    field <- unlist(field)
    field <- head(field, -2)      ## Remove "total" columns at end
    uu <- matrix(field, nrow = 2)
    uu <- t(uu)
    uu <- as.data.frame(uu)
    names(uu) <- c('value', 'hits')
    as_tibble(uu)
}

#' List all values for certain fields in a HCAExplorer Object
#'
#' @description To be used in conjunction with the fields function. This
#'  function allows one to find which values can be queried upon in a given
#'  field.
#'
#' @param x An HCAExplorer Object.
#' @param fields A character vector of fields to display available values for.
#'
#' @return a character vector of possible values for a filter.
#'
#' @seealso 
#'  \code{\link{HCAExplorer}} for the HCAExplorer class,
#'  \code{\link{fields}} for how how to search for fields in an HCAExplorer object.
#'
#' @examples
#' ## Intiate an HCAExplorer object.
#' x <- HCAExplorer()
#'
#' ## Find which firelds can be queried upon.
#' fields(x)
#'
#' ## Which values can "organ" be searched with?
#' values(x, 'organ')
#'
#' @importFrom S4Vectors values
#' @export
setMethod("values", "HCAExplorer", .project.values)

.try_convert_colname_to_field <- function(x)
{
    if(grepl("^.*\\.", x)) {
        x <- gsub("^.*\\.","", x)
    }
    x
}

.binary_op_project <- function(sep)
{
    force(sep)
    function(e1, e2) {
        field <- .try_convert_colname_to_field(as.character(substitute(e1)))
        value <- try({
            e2
        }, silent = TRUE)
        if (inherits(value, "try-error")) {
            value <- as.character(substitute(e2))
            if(value[1] == 'c')
                value <- value[-1]
            value
        }
        
        fun <- "is"

        leaf <- list(value)

        names(leaf) <- fun
        leaf <- list(leaf)
        names(leaf) <- field
        leaf
    }
}

.combine_op_project <- function(sep)
{
    force(sep)
    function(e1, e2) {
        
        con <- list(e1, e2)
        con
    }
}

.project_filter_loop <- function(li, expr)
{
    res <- rlang::eval_tidy(expr, data = .LOG_OP_REG_PROJECT)
    if (length(res) > 1)
        res <- unlist(res, recursive = FALSE)
    c(li, res)
}

#' Filter an HCAExplorer object using fields and values
#' 
#' @description Given some amount of fields and values associated with them,
#'  modify the search performed by the HCAExplorer object. This function adds
#'  terms to the query that is ultimately performed.
#' 
#' @param .data An HCAExplorer object to query.
#' @param ... Any number of fields and values to queried against. The binary
#'  operators '==' and '%in%' are allowed while only the combination operator
#'  '&' is allowed. See examples.
#' @param .preserve Unused argument
#'
#' @return An HCAExplorer object with the desired query performed.
#'
#' @seealso [HCAExplorer()]
#'
#' @examples
#'  ## Initiate an HCAExplorer Object
#'  x <- HCAExplorer()
#'
#'  ## First we want to perform a search for certain organs.
#'  ## Display possible fields looking for organs.
#'  fields(x)
#'  ## organs can be queried with "organ".
#'  ## What values can the field "organ" have?
#'  values(x, "organ")
#'
#'  ## Construct a query looking for projects that involve blood or brain.
#'  y <- x %>% filter(organ %in% c('blood', 'brain'))
#'  y
#'
#'  ## Now suppose one would also like to find projects that have a certain
#'  ## disease. What field corresponds to disease?
#'  fields(y)
#'  ## The "disease" field looks right.
#'  ## What possible disease values can be queried upon?
#'  values(y, "disease")
#'
#'  ## Add a query for a 'normal' diease state to our search.
#'  y <- y %>% filter(disease == 'normal')
#'  y
#'
#'  ## This entire query can also be performed at once in several ways.
#'  x %>% filter(organ == c('blood', 'brain') & disease == 'normal')
#'  x %>% filter(organ == c('blood', 'brain'), disease == 'normal')
#'
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class.
#'
#' @importFrom curl curl_escape
#' @importFrom dplyr filter
#' @importFrom jsonlite toJSON
#' @export
filter.HCAExplorer <- function(.data, ..., .preserve)
{
    dots = quos(...)
    project <- .data
    if (length(dots) == 0 && length(project@query) == 0) {
        ret <- paste0('filters=', curl::curl_escape('{}'))
        project@currentFilter <- ret
        .projectGet(project, ret)
    }
    else {
        query <- c(project@query, dots)
        searchTerm <- Reduce(.project_filter_loop, query, init = list())
        ret <- paste0('filters=', curl::curl_escape(jsonlite::toJSON(searchTerm)))
        project@query <- query
        project@searchTerm <- searchTerm
        project@currentFilter <- ret
        .projectGet(project, ret)
    }
}

#' Select columns to display upon showing the object
#'
#' @description An HCAExplorer object is intitated with certain default columns
#'  being displayed upon showing the object. This method allows a user to
#'  display columns other than the default columns when displaying the object.
#'
#' @param .data An HCAObject to filter.
#' @param ... Columns to be displayed.
#'
#' @return An HCAExplorer object with the applied filter.
#'
#' @examples
#'  ## Intiate an HCAExplorer object
#'  x <- HCAExplorer()
#'  x
#'
#'  ## Use the results() method to display which columns are present.
#'  results(x)
#'
#'  ## Select the 'projects.projectTitle' and 'samples.organ' columns.
#'  x <- x %>% select('projects.projectTitle', 'samples.organ')
#'  x
#'
#'  ## Use resetSelect() to return to the original selection
#'  x <- resetSelect(x)
#'  x
#'
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class,
#'  \code{\link{resetSelect}} for how to reset the selection applied to an HCAExplorer object.
#'
#' @importFrom dplyr select
#' @importFrom tidygraph %>%
#' @export
select.HCAExplorer <- function(.data, ...)
{
    hca <- .data
    selected <- unlist(list(...))
    hca@selected <- intersect(hca@selected, selected)
    hca
}

.LOG_OP_REG_PROJECT <- list()
.LOG_OP_REG_PROJECT$`==` <- .binary_op_project("==")
.LOG_OP_REG_PROJECT$`%in%` <- .binary_op_project("==")
.LOG_OP_REG_PROJECT$`&` <- .combine_op_project("&")

