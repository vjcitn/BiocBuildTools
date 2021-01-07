
.results <- function(x)
{
    x@results
}

#' Obtain search unattenuated results tibble from an HCAExplorer Object
#'
#' @description
#'  Returns a tibble either showing bundles or files based on whichever is
#'  activated. The tibble returned will display the full results of the search
#'  instead of the attenuated table shows when dispalying the HCAExplorer
#'  object.
#'
#' @param x An HCAExplorer object.
#'
#' @return a tibble giving the unattenuated search results of the HCAExplorer
#'  object.
#'
#' @examples
#'  ## Initiate an HCAExplorer object.
#'  x <- HCAExplorer()
#'  ## Display the object. Note that the tibble contains very few columns.
#'  x
#'
#'  ## Access the tibble and display at available columns using results()
#'  results(x)
#'
#' @name results
#' @aliases results,HCAExplorer-method
#' @docType methods
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class.
#'
#' @importFrom dplyr distinct
#' @importFrom httr GET content
#' @export
setMethod('results', 'HCAExplorer', .results)

.viewProjects <-
function(x)
{
    ids <- x@results$entryId
    ret <- lapply(ids, function(id) {
        url <- paste0(x@url, '/repository/projects/', id)
        res <- httr::GET(url)
        res <- httr::content(res)
        res
    })
    
    #    cat('\nProject Title\t', as.character(res[,"projects.projectTitle"]), '\n')
    #    cat('\n')
    #    cat('Project Details\n')
    #    cat('Project Label\t\t\t', as.character(res[,"projects.projectShortname"]), '\n')
    #    cat('Species\t\t\t\t', as.character(res[,"donorOrganisms.genusSpecies"]), '\n')
    #    cat('Organ\t\t\t\t', as.character(res[,"specimens.organ"]), '\n')
    #    cat('Organ Part\t\t\t', as.character(res[,"specimens.organPart"]), '\n')
    #    cat('Known Diseases (Specimens)\t', as.character(res[,"specimens.disease"]), "\n")
    #    cat('Library Construction Approach\t', as.character(res[,"protocols.libraryConstructionApproach"]), "\n")
    #    cat('Paired End\t\t\t', as.character(res[, "protocols.pairedEnd"]), "\n")
    #    cat('File Type\t\t\t', as.character(res[,"fileTypeSummaries.fileType"]), "\n")
    #    cat('Cell Count Estimate\t\t', as.character(res[,"fileTypeSummaries.count"]), "\n")
    
    #    cat("\nDescription\n")
    #    cat(as.character(x[,"project_json.project_core.project_description"]), "\n")
    
    #    cat("\n")
    
    #    cat('Publications\t\t', as.character(x[,'publication.publication_title']), "\n")
    #    cat('Laboratory\t\t\t', as.character(res[,'projects.laboratory']), "\n")
    ret
}

#' View all metadata about a selection of projects
#'
#' @description Returns a list of all the metadata from the current selection
#'  of entries in the HCAExplorer object as a projectView object. This method is
#'  is meant to return a clear and useful represntation of the metadata of a
#'  selection of projects.
#'
#' @param x An HCAExplorer object
#'
#' @return A list of all metadata in the selected entries. This list will
#'  contain mulitple lists representing the metadata.
#'
#' @examples
#'  ## Initiate an HCAExplorer object.
#'  x <- HCAExplorer()
#'  x
#'
#'  ## Use viewProjects to show all metadata information relating to a project.
#'  view <- viewProjects(x)
#'  view
#'
#'  ## Subset the data to obtain the first two rows.
#'  x <- x[1:2,]
#'  x
#'
#'  ## Fewer projects have there metadata shown due to our previous subset.
#'  view <- viewProjects(x)
#'  view
#'
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class.
#'
#' @name viewProjects
#' @aliases viewProjects,HCAExplorer-method
#' @docType methods
#' @seealso [HCAExplorer()][projectView()]
#'
#' @export

setMethod('viewProjects', 'HCAExplorer', .viewProjects)

.summary_filter <- function(.data, ...)
{
    dots <- quos(...)
    project <- .data
    searchTerm <- Reduce(.project_filter_loop, dots, init = list())
    paste0('filters=', curl::curl_escape(jsonlite::toJSON(searchTerm)))
}

.getManifestFileFormats <- function(x)
{
    url <- x@url
    res <- x@results
    ids <- res$entryId
    query <- .summary_filter(x, projectId == ids)
    url <- paste0(url, '/repository/summary?', query)
    res <- httr::GET(url)
    res <- httr::content(res)
    res <- as.data.frame(do.call(rbind, res$fileTypeSummaries))
    unlist(res$fileType)
}

#' Show all possible manifest file formats for current selection
#' 
#' @description Show all possible manifest file formats for the
#'  current selection of projects in the HCAExplorer object. To be
#'  used in conjunction with 'getManifest()'.
#'
#' @param x An HCAExplorer object
#'
#' @return A character vecotr of information about possible file formats.
#'
#' @examples
#'  ## Initiate an HCAExplorer object.
#'  x <- HCAExplorer()
#'  ## View the HCAExplorer object to decide which projects to subset.
#'  x
#'
#'  ## Decide to subset to just obtain the first two projects.
#'  x[1:2,]
#'  x
#'
#'  ## Get all possible manifest file formats for projects.
#'  formats <- getManifestFileFormats(x)
#'  formats
#'
#' @name getManifestFileFormats
#' @aliases getManifestFileFormats,HCAExplorer-method
#' @docType methods
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class
#'  \code{\link{getManifest}} for how to obtain the manifest file.
#'
#'
#' @export
setMethod('getManifestFileFormats', 'HCAExplorer', .getManifestFileFormats)

.getManifest <- function(x, fileFormat)
{
    url <- x@url
    res <- x@results
    ids <- res$entryId
    filters <- .summary_filter(x, projectId == ids & fileFormat == fileFormat)
    url <- paste0(x@url, '/fetch/manifest/files?', filters, '&format=tsv')
    res <- httr::GET(url)
    stop_for_status(res)
    repeat{
        url <- httr::content(res)$Location
        res <- httr::GET(url)
        status <- httr::content(res)$Status
        if (status == 302)
            break
        sleep <- httr::content(res)$`Retry-After`
        message('sleeping ', sleep, ' seconds')
        Sys.sleep(sleep)
    }
    url <- httr::content(res)$Location
    res <- httr::GET(url)
    stop_for_status(res)
    httr::content(res)
}

.checkExpressionMatricesAvailability <- function(x, format=c("loom", "csv", "mtx"), organism)
{
    format <- match.arg(format)
    if(format != "loom")
        format <- paste0(format, ".zip")

    ## Check if resources already saved to BiocFileCache
    res <- x@results
    ids <- res$entryId
    titles <- res$projects.projectTitle
    organisms <- res$donorOrganisms.genusSpecies
    useOrganism <- !missing(organism)
    if (useOrganism)
        organisms <- organism

    statuses <- vapply(seq_along(ids), function(id) {
        if(useOrganism)
            orgs <- organism
        else
            orgs <- strsplit(organisms[id], ", ")
        status <- vapply(orgs[[1]], function(org) {
            org <- tolower(org)
            org <- gsub(" ", "_", org)
            name <- paste0(ids[id],".", org, ".", format)
            url <- paste0("https://data.humancellatlas.org/project-assets/project-matrices/", name)
            ret <- httr::HEAD(url)
            ret$status_code != 404L
         }, logical(1))
         any(status)
    }, logical(1))

    tibble(projects.projectTitle = titles, donorOrganisms.genusSpecies = organisms, matrix.availiable = statuses)
}

#' Download Expression Matrices with HCAExplorer
#'
#' @aliases checkExpressionMatricesAvailability downloadExpressionMatrices
#' 
#' @description
#'  Methods used to download expression matrices availiable through the HCA Data
#'  Portal. The method `checkExpressionMatricesAvaiability` specifies whether
#'  expression matrices can be downloaded for a project. The method
#'  `downloadExpressionMatrices` downloads and returns expression matrices in
#'  a variety of formats.
#' 
#' @usage
#'  checkExpressionMatricesAvailability(x, ...)
#'  downloadExpressionMatrices(x, ...)
#'
#' @param format either "loom", "csv", of "mtx". The default value is "loom".
#'  Which format the expression matrices should be downloaded as. If "loom", a
#'  `LoomExperiment` object will be returned. If "csv", a tibble will be
#'  returned. If "mtx", a `SingleCellExperiment` object will be returned.
#'
#' @param organism character(1). Which organism file to download. Usually either
#'  "Homo sapiens" or "Mus musculus". If not specified, all organism files will
#'  be included in the download.
#'
#' @param useBiocFileCache logical(1). Whether to save file with
#'  `BiocFileCache`. Defaults to `TRUE`. If `TRUE`, `BiocFileCache` will be used
#'  to save a persistent copy of the file to the user's computer. This
#'  persistent copy will be used instead of redownloading the expression
#'  matrices in subsequent uses. If `FALSE`, the file will be saved to a
#'  temporary directory and will be removed when the session ends.
#'
#' @param x an HCAExplorer object. All project entries in the object will
#'  attempt to be downloaded. Use subsetting to reduce the projects that are
#'  to be downloaded.
#' 
#' @param ... Additional arguments.
#'
#' @return
#'  For `checkExpressionMatrixAvailability`, returns a tibble.
#'  For `downloadExpressionMatrices', returns a list of downloaded expression
#'  matrices in a format specified by the `format` argument.
#'  
#' @examples
#' hca <- HCAExplorer()
#' checkExpressionMatricesAvailability(hca)
#' 
#' hca <- hca[5]
#' hca <- downloadExpressionMatrices(hca, useBiocFileCache = FALSE)
#'
#' @name download-expression-matrices
#' @author Daniel Van Twisk
NULL

#' Check Expression Matrix Availability
#'
#' @rdname download-expression-matrices
#' @export
setMethod("checkExpressionMatricesAvailability", "HCAExplorer", .checkExpressionMatricesAvailability)

#' @importFrom BiocFileCache BiocFileCache bfcquery bfcrpath bfcadd bfccache
#' @importFrom LoomExperiment LoomExperiment import LoomFile
#' @importFrom readr read_csv
#' @importFrom utils download.file unzip
.downloadExpressionMatrices <- function(x, format = c("loom", "csv", "mtx"), organism,
                                        useBiocFileCache = TRUE)
{
    format <- match.arg(format)
    if(format != "loom")
        format <- paste0(format, ".zip")

    ## Check if resources already saved to BiocFileCache
    res <- x@results
    ids <- res$entryId
    titles <- res$projects.projectTitle
    organisms <- res$donorOrganisms.genusSpecies

    matrices <- list()

    for(id in seq_along(ids)) {
        if(!missing(organism))
            orgs <- organism
        else
            orgs <- strsplit(organisms[id], ", ")
        for (org in orgs) {
            org <- tolower(org)
            org <- gsub(" ", "_", org)
            name <- paste0(ids[id],".", org, ".", format)
            url <- paste0("https://data.humancellatlas.org/project-assets/project-matrices/", name)

            if (useBiocFileCache) {
                bfc <- BiocFileCache()
                nrec <- NROW(bfcquery(bfc, name, "rname", exact = TRUE))

                if (nrec == 0L) {
                    ## Check if file exists
                    ret <- httr::HEAD(url)
                    if (ret$status_code == 404L) {
                        warning("Expression matrix file ", name, " of ", titles[id], " not found.")
                        next
                    } 
                    message("Downloading ", name)
                    dbpath <- bfcadd(bfc, name, fpath = url)
                }
                else if (nrec == 1L){
                    dbpath <- bfcrpath(bfc, name)
                } else {
                    stop(
                        "\n  'bfc' contains duplicate record names",
                        "\n      bfccache(): '", bfccache(bfc), "'",
                        "\n      rname: '", name, "'"
                    )
               }
            } else {
                dbpath <- tempfile(fileext = paste0(".", format))
                download.file(url, dbpath)
            }
        if (format == "loom")
            matrices[[paste(titles[id], name)]] <- LoomExperiment::import(dbpath)
        else if (format == "csv.zip") {
            files <- utils::unzip(dbpath, exdir = tempfile())
            message("This may take a while...")
            matrices[[paste(titles[id], name)]] <- lapply(stats::setNames(files, basename(files)), readr::read_csv)
        }
        else if (format == "mtx.zip")
            matrices[[paste(titles[id], name)]] <- HCAMatrixBrowser:::import_mtxzip(dbpath)
        }
    }
    matrices
}

#' Download Expression Matrices
#'
#' @rdname download-expression-matrices
#' @export
setMethod("downloadExpressionMatrices", "HCAExplorer", .downloadExpressionMatrices)

#' Obtain metadata information from an HCAExplorer object
#'
#' @description Obtain metadata infromation from an HCAExplorer object.
#'  This metadata can then be passed on to download files from other services.
#'
#' @param x An HCAExplorer object
#' @param fileFormat character. A character vector of file formats of metadata
#'  to obtain. The possible aruments can be found using the
#'  getManifestFileFormats method.
#'
#' @return a tibble of metadata information.
#'
#' @examples
#'  ## Initiate an HCAExplorer object.
#'  x <- HCAExplorer()
#'  ## View the HCAExplorer object to decide which projects to subset.
#'  x
#'
#'  ## Decide to subset first project.
#'  x <- x[1,]
#'  x
#'
#'  ## Get all possible manifest file formats for the project.
#'  formats <- getManifestFileFormats(x)
#'  formats
#'
#'  ## Obtain the manifest for the file using only the first format
#'  manifest <- getManifest(x, formats[1])
#'  manifest
#'
#' @name getManifest
#' @aliases getManifest,HCAExplorer-method
#' @docType methods
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class,
#'  \code{\link{getManifestFileFormats}} for how to obtain file formats that can
#'  be used as arguments in this function.
#'
#' @importFrom httr timeout
#' @export
setMethod('getManifest', 'HCAExplorer', .getManifest)

.activate.HCAExplorer <-
    function(.data, what = c('projects', 'samples', 'files'))
{
    x <- .data
    type <- match.arg(what)
    if(type == "projects")
        x@selected <- c('projects.projectTitle', 'samples.sampleEntityType', 'samples.organ', 'protocols.libraryConstructionApproach', 'protocols.pairedEnd', 'donorOrganisms.genusSpecies', 'samples.disease')
    if(type == "samples")
        x@selected <- c('samples.id', 'projects.projectTitle', 'samples.sampleEntityType', 'samples.organ', 'samples.organPart', 'cellSuspensions.selectedCellType', 'protocols.libraryConstructionApproach', 'protocols.pairedEnd', 'donorOrganisms.genusSpecies', 'donorOrganisms.organismAge', 'donorOrganisms.biologicalSex', 'samples.disease')
    if(type == "files")
        x@selected <- c('samples.id', 'samples.sampleEntityType', 'samples.organ', 'samples.organPart', 'protocols.libraryConstructionApproach', 'protocols.pairedEnd', 'donorOrganisms.genusSpecies', 'donorOrganisms.organismAge', 'samples.disease')
    filter(x)
}

#' @importFrom tidygraph activate
#' @export
tidygraph::activate

#' Activate projects, samples, or files to display in the HCAExplorer Object
#'
#' @description The HCAExplorer can display its results in a variety of ways.
#'  Choose whether to display entries by project, samples, or files.
#'  The HCAExplorer class always defaults to projects.
#'
#' @param .data An HCAExplorer object
#' @param what character(1). Either 'projects', 'samples', or 'files'.
#'
#' @return An HCAExplorer object with medified activation.
#'
#' @examples
#'  ## Initiate an HCAExplorer object.
#'  x <- HCAExplorer()
#'  ## Display the object. Notice "projects" are shown by default.
#'  x
#'
#'  ## Now activate "samples" and now notice that "samples" are displayed.
#'  x <- activate(x, 'samples')
#'  x
#'
#'  ## Now activate "files" and now notice that "files" are displayed.
#'  x <- activate(x, 'files')
#'  x
#'
#'  ## Now activate "projects" and now notice that the original "projects" are
#'  ## displayed.
#'  x <- activate(x, 'projects')
#'  x
#'
#' @method activate HCAExplorer
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class.
#'
#' @export
activate.HCAExplorer <- .activate.HCAExplorer

.reset_query <-
function(x)
{
    x@searchTerm <- list()
    x@query <- quos()
    x %>% filter()
}

#' Reset all queries performed on an object
#'
#' @description Reset all queries performed on an HCAExplorer
#'  object then return the result.
#'
#' @param x An HCAExplorer object
#'
#' @return An HCAExplorer object with the changes applied to it.
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
#'  ## Construct a query looking for projects that involve brain.
#'  x <- x %>% filter(organ == brain)
#'  x
#'
#'  ## Now select the first two projects by row.
#'  x <- x[1:2,]
#'  x
#'
#'  ## Finally we can remove the previous two queries to get the original empty
#'  ## search.
#'  x <- resetQuery(x)
#'  x
#'
#' @name resetQuery
#' @aliases resetQuery,HCAExplorer-method
#' @docType methods
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class,
#'  \code{\link{undoQuery}} for how to undo some queries.
#'
#' @importFrom rlang quos
#' @export
setMethod('resetQuery', 'HCAExplorer', .reset_query)

.undo_query <-
function(x, n = 1L)
{
    check <- length(x@query) - n
    x@searchTerm <- list()
    if (check < 1)
    resetQuery(x)
    else{
        x@query <- head(x@query, -c(n))
        filter(x)
    }
}

#' Undo one or multiple queries performed on an object
#'
#' @description Undo one or multiple queries performed on an HCAExplorer
#'  object then return the result.
#'
#' @param x An HCAExplorer object
#' @param n integer(1). The number of queries to step back from.
#'
#' @return An HCAExplorer object with the changes applied to it.
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
#'  ## Construct a query looking for projects that involve brain.
#'  x <- x %>% filter(organ == brain)
#'  x
#'
#'  ## Now select the first two projects by row.
#'  x <- x[1:2,]
#'  x
#'
#'  ## Finally we can remove the previous query using undoQuery().
#'  ## search.
#'  x <- undoQuery(x, n = 1)
#'  x
#'
#' @name undoQuery
#' @aliases undoQuery,HCAExplorer-method
#' @docType methods
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class,
#'  \code{\link{resetQuery}} for reseting the entire query.
#'
#' @export
setMethod('undoQuery', 'HCAExplorer', .undo_query)

.resetSelect <- function(x)
{
    activate(x, x@activated)
}

#' Reset a select perform on an HCAExplorer object.
#'
#' @description This function will reset the selected slot of an HCAExplorer
#'  object to its default selection.
#'
#' @param x An HCAExplorer Object
#'
#' @return An HCAExplorer object with its selected slot returned to its default
#'  value.
#'
#' @examples  
#'  ## Initiate an HCAExplorer Object.
#'  x <- HCAExplorer()
#'  ## View object to decide which colums to select.
#'  x
#'
#'  ## Decide to select columns 'projcts.projectTitle' and 'samples.organ'.
#'  x <- x %>% select('project.projectTitle', 'samples.organ')
#'  x
#'
#'  ## Revert selec() with resetSelect()
#'  x <- resetSelect(x)
#'  x
#'
#' @name resetSelect
#' @aliases resetSelect,HCAExplorer-method
#' @docType methods
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class,
#'  \code{\link{select}} for how to make the initial selection.
#'
#' @export
setMethod('resetSelect', 'HCAExplorer', .resetSelect)

.subset.HCAExplorer <- function(x, i, j, ...)
{
    if(!missing(j))
    ## FIXME: maybe they could be subset by columns using a select statment?
    stop('HCAExplorer object cannot be subset on columns.')
    res <- x@results
    if (is.character(i)) {
        i <- which(res[['projects.projectTitle']] == i)
    }
    ids <- res[i,]$entryId
    x %>% filter(projectId == ids)
}

#' Subset an HCAExplorer Object by row number or project name
#'
#' @description Allows subsetting an HCAExplorer object by row number or
#'  project name. This method internally acts as a filter and actually
#'  performs a query using "projectIds".
#'
#' @param x An HCAExplorer object.
#' @param i Either a numeric vector indicating which rows to choose or a
#'  character vector of project titles indicating which projects to choose.
#' @param j Unused argument.
#' @param ... Unused argument.
#' @param drop Unused argument.
#'
#' @return An HCAExplorer object with the applied subset.
#'
#' @examples
#'  ## Initiate an HCAExplorer Object.
#'  x <- HCAExplorer()
#'  ## View object to decide which projects to choose.
#'  x
#'  ## Decide to select projects 1, 2, 3, and 7
#'  x <- x[c(1:3, 7)]
#'  x
#' 
#' @aliases `[`,HCAExplorer-method
#' @docType methods
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class,
#'  \code{\link{filter}} for how to performm a query on an HCAExplorer object.
#'
#' @export
setMethod('[', c('HCAExplorer', 'ANY', 'ANY'), .subset.HCAExplorer)

.attenuated_results <- function(x)
{
    select(x@results, x@selected)
}

.show_HCAExplorer <- function(object)
{
    cat('class:', class(object), '\n')
    cat('Using azul backend at:\n ', object@url, '\n')
    cat('\n')
    cat('Donor count:', object@donorCount, '\n')
    cat('Specimens:', object@specimenCount, '\n')
    cat('Estimated Cells:', object@totalCellCount, '\n')
    cat('Files:', object@fileCount, '\n')
    cat('File Size:', utils:::format.object_size(object@totalFileSize, "auto"), '\n')
    cat('\n')
    cat('Showing', object@activated, 'with', object@perPage ,'results per page.')
    print(.attenuated_results(object))
    cat('Showing page', object@currentPage, 'of', object@totalPages, '\n')
}

#' Show HCAExplorer
#'
#' @param object a HCAExplorer object to show
#'
#' @return outputs a text represntation of the object
#'
#' @examples
#'  ## Initiate an HCAExplorer object.
#'  x <- HCAExplorer()
#'  ## Invoke show() by simply displaying the object.
#'  x
#'
#' @seealso
#'  \code{\link{HCAExplorer}} for the HCAExplorer class.
#'
#' @importFrom methods show
#' @export
setMethod('show', 'HCAExplorer', .show_HCAExplorer)

