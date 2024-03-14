# get bucket content metadata

library(aws.s3)
library(DBI)
library(dplyr)
bb = get_bucket_df("s3://bioc-builddb-mirror/buildResults")
type = sapply(strsplit(bb$Key, "-"), "[", 2)
table(type)

bb$type = type
bb$repdate = as.Date(bb$LastModified)

devinf = "buildResults/f9785dba87426695825cc6524dcb82c6-info.csv.gz"

if (!exists("con")) con <- dbConnect(duckdb::duckdb(), read_only=TRUE)
dbExecute(con, "install 'httpfs'")
dbExecute(con, "load 'httpfs'")

available_types = c("build_summary.csv.gz", 
   "info.csv.gz", "propagation_status.csv.gz", 
   "report.tgz")

probe_file = function(key, con) {
  pa = sprintf('s3://bioc-builddb-mirror/%s', key)
  sqlstring = sprintf("FROM read_csv('%s')", pa)
  tmp = con |>
    dplyr::tbl(dplyr::sql(sqlstring)) 
  fields = colnames(tmp)
  nrec = tmp |> dplyr::count() |> as.data.frame() |> 
      unlist() |> as.numeric()
  head=(tmp |> head() |> as.data.frame())
  type = unlist(Map(grepl, available_types, key))
  type = names(type)[which(type)]
  ans = list(fields=fields, nrec = nrec, head=head, key=key,
    type=type)
  class(ans) = c(type, class(ans))
  ans
}

print.info.csv.gz = function(x, ...) {
 cat(sprintf("info with %d records\n", x$nrec))
}

print.build_summary.csv.gz = function(x, ...) {
 cat(sprintf("build_summary with %d records\n", x$nrec))
}

print.propagation_status.csv.gz = function(x, ...) {
 cat(sprintf("propagation_status with %d records\n", x$nrec))
}

details = function(x) UseMethod("details")

details.info.csv.gz = function(x) {
  cat(sprintf("info.csv for %s branch, %d records.\n",
     x$head$git_branch[1], x$nrec))
}

#   dateInput(
#       inputId,
#       label,
#       value = NULL,
#       min = NULL,
#       max = NULL,
#       format = "yyyy-mm-dd",
#       startview = "month",
#       weekstart = 0,
#       language = "en",
#       width = NULL,
#       autoclose = TRUE,
#       datesdisabled = NULL,
#       daysofweekdisabled = NULL
#     )


library(shiny)

ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText("probe BiocBuildDB bucket contents"),
   dateInput("date", "date", min="2024-02-29", max="2024-03-13")
   ),
  mainPanel(
   tabsetPanel(
    tabPanel("main",
     verbatimTextOutput("thedate")
    ),
    tabPanel("sel", DT::dataTableOutput("selbydate"))
   )
  )
 )
)

server = function(input, output) {
 output$thedate = renderText( as.character(as.Date(input$date)) )
 output$selbydate = DT::renderDataTable({
    print(dim(bb))
    bb[ which(bb$repdate == as.Date(input$date)), ]
    })
}

myapp = function() {
 runApp(list(ui=ui, server=server))
}
