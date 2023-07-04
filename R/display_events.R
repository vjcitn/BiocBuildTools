


#' simple app to present data on events gathered into a SQLite database
#' @param dbpath character(1) SQLite database produced by BiocBuildTools::build_sqlite_db
#' @export
display_events = function(dbpath) {
    con = RSQLite::dbConnect(RSQLite::SQLite(), dbpath)
    on.exit(RSQLite::dbDisconnect(con))

    basic = RSQLite::dbReadTable(con, "basic")

    ui = fluidPage(
      sidebarLayout(
       sidebarPanel( 
        helpText("BiocBuildTools browse_checks"),
        helpText("reports on errors, warnings, and notes produced"),
        helpText("by R CMD check and BiocCheck are available in tabs."),
        helpText(" "),
        selectInput("pkchoice", "Select a package", choices=sort(basic$package), selected=sort(basic$package)[1]),
        actionButton("stopBtn", "Stop app."),
        width=3
         ),
        mainPanel(tabsetPanel(
         tabPanel("description", verbatimTextOutput("desc")),
         tabPanel("Rerr", DT::dataTableOutput("error")),
         tabPanel("Rwarn", DT::dataTableOutput("warn")),
         tabPanel("Rnote", DT::dataTableOutput("notes")),
         tabPanel("BCerr", DT::dataTableOutput("bcerr")),
         tabPanel("BCwarn", DT::dataTableOutput("bcwarn")),
         tabPanel("summaries", 
                      helpText("-----"),
                      helpText("R CMD check errors"),
                      helpText("-----"),
                      DT::dataTableOutput("errfreq"),
                      helpText("-----"),
                      helpText("R CMD check warnings"),
                      helpText("-----"),
                      DT::dataTableOutput("warnfreq"),
                      helpText("-----")),
#         tabPanel("BCnote", verbatimTextOutput("bcnotes")),
#         tabPanel("pnet", visNetwork::visNetworkOutput("pnet")),
#         tabPanel("depnet", DT::dataTableOutput("depwidg"),
#          visNetwork::visNetworkOutput("pnet")),
#         tabPanel("funnet", DT::dataTableOutput("funwidg"),
#          visNetwork::visNetworkOutput("pnetfun")),
         tabPanel("about", uiOutput("about"))
         )
        )
       )
      )

    server = function(input, output) {
       output$error = DT::renderDataTable({
           tmp = RSQLite::dbGetQuery(con, paste0("select * from errors where package = '", input$pkchoice, "'"))
           as.data.frame(tmp)
           })
       output$warn = DT::renderDataTable({
           as.data.frame(RSQLite::dbGetQuery(con, paste0("select * from warnings where package = '", input$pkchoice, "'")))
           })
       output$notes = DT::renderDataTable({
           as.data.frame(RSQLite::dbGetQuery(con, paste0("select * from notes where package = '", input$pkchoice, "'")))
           })
       output$desc = renderPrint({
           tmp = RSQLite::dbGetQuery(con, paste0("select * from desc where package = '", input$pkchoice, "'"))
           cat(tmp[[2]], sep="\n")
           })
       output$bcerr = DT::renderDataTable({
           as.data.frame(RSQLite::dbGetQuery(con, paste0("select * from BcChkERR where package = '", input$pkchoice, "'")))
           })
       output$bcwarn = DT::renderDataTable({
           as.data.frame(RSQLite::dbGetQuery(con, paste0("select * from BcChkWARN where package = '", input$pkchoice, "'")))
           })

fixit = function(df, col) { df[[col]] = gsub("\\n", "<br>", df[[col]]); df }

       output$errfreq = DT::renderDataTable({
        con |> tbl("errors") |> filter(errors != "NO ERRORS") |> 
               group_by(errors) |> summarise(n=n()) |> arrange(desc(n)) |> as.data.frame() |> fixit("errors")
       }, escape=FALSE)
       output$warnfreq = DT::renderDataTable({
        con |> tbl("warnings") |> filter(warnings != "NO WARNINGS") |> group_by(warnings) |> summarise(n=n()) |> arrange(desc(n)) |>
		as.data.frame() |> fixit("warnings")
       },escape=FALSE)
#con |> tbl("notes") |> group_by(notes) |> summarise(n=n()) |> arrange(desc(n))
#con |> tbl("BcChkERR") |> group_by(message) |> summarise(n=n()) |> arrange(desc(n))
#con |> tbl("BcChkWARN") |> group_by(message) |> summarise(n=n()) |> arrange(desc(n))

        output$about = renderUI({
          helpText("This app", 
             tags$ul(tags$li("uses rcmdcheck::rcmdcheck to parse and organize the check log to separate errors, warnings, and notes,"), 
                     tags$li("ingests the BiocCheck log and decorates it lightly to simplify discovery of adverse conditions,"),
                     tags$li("[NOT YET:] formats results of covr::package_coverage to summarize test coverage (testthat or RUnit tests only) at the function level."),
                     tags$li("is based on a SQLite table generated using BiocBuildTools, which provides date-time of both the check event and the last commit to git")
              ) # end ul
             )  # end helpText
           }) 
        observeEvent(input$stopBtn, {
            dbDisconnect(con)
            stopApp(returnValue = NULL)
        })
    }
  runApp(list(ui=ui, server=server))
}
