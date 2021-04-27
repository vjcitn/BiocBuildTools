# from flexshi.R 25 dec 2019

    library(RSQLite)

    server = function(input, output) {
       output$error = renderPrint({
           tmp = RSQLite::dbGetQuery(con, paste0("select * from errors where package = '", input$pkchoice, "'"))
           cat(tmp[[2]], sep="\n---\n")
           })
       output$warn = renderPrint({
           tmp = RSQLite::dbGetQuery(con, paste0("select * from warnings where package = '", input$pkchoice, "'"))
           cat(tmp[[2]], sep="\n---\n")
           })
       output$notes = renderPrint({
           tmp = RSQLite::dbGetQuery(con, paste0("select * from notes where package = '", input$pkchoice, "'"))
           cat(tmp[[2]], sep="\n---\n")
           })
       output$desc = renderPrint({
           tmp = RSQLite::dbGetQuery(con, paste0("select * from desc where package = '", input$pkchoice, "'"))
           cat(tmp[[2]], sep="\n")
           })
       output$bcnotes = renderPrint({
           tmp = RSQLite::dbGetQuery(con, paste0("select * from bcnotes where package = '", input$pkchoice, "'"))
           cat(tmp[[2]], sep="\n---\n")
           })
       output$bcerror = renderPrint({
           tmp = RSQLite::dbGetQuery(con, paste0("select * from bcerrors where package = '", input$pkchoice, "'"))
           cat(tmp[[2]], sep="\n---\n")
           })
       output$bcwarn = renderPrint({
           tmp = RSQLite::dbGetQuery(con, paste0("select * from bcwarnings where package = '", input$pkchoice, "'"))
           cat(tmp[[2]], sep="\n---\n")
           })
#       output$testcov = DT::renderDataTable({
#           RSQLite::dbGetQuery(con, paste0("select * from covg where package = '", input$pkchoice, "'"))
#           })

        output$about = renderUI({
          helpText("This app", 
             tags$ul(tags$li("uses rcmdcheck::rcmdcheck to parse and organize the check log to separate errors, warnings, and notes,"), 
                     tags$li("ingests the BiocCheck log and decorates it lightly to simplify discovery of adverse conditions,"),
                     tags$li("formats results of covr::package_coverage to summarize test coverage (testthat or RUnit tests only) at the function level.")
              ) # end ul
             )  # end helpText
           }) 
        curpnet = reactive({
           td = tempdir()
           unzip(pnet_zip, file=paste0(input$pkchoice, ".rds"), exdir=td)
           readRDS(paste0(td, "/", input$pkchoice, ".rds"))
           })
     
        output$pnet = visNetwork::renderVisNetwork({
           curpnet()$dep$graph_viz  # GLOBAL pnet_obj from app interface function
           })
        output$pnetfun = visNetwork::renderVisNetwork({
           curpnet()$fun$graph_viz
           })
        output$depwidg = htmlwidgets::shinyRenderWidget(
           curpnet()$dep$get_summary_view(),
             DT::dataTableOutput, environment(), FALSE)
        output$funwidg = htmlwidgets::shinyRenderWidget(
           curpnet()$fun$get_summary_view(),
             DT::dataTableOutput, environment(), FALSE)
        observeEvent(input$stopBtn, {
            dbDisconnect(con)
            stopApp(returnValue = NULL)
        })
    }
