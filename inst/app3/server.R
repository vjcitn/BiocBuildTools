# from flexshi.R 25 dec 2019

library(RSQLite)
library(BiocBuildTools)
#con = RSQLite::dbConnect(RSQLite::SQLite(), system.file(
#   "sqlite/vjc3.sqlite", package="BiocBuildTools"))
con = RSQLite::dbConnect(RSQLite::SQLite(), "chks_3.16a.sqlite")

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
           tmp = RSQLite::dbGetQuery(con, paste0("select * from BiocChk where package = '", input$pkchoice, "'"))
           cat(tmp[[2]], sep="\n---\n")
           })

        output$about = renderUI({
          helpText("This app", 
             tags$ul(tags$li("uses rcmdcheck::rcmdcheck to parse and organize the check log to separate errors, warnings, and notes,"), 
                     tags$li("ingests the BiocCheck log and decorates it lightly to simplify discovery of adverse conditions,"),
                     tags$li("formats results of covr::package_coverage to summarize test coverage (testthat or RUnit tests only) at the function level.")
              ) # end ul
             )  # end helpText
           }) 
        observeEvent(input$stopBtn, {
            dbDisconnect(con)
            stopApp(returnValue = NULL)
        })
    }
