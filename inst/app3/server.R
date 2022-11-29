# from flexshi.R 25 dec 2019

library(RSQLite)
library(BiocBuildTools)
library(dplyr)
library(dbplyr)


con = RSQLite::dbConnect(RSQLite::SQLite(), "./biocnov28.sqlite")

#[1] "BcChkERR"  "BcChkWARN" "basic"     "desc"      "errors"    "inst"     
#[7] "notes"     "warnings" 

#> dbReadTable(con, "BcChkERR")[1:3,]
#        package                      type
#1 AnnotationHub checkExportsAreDocumented
#2 AnnotationHub checkExportsAreDocumented
#3 AnnotationHub checkExportsAreDocumented
#                                                                                       message
#1 * ERROR: At least 80% of man pages documenting exported objects must have runnable examples.
#2                                                                  The following pages do not:
#3                                                               AnnotationHubResource-class.Rd
##> dbReadTable(con, "BcChkWARN")[1:3,]
#        package                                 type
#1 AnnotationHub                   checkVersionNumber
#2 AnnotationHub checkDescriptionNamespaceConsistency
#3 AnnotationHub                 checkVigEvalAllFalse
#                                                              message
#1             * WARNING: y of x.y.z version should be even in release
#2 * WARNING: Import tools, stats in DESCRIPTION as well as NAMESPACE.
#3                 * WARNING:  Vignette set global option 'eval=FALSE'



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
