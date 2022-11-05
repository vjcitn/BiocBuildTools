# from flexshi.R 25 dec 2019

library(RSQLite)
library(BiocBuildTools)


con = RSQLite::dbConnect(RSQLite::SQLite(), "~/coredemo2.sqlite")

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
       output$bcerr = DT::renderDataTable({
           tmp = RSQLite::dbGetQuery(con, paste0("select * from BcChkERR where package = '", input$pkchoice, "'"))
           tmp[, c("type", "message")]
           })
       output$bcwarn = DT::renderDataTable({
           tmp = RSQLite::dbGetQuery(con, paste0("select * from BcChkWARN where package = '", input$pkchoice, "'"))
           tmp[, c("type", "message")]
           })

        output$about = renderUI({
          helpText("This app", 
             tags$ul(tags$li("uses rcmdcheck::rcmdcheck to parse and organize the check log to separate errors, warnings, and notes,"), 
                     tags$li("ingests the BiocCheck log and decorates it lightly to simplify discovery of adverse conditions,"),
                     tags$li("formats results of covr::package_coverage to summarize test coverage (testthat or RUnit tests only) at the function level."),
                     tags$li("is based on a SQLite table generated using BiocBuildTools, which provides date-time of both the check event and the last commit to git")
              ) # end ul
             )  # end helpText
           }) 
        observeEvent(input$stopBtn, {
            dbDisconnect(con)
            stopApp(returnValue = NULL)
        })
    }
