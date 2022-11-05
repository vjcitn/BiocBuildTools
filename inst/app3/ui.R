
library(BiocBuildTools)
library(RSQLite)
con = RSQLite::dbConnect(RSQLite::SQLite(), "~/biocmaint.sqlite")
#suppressWarnings({
#pk = readRDS(system.file("pkgnet/pnet.rds", package="BiocBuildTools"))
#})

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
         tabPanel("Rerr", verbatimTextOutput("error")),
         tabPanel("Rwarn", verbatimTextOutput("warn")),
         tabPanel("Rnote", verbatimTextOutput("notes")),
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
