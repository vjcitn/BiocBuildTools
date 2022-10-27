
library(BiocBuildTools)
library(RSQLite)
con = RSQLite::dbConnect(RSQLite::SQLite(), "chks_3.16a.sqlite")
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
         tabPanel("BCerr", verbatimTextOutput("bcerror")),
         tabPanel("BCwarn", verbatimTextOutput("bcwarn")),
         tabPanel("BCnote", verbatimTextOutput("bcnotes")),
#         tabPanel("pnet", visNetwork::visNetworkOutput("pnet")),
         tabPanel("depnet", DT::dataTableOutput("depwidg"),
          visNetwork::visNetworkOutput("pnet")),
         tabPanel("funnet", DT::dataTableOutput("funwidg"),
          visNetwork::visNetworkOutput("pnetfun")),
         tabPanel("about", uiOutput("about"))
         )
        )
       )
      )
