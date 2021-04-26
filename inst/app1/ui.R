library(RSQLite)
    basic = RSQLite::dbReadTable(con, "basic")

    ui = fluidPage(
      sidebarLayout(
       sidebarPanel( 
        helpText("BiocBuildTools browse_checks"),
        helpText("reports on errors, warnings, and notes produced"),
        helpText("by R CMD check and BiocCheck are available in tabs."),
        helpText(" "),
        helpText("The pkgnet package-dependency analysis is in depnet tab."),
        helpText("The pkgnet function-dependency analysis is in funnet tab."),
        helpText(" "),
        helpText("Network displays below tables in these pkgnet tabs are zoomable visNetwork widgets."),
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
