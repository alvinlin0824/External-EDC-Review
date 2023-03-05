## Update Add Tabsets on 03-Aug-2022
library(shiny)
library(tidyverse)
library(shinyFeedback)
library(rclipboard)
options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  titlePanel("ADC-US-RES EDC Review"),
  useShinyFeedback(),
  a(span("Please email Alvin Lin if you run into any issues",style = "color:black"),href = "mailto:alvin.lin@abbott.com"),
  fluidRow(column(12,
                  textInput("study",h6("Please enter study",value = "")),
                  uiOutput("new"),
                  verbatimTextOutput("text"),
                  rclipboardSetup(),
                  uiOutput("clip"))),
                  br(),
  tabsetPanel(
    tabPanel(p("Adverse Event Report",style = "font-size:20px;"),
             fileInput("ae1","Please Upload AE1",accept = ".sas7bdat"),
             fileInput("ae2","Please Upload AE2",accept = ".sas7bdat")),
    tabPanel(p("Clinic Visit Admission and Discharge",style = "font-size:20px;"),
             fileInput("cad2","Please Upload CAD2",accept = ".sas7bdat"),
             fileInput("cad3","Please Upload CAD3",accept = ".sas7bdat"),
             fileInput("cad4","Please Upload CAD4",accept = ".sas7bdat")),
    tabPanel(p("Medications",style = "font-size:20px;"),
             fileInput("cm1","Please Upload CM1",accept = ".sas7bdat"),
             fileInput("cm2","Please Upload CM2",accept = ".sas7bdat")),  
    tabPanel(p("Device Incident Report",style = "font-size:20px;"),
             fileInput("de","Please Upload DE",accept = ".sas7bdat")),
    tabPanel(p("Demographics",style = "font-size:20px;"),
             fileInput("dm","Please Upload DM",accept = ".sas7bdat"),
             fileInput("mh1","Please Upload MH1",accept = ".sas7bdat"),
             fileInput("vs","Please Upload VS",accept = ".sas7bdat")),     
    tabPanel(p("Final Data Upload Log",style = "font-size:20px;"),
             fileInput("du2","Please Upload DU2",accept = ".sas7bdat")),
    tabPanel(p("Eligibility",style = "font-size:20px;"),
             fileInput("ie","Please Upload IE",accept = ".sas7bdat")),
    tabPanel(p("IV Sample Collection",style = "font-size:20px;"),
             fileInput("iv1","Please Upload IV1",accept = ".sas7bdat"),
             fileInput("iv2","Please Upload IV2",accept = ".sas7bdat")),
    tabPanel(p("Protocol Deviation Report",style = "font-size:20px;"),
             fileInput("pd","Please Upload PD",accept = ".sas7bdat")),
    tabPanel(p("Quality Control Check - YSI",style = "font-size:20px;"),
             fileInput("qcrg1","Please Upload QCRG1",accept = ".sas7bdat"),
             fileInput("qcrg2","Please Upload QCRG2",accept = ".sas7bdat")),
    tabPanel(p("Sensor Application",style = "font-size:20px;"),
             fileInput("sa1","Please Upload SA1",accept = ".sas7bdat"),
             fileInput("sa2","Please Upload SA2",accept = ".sas7bdat")),
    tabPanel(p("Study Exit",style = "font-size:20px;"),
             fileInput("se","Please Upload SE",accept = ".sas7bdat")),
    tabPanel(p("Skin Assessment",style = "font-size:20px;"),
             fileInput("sk1","Please Upload SK1",accept = ".sas7bdat"),
             fileInput("sk2","Please Upload SK2",accept = ".sas7bdat")),
    tabPanel(p("Study Set-up",style = "font-size:20px;"),
             fileInput("sl1","Please Upload SL1",accept = ".sas7bdat"),
             fileInput("sl2","Please Upload SL2",accept = ".sas7bdat"),
             fileInput("sl3","Please Upload SL3",accept = ".sas7bdat")),
    tabPanel(p("Sensor Removal",style = "font-size:20px;"),
             fileInput("sr2","Please Upload SR2",accept = ".sas7bdat")),
    tabPanel(p("Ascorbic Acid Acetaminophen Dosing and Meal",style = "font-size:20px;"),
             fileInput("vc1","Please Upload VC1",accept = ".sas7bdat"),
             fileInput("vc2","Please Upload VC2",accept = ".sas7bdat")),
  ),

  fluidRow(column(12, 
                  downloadButton("download","Download Report",class = "btn-block",style = "width:100%;"))
  )
)

server <- function(input, output, session) {
  
  ## Dynamic User interface
  observeEvent(input$reset,{
    session$reload()
  })
  
  output$new <- renderUI({
    if (input$study %in% c("21206","21216")) return(NULL) else {
      textInput("event", "Please enter study event")
    }
  })
  
  text <- reactive({
    req(input$study)
    
    exists <- input$study %in% c("21217","21211","22225","21206","21216")
    feedbackWarning("study",!exists,"Unkown Study")
    req(exists,cancelOutput = FALSE)
    
    if (input$study %in% c("21217","21211","22225")) {
      cat(str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC","-","US","-","RES","-",input$study,"\\","SE",input$event,"\\","Openclinica","\\","Current"))
    } else  {
      cat(str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC","-","US","-","VAL","-",input$study,"\\","Openclinica","\\","Current"))
    }
  })
  
  output$text <- renderPrint({text()})
  
  ## Copy button
  
  output$clip <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy Path",
      clipText =  if (input$study %in% c("21217","21211","22225")) {
        str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC","-","US","-","RES","-",input$study,"\\","SE",input$event,"\\","Openclinica","\\","Current")
      } else  {
        str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC","-","US","-","VAL","-",input$study,"\\","Openclinica","\\","Current")
      }
      ,
      icon = icon("clipboard")
    )
  })
  
  
  ## Download Report
  output$download <- downloadHandler(
    filename = function(){
      str_c("ADC-US-RES ",input$study," EDC Review Report",".html")
      # str_c("ADC-US-RES-21217 EDC Review Report.html")
    },
    content = function(file){
      params <- list(Study = input$study,
                     data1 = input$ae1$datapath,
                     data2 = input$ae2$datapath,
                     data3 = input$cad2$datapath,
                     data4 = input$cad3$datapath,
                     data5 = input$cad4$datapath,
                     data6 = input$cm1$datapath,
                     data7 = input$cm2$datapath,
                     data8 = input$co$datapath,
                     data9 = input$de$datapath,
                     data10 = input$dm$datapath,
                     data11 = input$du2$datapath,
                     data12 = input$ie$datapath,
                     data13 = input$iv1$datapath,
                     data14 = input$iv2$datapath,
                     data15 = input$mh1$datapath,
                     data16 = input$pd$datapath,
                     data17 = input$qcrg1$datapath,
                     data18 = input$qcrg2$datapath,
                     data19 = input$sa1$datapath,
                     data20 = input$sa2$datapath,
                     data21 = input$se$datapath,
                     data22 = input$sk1$datapath,
                     data23 = input$sk2$datapath,
                     data24 = input$sl1$datapath,
                     data25 = input$sl2$datapath,
                     data26 = input$sl3$datapath,
                     data27 = input$sr2$datapath,
                     data28 = input$vs$datapath,
                     data29 = input$vc1$datapath,
                     data30 = input$vc2$datapath
      )
      id <- showNotification(
        "Rendering Report...",
        duration = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      rmarkdown::render("EDC-Review.Rmd",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
}

shinyApp(ui, server)





## Update Add Copy button on 15-Mar-2022
# library(shiny)
# library(bslib)
# library(shinyFeedback)
# library(rclipboard)
# options(shiny.maxRequestSize = 30*1024^2)
# 
# ui <- fluidPage(
#      theme = bslib::bs_theme(bootswatch = "journal"),
#      titlePanel("ADC-US-RES-21217 EDC Review"),
#      useShinyFeedback(),
#      a(span("Please email Alvin Lin if you run into any issues",style = "color:black"),href = "mailto:alvin.lin@abbott.com"),
#      fluidRow(column(12,
#                     textInput("study",h6("Please enter study",value = "")),
#                     verbatimTextOutput("text"),
#                     rclipboardSetup(),
#                     uiOutput("clip"))),
#      fluidRow(
#               column(3,
#                      fileInput("ae1","Please Upload AE1",accept = ".sas7bdat")
#                     ),
#               column(3,
#                      fileInput("ae2","Please Upload AE2",accept = ".sas7bdat")
#                     ),
#               column(3,
#                      fileInput("cad2","Please Upload CAD2",accept = ".sas7bdat")
#                     ),
#               column(3,
#                      fileInput("cad3","Please Upload CAD3",accept = ".sas7bdat")
#                     )
#              ),
#      fluidRow(
#               column(3,
#                      fileInput("cad4","Please Upload CAD4",accept = ".sas7bdat")
#                      ),
#               column(3,
#                      fileInput("cm1","Please Upload CM1",accept = ".sas7bdat")
#                      ),
#               column(3,
#               fileInput("cm2","Please Upload CM2",accept = ".sas7bdat")
#                      ),
#               column(3,
#               fileInput("co","Please Upload CO",accept = ".sas7bdat")
#                      )
#               ),
#      fluidRow(
#               column(3,
#               fileInput("de","Please Upload DE",accept = ".sas7bdat")
#                      ),
#               column(3,
#               fileInput("dm","Please Upload DM",accept = ".sas7bdat")
#                       ),
#               column(3,
#               fileInput("du2","Please Upload DU2",accept = ".sas7bdat")
#                     ),
#               column(3,
#               fileInput("ie","Please Upload IE",accept = ".sas7bdat")
#                     )
#               ),
#      fluidRow(
#               column(3,
#               fileInput("iv1","Please Upload IV1",accept = ".sas7bdat")
#                       ),
#               column(3,
#               fileInput("iv2","Please Upload IV2",accept = ".sas7bdat")
#                       ),
#               column(3,
#               fileInput("mh1","Please Upload MH1",accept = ".sas7bdat")
#                       ),
#               column(3,
#               fileInput("pd","Please Upload PD",accept = ".sas7bdat")
#                       )
#               ),
#      fluidRow(
#               column(3,
#               fileInput("qcrg1","Please Upload QCRG1",accept = ".sas7bdat")
#                       ),
#               column(3,
#               fileInput("qcrg2","Please Upload QCRG2",accept = ".sas7bdat")
#                       ),
#               column(3,
#               fileInput("sa1","Please Upload SA1",accept = ".sas7bdat")
#                       ),
#               column(3,
#               fileInput("sa2","Please Upload SA2",accept = ".sas7bdat")
#                     )
#               ),
#      fluidRow(
#               column(3,
#               fileInput("se","Please Upload SE",accept = ".sas7bdat")
#                     ),
#               column(3,
#               fileInput("sk1","Please Upload SK1",accept = ".sas7bdat")
#                     ),
#               column(3,
#               fileInput("sk2","Please Upload SK2",accept = ".sas7bdat")
#                     ),
#               column(3,
#               fileInput("sl1","Please Upload SL1",accept = ".sas7bdat")
#                     )
#               ),
#      fluidRow(
#               column(3,
#               fileInput("sl2","Please Upload SL2",accept = ".sas7bdat")
#                     ),
#               column(3,
#               fileInput("sl3","Please Upload SL3",accept = ".sas7bdat")
#                     ),
#               column(3,
#               fileInput("sr2","Please Upload SR2",accept = ".sas7bdat")
#                     ),
#               column(3,
#               fileInput("vs","Please Upload VS",accept = ".sas7bdat")
#                     )
#               ),
#     fluidRow(column(12, 
#                     downloadButton("download","Download Report",class = "btn-block"))
#              )
# )
# 
# server <- function(input, output, session) {
# 
#   ## EDC File Path
#   text <- reactive({
#     req(input$study)
#     
#     exists <- input$study %in% c("217")
#     feedbackWarning("study",!exists,"Unkown Study")
#     req(exists,cancelOutput = FALSE)
#     
#       # if (input$study %in% paste0("0",as.character(seq(47,91)))) {
#       cat(paste0("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC-US-RES-21217","\\","SE01","\\","OpenClinica","\\","Current","\\"))
#     # }
#   })
#   
#   output$text <-  renderPrint({text()})
#   
#   ## Copy button
#   
#   output$clip <- renderUI({
#     rclipButton(
#       inputId = "clipbtn",
#       label = "Copy Path",
#       clipText = paste0("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC-US-RES-21217","\\","SE01","\\","OpenClinica","\\","Current","\\"), 
#       icon = icon("clipboard")
#     )
#   })
#   
#   if (interactive()){
#     observeEvent(input$clipbtn, clipr::write_clip(paste0("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC-US-RES-21217","\\","SE01","\\","OpenClinica","\\","Current","\\")))
#   }
#   
#   
#   ## Download Report
#    output$download <- downloadHandler(
#       filename = function(){
#         paste0("ADC-US-RES-21217 EDC Review Report.html")
#       },
#       content = function(file){
#         params <- list(data1 = input$ae1$datapath,
#                        data2 = input$ae2$datapath,
#                        data3 = input$cad2$datapath,
#                        data4 = input$cad3$datapath,
#                        data5 = input$cad4$datapath,
#                        data6 = input$cm1$datapath,
#                        data7 = input$cm2$datapath,
#                        data8 = input$co$datapath,
#                        data9 = input$de$datapath,
#                        data10 = input$dm$datapath,
#                        data11 = input$du2$datapath,
#                        data12 = input$ie$datapath,
#                        data13 = input$iv1$datapath,
#                        data14 = input$iv2$datapath,
#                        data15 = input$mh1$datapath,
#                        data16 = input$pd$datapath,
#                        data17 = input$qcrg1$datapath,
#                        data18 = input$qcrg2$datapath,
#                        data19 = input$sa1$datapath,
#                        data20 = input$sa2$datapath,
#                        data21 = input$se$datapath,
#                        data22 = input$sk1$datapath,
#                        data23 = input$sk2$datapath,
#                        data24 = input$sl1$datapath,
#                        data25 = input$sl2$datapath,
#                        data26 = input$sl3$datapath,
#                        data27 = input$sr2$datapath,
#                        data28 = input$vs$datapath
#                       )
#         id <- showNotification(
#           "Rendering Report...",
#           duration = NULL,
#           closeButton = FALSE
#         )
#         on.exit(removeNotification(id), add = TRUE)
# 
#         rmarkdown::render("EDC-Review.Rmd",
#                           output_file = file,
#                           params = params,
#                           envir = new.env(parent = globalenv()))
#         }
#     )
# 
# }
# 
# shinyApp(ui, server)