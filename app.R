## Update Add Tabsets on 03-Aug-2022
library(shiny)
library(tidyverse)
library(fs)
library(shinyFeedback)
library(rclipboard)
options(shiny.maxRequestSize = 30*1024^2)

# External
m_drive <- gsub("\\\\", "/", r"(\\wf00168p.oneabbott.com\data1\CDM\)")

study_list <- dir_ls(gsub("\\\\", "/", r"(\\wf00168p.oneabbott.com\data1\CDM)"),type = "directory") |>
              file_info() |>
              # Filter Year >= 2022
              filter(year(modification_time) >= 2022) |>
              # Filter Study
              filter(str_detect(path,"RES|PMS|VAL|EXP")) |>
              # Length
              mutate(Path = str_extract(path, regex("(?<=CDM/).+"))) |> 
              filter(str_length(Path) == 16) |> 
              pull(Path)

ui <- fluidPage(
        theme = bslib::bs_theme(bootswatch = "minty"),
        titlePanel("ADC US External EDC Review"),
        useShinyFeedback(),
        a(span("Please email Alvin Lin if you run into any issues",style = "color:black"),href = "mailto:alvin.lin@abbott.com"),
        fluidRow(column(12,
                  selectInput("study",h6("Please enter study"), study_list,width = "400px"),
                  uiOutput("new"),
                  verbatimTextOutput("text"),
                  selectInput("label","Column Label",choices = c(TRUE,FALSE),selected = FALSE),
                  rclipboardSetup(),
                  uiOutput("clip"))),
                  br(),
                  actionButton("reset","Refresh"),
                  br(),
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
             # fileInput("sa1","Please Upload SA1",accept = ".sas7bdat"),
             fileInput("sa","Please Upload SA",accept = ".sas7bdat")),
    tabPanel(p("Study Exit",style = "font-size:20px;"),
             fileInput("se","Please Upload SE",accept = ".sas7bdat")),
    tabPanel(p("Skin Assessment",style = "font-size:20px;"),
             fileInput("sk1","Please Upload SK1",accept = ".sas7bdat"),
             fileInput("sk2","Please Upload SK2",accept = ".sas7bdat")),
    tabPanel(p("Study Set-up",style = "font-size:20px;"),
             fileInput("sl1","Please Upload SL1 or SSU1",accept = ".sas7bdat"),
             fileInput("sl2","Please Upload SL2 or SSU2",accept = ".sas7bdat"),
             fileInput("sl3","Please Upload SL3 or SSU3",accept = ".sas7bdat")),
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
      if (input$study %in% c("ADC-US-RES-22225")) {
      event <- dir_ls(gsub("\\\\", "/", r"(\\wf00168p.oneabbott.com\data1\CDM\ADC-US-RES-22225\)"), type = "directory", regexp = "SE") |>
        str_extract("(?<=[:digit:]{5}/).+")
      selectInput("event", "Please select study event", event, width = "400px")
    } else if (input$study %in% c("ADC-US-RES-21211")) {
      event <- dir_ls(gsub("\\\\", "/", r"(\\wf00168p.oneabbott.com\data1\CDM\ADC-US-RES-21211\)"), type = "directory", regexp = "SE") |>
        str_extract("(?<=[:digit:]{5}/).+")
      selectInput("event", "Please select study event", event, width = "400px")
    } else if (input$study %in% c("ADC-US-RES-23241")) {
      event <- dir_ls(gsub("\\\\", "/", r"(\\wf00168p.oneabbott.com\data1\CDM\ADC-US-RES-23241\)"), type = "directory", regexp = "SE") |>
        str_extract("(?<=[:digit:]{5}/).+")
      selectInput("event", "Please select study event", event, width = "400px")
    }
    else {
      return(NULL)
    }
  })
  

  
  text <- reactive({
    req(input$study)
    # study event
    if (input$study %in% c("ADC-US-RES-22225","ADC-US-RES-21211","ADC-US-RES-23241")) {
      cat(str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\",input$study,"\\",input$event,"\\","Openclinica","\\","Current"))
    } 
    # Without study event
    else if (!(input$study %in% c("ADC-US-RES-22225","ADC-US-RES-21211","ADC-US-RES-23241"))) {
      cat(str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\",input$study,"\\","Openclinica","\\","Current"))
    } 
  
  })
  
  output$text <- renderPrint({text()})
  
  ## Copy button
  
  output$clip <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy Path",
      clipText =  
        if (input$study %in% c("ADC-US-RES-22225","ADC-US-RES-21211","ADC-US-RES-23241")) {
          str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\",input$study,"\\",input$event,"\\","Openclinica","\\","Current")
          # str_c(m_drive,input$study,"\\",input$event,"\\","Openclinica","\\","Current")
      } 
      else if (!(input$study %in% c("ADC-US-RES-22225","ADC-US-RES-21211","ADC-US-RES-23241"))) {
        str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\",input$study,"\\","Openclinica","\\","Current")
      }
      ,
      icon = icon("clipboard")
    )
  })
  
  
  ## Download Report
  output$download <- downloadHandler(
    filename = function(){
      str_c(input$study," ",input$event," EDC Review Report ",Sys.Date(),".html")
    },
    content = function(file){
      params <- list(Study = input$study,
                     label = input$label,
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
                     # data19 = input$sa1$datapath,
                     data20 = input$sa$datapath,
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
      
      rmarkdown::render("External-EDC-Review.Rmd",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
}

shinyApp(ui, server)
