# Load packages
library(shiny)
library(shinythemes)
source("global.R")  

# input the default reference profiles and example cancer samples(if necessary)
#default_profiles <- read.table(file = "data/immune_14types_cell_marker.txt", header = T, row.names = 1)
#default_samples <- read.table(file = "data/sample.txt", header = T, row.names = 1)

ui <- tagList(
  #shinythemes::themeSelector(), #select interface theme
  navbarPage(
    #navigation bar 
    title="TCAP",
    footer=div(
      fluidRow(
        style="width:1500px;margin:auto;",
        #hr(style="color:white"),
        p(align="center","Copyright",a("Guo Lab", href = "http://bioinfo.life.hust.edu.cn/home_page#!/", target = "_blank", style = "color:#008176"),",",a("College of Life Science and Technology", href = "http://life.hust.edu.cn/", target = "_blank", style = "color:#008176"),
               ",",a("HUST", href = "http://www.hust.edu.cn/", target = "_blank", style = "color:#008176"),", China"
        )
      )
      
    ),
    tabPanel(
      "Home",
      fluidPage(
        div(id="welcomepage",
            style="box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);padding: 8px;border: 1px solid #ccc;border-radius: 5px;max-width: 1500px;margin: 0 auto;background-color: #EDEDED",
            div(
            h2("Welcom to TCAP",style = "color:#3F96C3")
            ),
            br(),
            br(),
        sidebarLayout(
          sidebarPanel(h3("What we do",style = "color:#3F96C3"),
                       p("TCAP (T Cell Abundance Prediction) is a tool designed to predict the T cell abundance in bulk samples.
                         In total, abundance of 15 T cell subtypes can be detected, which are CD4 naive, CD8 naive, central memory, cytotoxic, effector memory, exhaustion, iTreg, nTreg, Tr1, MAIT, Tfh, Th1, Th17, Th2. The data input allowed to be RNA-seq data or Chip-seq data.
                        "),
                       br(),
                       br(),
                       br(),
                       h3("About the work",style = "color:#3F96C3"),
                       p("We used ssgsea from GSVA R pacakage to help us figure out the abundance of T cell subtypes in sample,
                         the marker gene sets was collected from literature, and the pure expression profile of 15 T cell subtypes collected from GEO,
                         and the figure on the right is our workflow")),
          mainPanel(tabsetPanel(
            tabPanel(
              "Overview",
              div(style="text-align:center",
                         img(src="flow_chart.png",width="700px",height="450px"))),
            tabPanel(
              "Get started",
              h4("1. About the input",style = "color:#3F96C3"),
              p("The input file should be ended up with .txt and seperated by tab, the rowname of the file should 
                be gene symbol, and column name should be sample, single sample or multi samples is allowed click 
                on browse button on Run page, then you can choose your file match the condition above, when you 
                see 'Upload complete' on the blue progress bar, you file was uploaded successful"),
              h4("2. About the prediction process",style = "color:#3F96C3"),
              p("After uploading you file, you need to choose the data type of you sample, they should be one 
                of RNA-seq and Chip-seq, we will pick up the corresponding formular we mentioned in the flowchart 
                to calculated the T cell sybutype abundance of samples, after this, you can click on Run button to get your result"),
             
              h4("3. About the result",style = "color:#3F96C3"),
              p("The result was presented in the format of table and figure, which is downloadable. The rowname of the table is sample names,
                and column name of the table is 15 T cell subtypes including CD4 naive, CD8 naive, central memory, cytotoxic, effector memory, exhaustion, iTreg, nTreg, Tr1, MAIT, Tfh, Th1, Th17, Th2.")
          )) )
        )   
           
       )
      )
    ),
    tabPanel(
      "Run",
      fluidPage(
        div(style="box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);padding: 8px;border: 1px solid #ccc;border-radius: 5px;width:100%;max-width: 1500px;margin: 0 auto;background-color: #EDEDED",
          tabPanel("Run", icon=icon("home"),
                     #main title         
                     titlePanel(div("Welcome to TCAP",
                                    style = "color:#3F96C3")), 
                     br(),
                     br(),
                    sidebarLayout(
                       div( class="col-lg-2",
                            div(class="well",
                             conditionalPanel(
                               'input.dataset === "Results"',
                               h3("Input parameters", style = "color:#3F96C3"),
                               h5(strong("You can choose the example file or your own data as input")),
                               downloadLink('downloadData', 'example'),
                               fileInput("file",
                                         h5("Maximum file size is 50MB"),
                                         accept = c(
                                           "text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")), 
                               #input cancer samples file
                               #textOutput("file_information"),
                               br(),
                               
                               selectInput("sample_type",
                                           label = "Select Sample Type",
                                           choices = list("RNA-seq"="rnaseq",
                                                          "Chip-seq"="chip"),
                                           selected = "rnaseq"),
                               
                               br(),
                               textInput("job",
                                         label = "Give a job name to use when exporting files",
                                         value = "TCAP_example_result"),
                               br(),
                               
                               #button to run software
                               actionButton("action", "Run", icon = icon("rocket"))
                           
                         ),
                         conditionalPanel(
                           'input.dataset === "Figure"',
                           
                           radioButtons("dist", "Figure type:",
                                        c("BoxPlot" = "boxplot",
                                          "Violin" = "violin"
                                        )),
                              textInput("title",
                                     label = "Give a job name to use when exporting files",
                                     value = "TCAP_example_figure")
                         )
                            )
                       ),
                       div(class="col-lg-10",
                             tabsetPanel(
                               id="dataset",
                               tabPanel(
                                 "Results",
                                 helpText("Please select the parameters and then click on 'Run'"),
                                 
                                 #display the result
                                 uiOutput("predict_result"),
                                 
                                 #download the result file
                                 uiOutput("file_download")
                                 
                               ),
                               tabPanel(
                                 "Figure",
                                 #download the result file
                                 uiOutput("You can choose to browse figure of the input data or cancer from TCGA "),
                               
                                 
                                 #display the result
                                 uiOutput("figure_result"),
                                 uiOutput("figure_download")
                               )
                             )
                    
                       )
                     ))
        )
      )
      ),
   
    tabPanel("Document", icon = icon("question-circle"),
             div(
               style="box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);padding: 8px;border: 1px solid #ccc;border-radius: 5px;width:100%;max-width: 1500px;margin: 0 auto;background-color: #EDEDED",
               h3("TCAP (T Cell Abundance Prediction)",style = "text-align:center"),
               h4("1. Overview"),
               div(
                 p("TCAP was designed to predict T cell Abundance, the method we used was ssgsea from GSVA R package, 15 T cell subtype marker gene sets was collected from GEO, including 330 samples of 24 studies. Next, maker genes of those T cell subtypes was collected by literature research. The tool can handle the RNA-seq sample and chip-seq sample, below is our flowchart."),
                 div(style="text-align:center",
                     img(src="flow_chart.png",width="800px", height="500px")
                 )),
               h4("2. About the input"),
               p("The input file should be ended up with .txt and seperated by tab, the rowname of the file should be gene symbol, and column name should be sample names, single sample or multi samples is allowed. Click on browse button on Run page, then you can choose your file match the condition above to upload , when you see 'Upload complete' on the blue progress bar, you file was uploaded successful."),
               h4("3. How to run TCAP"),
               p("After uploading your file, you need to choose the data type of you sample, they should be one of RNA-seq and Chip-seq, we will pick up the corresponding formular we mentioned in the flowchart to calculated the T cell sybutype abundance of samples, after this, you can click on Run button to get your result."),
               h4("4. About the result"),
               p("The result was presented in the format of table and figure, which is downloadable. The rowname of the table is sample names, and column name of the table is 15 T cell subtypes including CD4 naive, CD8 naive, central memory, cytotoxic, effector memory, exhaustion, iTreg, nTreg, Tr1, MAIT, Tfh, Th1, Th17, Th2. And for the figure, we prepared boxplot and violin to present the result.
                 "),
               div(
                 p("Here is an example of result table:"),
                 div(
                   style="text-align:center",
                   img(src="result_table.png")
                 )),
               div(
                 p("Here is an example of result figure:"),
                 div(
                   style="text-align:center",
                   img(src="result_figure.png",width="1000px")
                 ))
               )
             
             
    #    tabPanel("About", icon = icon("info-circle"), "Something about the software.")
)
)
)

# Server logic
server <- function(input, output) {
  options(shiny.maxRequestSize = 50*1024^2)
  go <- reactiveValues(doRun = F)
  output$downloadData <- downloadHandler(
    filename = function() {
      "TCAP_example.txt"
    },
    content = function(file) {
      write.table(data, file)
    }
  )
  
  sample_file <- reactive({
    if (is.null(input$file)){
      return(NULL)
    }
    else{
      return(read.table(input$file$datapath, header = T, row.names = 1))
    }
  }) 
  #get the predict_result dynamically
  result <- reactive({
    getResult(sample_file(),input$sample_type)
  })
  
  #trigger response
  observeEvent(input$action,{ go$doRun = input$action})
  observeEvent(input$file, { go$doRun = F }) 
  #observeEvent(input$figure_type)
  output$predict_result <- renderUI({
    if (go$doRun == F) return()
    #remind user that there are parameters not filled
    isolate({
      if((is.null(input$file))){
        h3("Please fill all the parameters!!!", style = "color:red")
      }
      else{
        DT::dataTableOutput("result_table")
      }
    })
  })

  #display the result in table forma
  output$result_table <- DT::renderDataTable(
    DT::datatable(result(), options = list(pageLength = 10,searching = FALSE,sScrollX=TRUE))
  )
  
  #how to dispaly the result table and download result file
  output$file_download <- renderUI({
    if (go$doRun == F) return()
    else{
      fluidRow(
        column(3,
               downloadButton("download", "Download")))
    }
  }) 
  
  output$download <- downloadHandler(
    filename = function(){
      paste(input$job, ".txt")
    },
    content = function(file){
      write.table(T_FRE, file, row.names = T)
    }
  )
  output$figure_result<-renderUI({
    plotOutput("result_plot",width=1000,height=600)
  })
  output$result_plot <- renderPlot(plot_result(input$dist))
  output$figure_download <- renderUI({
    fluidRow(
      column(3,
             downloadButton("download_figure", "Download")))
  }) 
  output$download_figure <- downloadHandler(
    filename = function() { paste(input$title, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plot_result(input$dist), device = "png")
    }
  )
  
}
# Run the app
shinyApp(ui = ui, server = server)



