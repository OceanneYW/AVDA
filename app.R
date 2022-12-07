#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Cleanup environment
rm(list = ls())

tryCatch(
  {
    setwd(getSrcDirectory()[1])
  },
  error = function(e) {
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }
)

# Helper functions
source("dependencies.R")
source("parse.R")

# Plot functions
source("mds.R")
source("dendro.R")
source("envfit.R")
source("cluster.R")
source("report.R")
source("kmeans.R")

# Define UI for application that draws a histogram 
ui <- fluidPage(
  setBackgroundImage(src = "/Data-Visualization_Blog-scaled.jpeg"),

  # Application title with style added
   div(
     style = "background-color: rgba(0, 0, 0, 0.5); padding: 20px; border: 1px solid #202020; border-radius: 10px; margin-top: 20px; margin-bottom: 20px; color: #ffffff; line-height: 1; font-size: 30px; font-weight: bold; text-align: center;",
     "Advanced Data Analysis and Visualization App"
   ),

  # Sidebar for options
  #File uploading window
  #MDS (Clusters, Driving force?)
  #Dendrogram
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", h3("Upload .csv file")),

      conditionalPanel(condition = "input.cur_tab == 'MDS'",
        h4("MDS"),
        #Default algo: bray
        selectInput("mds_algo", "Scaling Algorithms",
                    list("Bray-Curtis" = "bray",
                    "Euclidean" = "euclidean")),
        h5("Options:"),
        checkboxInput("mds_binary", "Binary"),
        # checkboxInput("mds_drive_force", "Drive Force"),
        # checkboxInput("mds_clusters", "Clusters"),
        radioButtons("additional", "Additional:",
                     c("None" = "none",
                       "Drive Force" = "mds_drive_force",
                       "Clusters" = "mds_clusters"
                       )),
      ),
      
      conditionalPanel(condition = "input.cur_tab == 'k-Means'",
        h4("K-Means Clustering"),
        checkboxInput("kMeans_clusters", "K-means Clusters"),
        selectInput("method", "Methods",
                    list("Elbow" = "wss",
                         "Silhouette" = "silhouette",
                         "gap_stat" = "gap_stat")),
      ),
      
      conditionalPanel(condition = "input.cur_tab == 'Hierarchical Clustering'",
        h4("Hierarchical Clustering"),
        selectInput("dendro_algo", "Scaling Algorithms",
                    list("Bray-Curtis" = "bray",
                         "Euclidean" = "euclidean",
                         "Manhattan" = "manhattan")),
        h5("Options:"),
        checkboxInput("hc_binary", "Binary"),
      ),
    ),


    # Shows plots and results
    #Enable downloads for the plots
    mainPanel(
      div(
      tabsetPanel(id="cur_tab",
        tabPanel("Data", dataTableOutput("rData")),
        tabPanel("k-Means", 
                 plotlyOutput("plot_kMeans"), 
                 downloadButton('download_kmeans'),
                 conditionalPanel(condition = "input.kMeans_clusters",
                                  sliderInput("kmeans_clusters", "Number of clusters:",min = 1, max = 10, value = 1, step = 1))
        ),
        tabPanel("MDS", 
                plotlyOutput("plot_mds"),
                  downloadButton('download_mds'),
                    conditionalPanel(condition = "input.additional == 'mds_clusters'",
                                     sliderInput("num_clusters", "Number of clusters:",min = 1, max = 10, value = 1, step = 1))
            
            ), 
        tabPanel("Hierarchical Clustering",plotlyOutput("plot_dendro"), downloadButton('download_dendro')),
          
        #TODO: sink() report generation
        tabPanel("Report", pre(textOutput("logHistory")), downloadButton('download_report')),
      ),
      
      style = "background-color: #ffffff; padding: 5px;"
      )
    ),
  ),
)

server <- function(input, output) {
  set.seed(55)
  #Creates data object from input file
  rData <- reactive({
    validate(
      need(input$file1$datapath, "")
    )
    parse(input$file1$datapath)
  })
  
  #Creates an empty list in a variable for results logging
  vals <- reactiveValues(log=c())
  
  observeEvent(input$file1$datapath, {
    vals$log <- append(vals$log, paste("File Upload ", input$file1$datapath))
  })
  
  mds_clusters_on <- reactive({
    input$additional == 'mds_clusters'
  })
  
  #Eventlistener for MDS clustering input changes
  clustersListener <- reactive({list(input$num_clusters, mds_clusters_on())})
  
  # Log mds clusters "Off" or number of mds clusters set
  observeEvent(clustersListener(), {
    #Only log numbers of clusters when the toggle is on
    text <- if (mds_clusters_on()) input$num_clusters else "Off"
    vals$log <- append(vals$log, paste("Clusters: " , text))
  })
  
  rMdsData <- reactive({
    calcMds(rData(), input$mds_algo, binary = input$mds_binary)
  })
  
  # Watch and log MDS algorithm input
  observeEvent(input$mds_algo, {
    vals$log <- append(vals$log, paste("MDS algorithm selected: ", input$mds_algo))
  })
  
  rPlotMds <- reactive({
    data <- rData()
    algo <- input$mds_algo
    nms <- rMdsData()
    plot <- ggplot()
    #If cluster toggles, plot pentagons on the mds plot
    if (mds_clusters_on()) plot <- appendClustersToPlot(calcDendro(data,algo,input$mds_binary), nms, plot, input$num_clusters)
    if(input$additional == 'mds_drive_force') plot <-plotEnvfit(calcEnvfit(nms,data), nms)
    else plot <- plotMds(plot, nms)
    return(plot)
  })
  
  rPlotMdsPrint <- reactive({
    data <- rData()
    algo <- input$mds_algo
    nms <- rMdsData()
    plot <- ggplot()
    if (mds_clusters_on()) plot <- appendClustersToPlot(calcDendro(data, algo, input$mds_binary), nms, plot, input$num_clusters)
    if(input$additional == 'mds_drive_force') plot <-plotEnvfit(calcEnvfit(nms,data), nms)
    else plot <- plotMds(plot, nms, T)
    return(plot)
  })
  
  rPlotDendro <- reactive({
    data <- rData()
    plotDendro(calcDendro(data, input$dendro_algo, input$hc_binary))
  })
  
  output$rData <- renderDataTable({
      cbind(Names = row.names(rData()),rData())
    },   options = list(scrollX = TRUE, autoWidth = TRUE))
  
  output$plot_kMeans <- renderPlotly({
    data <- rData()
    
    if(input$kMeans_clusters) plotKMeans(data, input$kmeans_clusters)
    else optimalClusters(data, input$method)
  })
  
  output$plot_mds <- renderPlotly({
    plot <- rPlotMds()
    p <- ggplotly(plot)
    p
  })

  output$plot_dendro <- renderPlotly({
    plot <- rPlotDendro()
    ggplotly(plot)
  })
  
  
  output$download_mds <- downloadHandler(
    filename = "mds.pdf",
    content = function (file) {
      plot <- rPlotMdsPrint()
      ggsave(file, plot = plot, width=17, height=11, dpi=300, units="in")
    }
  )
  output$download_dendro <- downloadHandler(
    filename = "dendrogram.pdf",
    content = function (file) {
      plot <- rPlotDendro()
      ggsave(file, plot = plot, width=17, height=11, dpi=300, units="in")
    }
  )
  output$logHistory <- renderText(paste(vals$log, collapse="\n"))
  
  output$download_report <- downloadHandler(
    filename = "Report.txt",
    content = function (file) {
     writeLines(paste(vals$log, collapse="\n"), file)
    }
  )
  # reactive({vals$log})
}


# Run the application
shinyApp(ui = ui, server = server)