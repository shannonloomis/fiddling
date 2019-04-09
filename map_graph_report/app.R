######################################################################
######################################################################
###                                                                ###
###                 ******************************                 ###
###                 * Created by: SHANNON LOOMIS *                 ###
###                 ******************************                 ###
###                                                                ###
###    This Shiny app was designed to create a map and a chart     ###
###         based on an input value selection                      ###
###    The resulting plot and any map navigation can be exported   ### 
###         to a word document for a user-defined report           ###
###    Reporting only works with the accompanying report.Rmd       ###
###         file in the same directory as the app.R file           ###
###                                                                ###
######################################################################
######################################################################





#################################
# INITIALIZE LIBRARIES AND DATA #
#################################


### LOAD LIBRARIES ###
library(ggplot2)
library(leaflet)
library(mapview)
library(shiny)


### INSTALL PHANTOM JS FOR PLOT EXPORT ###
webshot::install_phantomjs()


### CREATE DATASET ###

# Define number of datapoints
n = 23

# Pick random numbers for location value
set.seed(n)
df = data.frame(x = -87.600730 + rnorm(n)/100,
                y =  41.791076 + rnorm(n)/100)

# Add columns of random values to the data
df$a = runif(n,min = -2, max = 2)
df$b = runif(n,min = -2, max = 2)





###########################
# DESIGN THE FRONT END UI #
###########################

ui = fluidPage(
  column(12,
         h2("Generate a Report with Maps and Graphs"),
         selectInput("value",'Select Value:',c('a','b'))),
  column(6,plotOutput("plot")),
  column(6,leafletOutput("map")),
  column(12,downloadButton("report", "Generate report"))
)




#############################
# DEFINE THE BACKEND SERVER #
#############################

server = function(input, output, session) {
  
  ### RANDOM DATA PLOT ###
  
  # Create histogram of selected values
  # Needs to be done as reactive value so can pass both to UI and to report
  histogram = reactive({
    d = data.frame(value = df[,input$value])
    ggplot() + geom_histogram(data = d, aes(x = value),
                              bins=round(log(n,2)),
                              color="black", fill="darkred") + 
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(colour = "black"),
        legend.position="bottom"
      )
  })
  
  # Render plot on ui
  output$plot = renderPlot({
    histogram()
  })
  
  
  
  ### MAP OUTPUT ###
  
  # Create map
  reactive_map = reactive({
    
    # Define color pallet to color markers
    pal = colorNumeric("RdGy", -2:2)
    
    # Create map
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(lng = df$x,
                       lat = df$y,
                       radius = 10,
                       color = pal(df[,input$value]),
                       stroke = TRUE,
                       fillOpacity = 0.8)
  })
  
  # Display map
  output$map = renderLeaflet({
    reactive_map()
  })
  
  # Save the user map (if they've zoomed or moved around)
  # If you don't do this step and use the reactive_map instead, 
  #     the report will contain the default settings instead of any map navigation
  user_created_map <- reactive({
    m = reactive_map() %>%
      setView(lng = input$map_center$lng, lat = input$map_center$lat, 
              zoom = input$map_zoom)
    m
  })
  
  
  
  
  ### DOWNLOAD REPORT ###
  
  output$report = downloadHandler(
    
    # Create a word document so user has the ability to edit report if needed
    filename = "report.docx",
    content = function(file) {
      
      # # Define temp directory to write/copy files in case don't have write permissions to current directory
      # tmpDir = tempdir()
      # 
      # # Copy the report file to a temporary directory before processing
      # tempReport = file.path(tmpDir, "report.Rmd")
      # file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Save the histogram to the temp directory
      png(filename = "histogram.png",#file.path(tmpDir,"histogram.png"),
          width = 480, height = 480)
      print(histogram())
      dev.off()
      
      # Save the map to the temp directory
      mapshot(user_created_map(), 
              url = "user_map.html", #file.path(tmpDir,"user_map.html"),
              file = "user_map.png" #file.path(tmpDir,"user_map.png")
              )
      
      # Set up parameters to pass to Rmd document
      # We will use the number of data points and the data itself in the report
      params <- list(n = n,
                     df = df,
                     v = input$value)
      
      
      # Knit the document, passing in the params list
      rmarkdown::render("report.Rmd", output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}




#########################
# CALL UP THE SHINY APP #
#########################

shinyApp(ui = ui, server = server)
