#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ncdf4)
library(tidyverse)

locationData <- read.csv("https://raw.githubusercontent.com/vargovargo/wildfires/master/LOCAcounties.csv", header=T) %>% 
  select(lon, lat, County, ClimateRegion)

popMat <- c("L","bau","H")
names(popMat) <- c("Low","Central (bau)","High")

rcpMat <- c("45","85")
names(rcpMat) <- c("RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)")

modelMat <- c("CanESM2","CNRM-CM5","HadGEM2-ES","MIROC5")
names(modelMat) <- c("CanESM2 (average)", "CNRM-CM5 (Cool/Wet)","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs)")

process_single_nc <- function(modelVar, rcpVar, popVar){
  
  model <- modelMat[modelVar]
  rcp <- rcpMat[rcpVar]
  pop <- popMat[popVar] 
  
  dname = "hectares"
  ncdfURL <- paste0("http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/",model,"/rcp",rcp,"/",model,"_",rcp,"_AA.all.",pop,".mu.nc")
  dest <-  paste0("~/",model,"_",rcp,"_AA.all.",pop,".mu.nc") 
  download.file(url=ncdfURL,destfile=dest, mode = "wb") 
  
  ncin <- nc_open(dest)
  # store values from variables and atributes
  attributes(ncin$dim)$names
  nc_lat <- ncvar_get(ncin, "lat")
  n_lat <- dim(nc_lat)
  nc_lon <- ncvar_get(ncin, "lon")
  n_lon <- dim(nc_lon)
  nc_year <- ncvar_get(ncin, "time")+1953
  n_year <- dim(nc_year)
  
  # get hectares
  tmp_array <- ncvar_get(ncin,dname)
  dlname <- ncatt_get(ncin,dname,"long_name")
  dunits <- ncatt_get(ncin,dname,"units")
  fillvalue <- ncatt_get(ncin,dname,"_FillValue")
  dim(tmp_array)
  
  # get global attributes
  # title <- ncatt_get(ncin,0,"title")
  # institution <- ncatt_get(ncin,0,"institution")
  # datasource <- ncatt_get(ncin,0,"source")
  # references <- ncatt_get(ncin,0,"references")
  # history <- ncatt_get(ncin,0,"history")
  # Conventions <- ncatt_get(ncin,0,"Conventions")
  
  nc_close(ncin)
  
  # replace netCDF fill values with NA's
  tmp_array[tmp_array==fillvalue$value] <- NA
  
  # check number of missing data values
  # length(na.omit(as.vector(tmp_array[,,1])))
  # dimnames(tmp_array) <-list(lon=nc_lon, lat=nc_lat, year=nc_year)
  
  bar <- data.frame(expand.grid(lon = nc_lon, lat = nc_lat)) %>%
    cbind(matrix(as.vector(tmp_array), nrow = n_lon*n_lat, ncol = n_year)) %>% 
    mutate(model=model, scenario = rcp, population = pop) %>% 
    select(model, scenario, population, lon, lat, 3:150)
  
  names(bar) <- c("model","scenario","population","lon","lat",1954:2100) 
  
  
  CAonly <- inner_join(bar, locationData) %>%
    gather(6:152, key = "year", value = "hectares") %>% 
    mutate(year = as.integer(as.character(year))) %>% 
    mutate(period = factor(ifelse(year %in% c(1961:1990),"baseline (1961-1990)", 
                                  ifelse(year %in% c(2000:2020), "early (2000-2020)",
                                         ifelse(year %in% c(2040:2060),"mid-century (2040-2060)",
                                                ifelse(year %in% c(2080:2100), "late-century (2080-2100)", "between")))),
                           levels = c("baseline (1961-1990)","early (2000-2020)","mid-century (2040-2060)","late-century (2080-2100)","between"))) %>%
    mutate(climateModel = factor(ifelse(model == "CanESM2","CanESM2 (average)",
                                        ifelse(model == "CNRM-CM5","CNRM-CM5 (Cool/Wet)",
                                               ifelse(model == "HadGEM2-ES","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs"))),
                                 levels = c("CanESM2 (average)", "CNRM-CM5 (Cool/Wet)","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs)")),
           
           RCP = factor(ifelse(scenario == "45", "RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)"),
                        levels = c("RCP4.5 (emissions peak 2040, stabiliazation by 2100)","emissions continue to rise throughout the 21st century")), 
           
           PopulationGrowth = factor(ifelse(population == "AA.all.bau.mu.nc","Central Projection",
                                            ifelse(population == "AA.all.L.mu.nc","Low Projection","High Projection")),
                                     levels = c("Low Projection", "Central Projection","High Projection")))
  return(CAonly)
}


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Projected California Wildfires Vizualization"),
   
   # Sidebar with a radio input for model, RCP, and population  
   sidebarLayout(
      sidebarPanel(
         radioButtons("modelVar",
                     "Select a Climate Model",
                     c("CanESM2 (average)", "CNRM-CM5 (Cool/Wet)","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs)")),
         radioButtons("rcpVar",
                      "Select an Emissions Scenario",
                      c("RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)")),
         radioButtons("popVar",
                      "Select a Population Projection",
                      c("Low","Central (bau)","High")),
         # Download Button
         downloadButton("downloadWFcsv", "Download csv")
            
         ),
      
      # Show a plot of the generated distribution
      mainPanel(
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("plot")),
                    tabPanel("Graph", plotOutput("graph")),
                    tabPanel("Table", tableOutput("table"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$table <- renderTable({
    WFdata() %>% 
      filter(period != "between") %>%
      group_by(County, ClimateRegion, period) %>%
      summarise(mean_Ha_burned = mean(hectares, na.rm=T)) %>%
      spread(key = period, value = mean_Ha_burned)
    })
    
    
   WFdata <- reactive({
     
     process_single_nc(modelVar = input$modelVar, rcpVar = input$rcpVar, popVar = input$popVar)
    
     
  })
   
  
   output$graph <- renderPlot({
     
     ggplot(WFdata(), aes(x=year, y=hectares, color=ClimateRegion)) + geom_smooth()
     
   })
  
   
   output$plot <- renderPlot({
     
     WFdata() %>% 
       filter(period != "between") %>%
       group_by(period, lon, lat) %>%
       summarise(mean_Ha_burned = mean(hectares, na.rm=T)) %>%
       ggplot(aes(x=lon, y=lat, color=mean_Ha_burned)) + geom_point() + facet_grid(.~ period) +
       scale_colour_gradient(low = "white", high = "red")
     
   })
   
   
   # Downloadable csv of selected dataset ----
   output$downloadWFcsv <- downloadHandler(
     filename = function(){
       model <- modelMat[input$modelVar]
       rcp <- rcpMat[input$rcpVar]
       pop <- popMat[input$popVar]
       paste0("CA_wildfire_",model,"_",rcp,"_",pop,".csv")},
     content = function(file) {
       write.csv(WFdata(), file, row.names = FALSE)
     }
   )
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

