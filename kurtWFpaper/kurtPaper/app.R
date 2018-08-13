#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(DT)
library(foreign)
library(plotly)
library(shiny)
library(shinythemes)

foo <- read.csv("PoissonSandboxData.csv", stringsAsFactors = FALSE) %>% 
  rename(zip = bene_addr_zip,
         date = svc_from_dt) %>%
  transform(date = as.Date(date)) %>%
  mutate(all = white+hispanic+black+native_am+asian_pi+other+missing)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Sidebar with a slider input for number of bins 
  navbarPage(theme = shinytheme("yeti"),
              title ="Kurt Wildfire Paper",
              
    tabPanel(title = "Earlier Examination",
      fluidRow(
      column(3, 
      sliderInput(inputId = "const", label = "Constant to replace non cases", min = 0.00, max =  1, value =  .1, step = .05),
      sliderInput(inputId = "WF", label = "PM2.5 values above this number define the WF period", min = 0, max =  150, value =  0, step = 5)),
      column(9,  
             plotlyOutput("plot2", height = "800"),
             hr(),
             plotOutput("plot")
              
          )
      )
    ),
    
    
    tabPanel(title = "Comparing Exposure Levels",
             fluidRow(
               column(9, 
                      sliderInput(inputId = "exposure",
                                  label = "Select the cutpoints to define Medium exposure",
                                  min = 0, 
                                  max=150,
                                  round = TRUE,
                                  step = 1, 
                                  value = c(12, 35)
                                  )
                      
                      ),
               column(3, selectInput(inputId = "raceDropDown",
                                     label = "Select the Race of Interest (for right plot)",
                                     choices = c("all", "white","hispanic","black","asian_pi","native_am","other","missing")))
               
             ),
             fluidRow(
               column(4, 
                      wellPanel(h6("This plot shows where each zip falls in terms of the exposures as you have defined low, med, and high."),
                                hr(),
                                plotOutput("exposurePeriod", height = "700"))),
               column(4, 
                      wellPanel(h6("This plot pools zip codes and days that have the same exposure level to calculate the number of cases for each race."),
                                hr(),
                                plotOutput("casesPerDay", height = "700")
                    )
               ),
               column(4, 
                      wellPanel(h6("This plot show the actual risk differences (delta cases/day) by zip code for the selected race. Values greater than zero indicate more cases/day in the exposure period (med [blue] or high [red]) compared to the low exposure."), 
                      hr(),
                      plotOutput("riskdiffRace", height = "700")
                             
                      )
               )
             )
    ) 
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  foor <- reactive({
    foo %>% 
      mutate(wildfire = ifelse(pm_25 > input$WF, "WFperiod", "nonWFperiod"))
    
  })
  
####### make exposure chart ######## 
  

output$riskdiffRace <- renderPlot({
  
  
  raceSelection <- input$raceDropDown

   lowMed <- input$exposure[1]
   medHi <- input$exposure[2]
  
  
foo %>%
     mutate(exposure = factor(ifelse(pm_25<lowMed, "low", ifelse(pm_25<medHi, "med","high")), levels = c("low","med","high"))) %>%
     gather(all, white,hispanic,black,native_am,asian_pi,other,missing, key = race, value = count) %>%
     transform(race = factor(race, levels = c("all", "white","hispanic","black","asian_pi","native_am","other","missing"))) %>%
     select(zip, date, race, exposure, count)  %>% 
     group_by(race, zip, exposure) %>% 
     summarise(sum = sum(count),
               days = length(count),
               rate = sum(count)/length(count)) %>%
     gather(sum, days, rate, key = metric, value = value) %>%
     spread(key = exposure, value = value) %>%
     filter(metric == "rate" & race == raceSelection) %>%
     mutate(lowToMedRiskDiff = med - low, 
            lowToHighRiskDiff = high - low) %>%
     gather(lowToMedRiskDiff, lowToHighRiskDiff, key = comparison, value = RiskDiff) %>%
     ggplot(aes(y=reorder(zip, ), x=RiskDiff, color = comparison)) + geom_point(alpha = 0.5) + facet_grid(. ~ race)
  
  
})


output$casesPerDay <- renderPlot({
  
  
  raceSelection <- input$raceDropDown
  
  lowMed <- input$exposure[1]
  medHi <- input$exposure[2]
  
  
  read.csv("PoissonSandboxData.csv", stringsAsFactors = FALSE) %>% 
    rename(zip = bene_addr_zip,
           date = svc_from_dt) %>%
    transform(date = as.Date(date)) %>%
    mutate(all = white+hispanic+black+native_am+asian_pi+other+missing,
           exposure = factor(ifelse(pm_25<lowMed, "low", ifelse(pm_25<medHi, "med","high")), levels = c("low","med","high"))) %>%
    gather(all, white,hispanic,black,native_am,asian_pi,other,missing, key = race, value = count) %>%
    transform(race = factor(race, levels = c("all", "white","hispanic","black","asian_pi","native_am","other","missing"))) %>%
    select(zip, date, race, exposure, count)  %>% 
    group_by(race, exposure) %>% 
    summarise(cases = sum(count),
              days = length(count),
              casesPerDay = sum(count)/length(count)) %>%
    ggplot(aes(x=reorder(race, casesPerDay), y=casesPerDay, fill = exposure)) + geom_bar(stat="identity", position="dodge")  
  
  
})



 
######## plot to show zips by exposure #########



output$exposurePeriod <- renderPlot({
  
  
  lowMed <- input$exposure[1]
  medHi <- input$exposure[2]


  foo %>% 
    select(zip, all, date, pm_25) %>%
    filter(date > "2007-10-01") %>%
    ggplot(aes(x=date, y=pm_25, group= zip)) + 
    geom_point(size=0.7, alpha=0.2) + 
    geom_hline(yintercept = lowMed, linetype="dashed", color ="red") + 
    geom_hline(yintercept = medHi, linetype="dashed", color ="blue")

}) 

  
  ###### make WF period plots #################
  
  output$plot2 <- renderPlotly({ 
    plot_ly(
      data = foor(),
      x = ~ date,
      y = ~ factor(zip),
      type = "scatter",
      mode = "markers",
      text = paste0("zip:", foo[["zip"]],"</br> Date:", foo[["date"]], "</br> PM2.5 (ppm):", foo[["pm_25"]]),
      color = ~ wildfire,
      showlegend = TRUE
    ) %>%  
      layout(
        title = "San Diego 2007 Presence of Wildfire Smoke by Date and Zip",
        margin = list(l = 100),
        xaxis = list(
          title = "Date",
          autotick = TRUE,
          ticks = "outside",
          tick0 = 0,
          dtick = 1,
          ticklen = 5,
          tickwidth = 2,
          tickcolor = toRGB("black")
        ),
        yaxis = list(
          title = "Zip Code",
          autotick = TRUE,
          ticks = "outside",
          tick0 = 0,
          dtick = 25,
          ticklen = 5,
          tickwidth = 2,
          tickcolor = toRGB("black")
        ))  %>%
      config(
        collaborate = FALSE,
        displaylogo = FALSE,
        modeBarButtonsToRemove = list(
          'toggleSpikelines',
          'sendDataToCloud',
          'hoverCompareCartesian',
          'zoom2d',
          'pan2d',
          'select2d',
          'lasso2d',
          'zoomIn2d',
          'zoomOut2d',
          'autoScale2d',
          'resetScale2d',
          'hoverClosestCartesian'
        ))
    
  })
  
  
  output$plot <- renderPlot({
    merge({
      foor() %>%
        group_by(zip, wildfire) %>%
        summarise(
          white = sum(white),
          hispanic = sum(hispanic),
          black = sum(black),
          native_am = sum(native_am),
          asian_pi = sum(asian_pi),
          other = sum(other),
          days = length(date)
        ) %>%
        gather(
          white,
          hispanic,
          black,
          native_am,
          asian_pi,
          other,
          key = race,
          value = cases
        ) %>%
        select(-days) %>%
        spread(key = wildfire, value = cases) %>%
        mutate(remove = ifelse(WFperiod == 0 &
                                 nonWFperiod == 0, "remove", "keep")) %>%
        rename(WF_cases = WFperiod,
               noWF_cases = nonWFperiod)
    }, {
      foor() %>%
        group_by(zip, wildfire) %>%
        summarise(
          white = sum(white),
          hispanic = sum(hispanic),
          black = sum(black),
          native_am = sum(native_am),
          asian_pi = sum(asian_pi),
          other = sum(other),
          days = length(date)
        ) %>%
        gather(
          white,
          hispanic,
          black,
          native_am,
          asian_pi,
          other,
          key = race,
          value = cases
        ) %>%
        mutate(casesPerDay = ifelse(cases == 0, input$const /
                                      days, cases / days)) %>%
        select(-days,-cases) %>%
        spread(key = wildfire, value = casesPerDay) %>%
        rename(WF_rate = WFperiod,
               noWF_rate = nonWFperiod)
      
    }) %>%
      mutate(
        RateRatio = ifelse(remove == "remove", 1, WF_rate / noWF_rate),
        ln_RateRatio = log(RateRatio),
        weight = 1 / ((1 / WF_rate) + (1 / noWF_rate))
      )  %>%
      ggplot() + geom_boxplot(aes(x = race, y = ln_RateRatio)) + 
      geom_jitter(aes(x = race, y = ln_RateRatio, color =race), alpha = 0.2) + 
      guides(color = FALSE) +
      facet_wrap( ~ remove)
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

