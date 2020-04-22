library(shiny)
library(ggplot2)
library(deSolve)
library(tidyverse)

# read in database
BigData <- read_csv('bigData.csv')

#extract country names 
countryList<- sort(unique(BigData$`Country/Region`))

#extract date range
  

# Define UI for app that draws the COVID 19 plot----
ui <- fluidPage(
  
  # App title ----
  titlePanel("COVIDator 19"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # select country
      selectInput(inputId = "countrySelect", label = "Select one or more countries",
                  choices = countryList, multiple = TRUE),
      
      dateRangeInput(inputId = "dateSelect", label = "Select a date range", 
                     start = "2020-02-01", 
                     min = "2020-01-01", max = Sys.Date())

      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      
      tags$div("Data is obtained from",  tags$a(href="https://coronavirus.jhu.edu/data", "John Hopkins University."), "Amount of infected people is calculated as the difference between the total number of confirmed cases and the number of deceased and recovered."),
      
    )
    
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  

  output$distPlot <- renderPlot({
    req(input$countrySelect)
    #validate(need(nrow(input$countrySelect) > 0, "Please select one or more countries"))

    country <- input$countrySelect
    dates <- input$dateSelect
    
    validate(need(dates[2] >= dates[1], "Please select a valid date range"))
    
    plotData <- BigData %>% filter(`Country/Region` %in% country & Measurement_Date >= dates[1] & Measurement_Date <= dates[2]) %>% 
      group_by(Measurement_Date) %>% 
      summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered), Infected = sum(Confirmed) - sum(Recovered) - sum(Deaths))
    # print(plotData)
    
    # exit in case there is not data to plot
    validate(
      need(nrow(plotData) > 0, "No data found.")
    )
    
    # generate a plot of development in Netherlands over time?
    #p <- ggplot(summarize(DutchData, Confirmed = sum(Confirp med)), aes(x=Measurement_Date, y=Confirmed)) + geom_point() + geom_line()
    p <- ggplot(plotData, aes(x=Measurement_Date)) +
      geom_line(aes(y=Confirmed, color="Confirmed"), size=2) +
      geom_line(aes(y=Deaths, color="Deaths"), size=2) +
      geom_line(aes(y=Recovered, color="Recovered"),size=2) +
      geom_line(aes(y=Infected, color = "Infected"),size=2) +
      labs(title = paste("COVID-19 for", paste(country,collapse="/")), x = 'Date', y = '# people') +
      xlim(dates[1],dates[2]) +
      scale_colour_manual(name = "Legend", values = c("skyblue", "red", "gold", "chartreuse")) +
      scale_x_date(date_minor_breaks = "1 week", date_breaks = "2 weeks")
      theme(legend.position = "bottom")

    p
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


