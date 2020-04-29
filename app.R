library(shiny)
library(ggplot2)
library(deSolve)
library(tidyverse)

# read in database
BigData <- read_csv('bigData.csv')

#extract country names 
countryList<- sort(unique(BigData$`Country/Region`))
#europe <- list("Europe" = list("Germany","France","Spain"))
europe <- list("Europe" = paste("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Kosovo", "Liechtenstein", "Lithuania", "Luxembourg", "North Macedonia", "Malta", "Moldova", "Montenegro", "Monaco", "Netherlands", "Norway", "Poland", "Portugal", "Romania" ,"Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "UK", "United Kingdom", "Vatican City", "North Ireland", "Republic of Ireland", sep = "/"))
north_america <- list("North-America" = paste("Antigua and Barbuda","Anguilla","Aruba","The Bahamas","Barbados","Belize","Bermuda","Bonaire","British Virgin Islands","Canada","Cayman Islands","Clipperton Island","Costa Rica","Cuba","Curacao","Dominica","Dominican Republic","El Salvador","Greenland","Grenada","Guadeloupe","Guatemala","Haiti","Honduras","Jamaica","Martinique","Mexico","Montserrat","Navassa Island","Nicaragua","Panama","Puerto Rico","Saba","Saint Barthelemy","Saint Kitts and Nevis","Saint Lucia","Saint Martin","Saint Pierre and Miquelon","Saint Vincent and the Grenadines","Sint Eustatius","Sint Maarten","Trinidad and Tobago","Turks and Caicos","US","US Virgin Islands", sep = "/"))
south_america <- list("South-America" = paste("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","Falkland Islands","French Guiana","Guyana","Paraguay","Peru","South Georgia and the South Sandwich Islands","Suriname","Uruguay","Venezuela", sep = "/"))
africa <- list("Africa" = paste("Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi","Cameroon","Cape Verde","Central African Republic","Chad","Comoros","Republic of the Congo","Democratic Republic of the Congo","Cote d'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia, The", "Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia","Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia","South Africa","South Sudan","Sudan","Swaziland","Tanzania","Togo","Tunisia","Uganda","Western Sahara","Zambia","Zimbabwe", sep = "/"))
asia <- list("Asia" = paste("Afghanistan","Armenia","Azerbaijan","Bahrain","Bangladesh","Bhutan","Brunei","Cambodia","China","Cyprus","Timor Leste","Georgia","India","Indonesia","Iran","Iraq","Israel","Japan","Jordan","Kazakhstan","Kuwait","Kyrgyzstan","Laos","Lebanon","Malaysia","Maldives","Mongolia","Burma","Nepal","North Korea","Oman","Pakistan","Philippines","Palestine","Qatar","Russia","Saudi Arabia","Singapore","South Korea","Sri Lanka","Syria","Tajikistan","Thailand","Turkey","Turkmenistan","Taiwan","United Arab Emirates","Uzbekistan","Vietnam","Yemen", sep = "/"))
oceania <- list("Oceania" = paste("Australia","Federated States of Micronesia","Fiji","Kiribati","Marshall Islands","Nauru","New Zealand","Palau","Papua New Guinea","Samoa","Solomon Islands","Tonga","Tuvalu","Vanuatu", sep = "/"))

countryList <- (c(africa, asia, europe, north_america, oceania, south_america, countryList))

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
      #cat(file=stderr(), "blaat", "\n"),
      #print("woei"),
      
      plotOutput(outputId = "distPlot"),
      
      tags$div("Data is obtained from",  tags$a(href="https://coronavirus.jhu.edu/data", "John Hopkins University."), "Number of infected people is calculated as the difference between the total number of confirmed cases and the number of deceased and recovered."),
      
    )
    
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  

  output$distPlot <- renderPlot({
    req(input$countrySelect)
    #validate(need(nrow(input$countrySelect) > 0, "Please select one or more countries"))
    #cat(file=stderr(), "Rendering Plot", "\n")
    country <- input$countrySelect
    dates <- input$dateSelect
    
    validate(need(dates[2] >= dates[1], "Please select a valid date range"))
    print(country)
    foo <- (unlist(strsplit(country, "/")))
    plotData <- BigData %>% filter(`Country/Region` %in% foo & Measurement_Date >= dates[1] & Measurement_Date <= dates[2]) %>% 
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


