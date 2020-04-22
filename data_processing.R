# remove all variables from the workspace
rm(list=ls(all=TRUE))

library(deSolve)
library(tidyverse)
library(ggplot2)

baseURL <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/"

preprocess <- TRUE
dataDirectory <- "./data/"
startDate <- as.Date("22-01-2020",format="%d-%m-%Y")
endDate <- Sys.Date()

if (preprocess) {
  # only perform preprocessing of data when required
  
  # loop over all dates from start of measurements to today
  theDate <- startDate
  while (theDate <= endDate)
  {
    filename <- paste(dataDirectory, format(theDate, "%m-%d-%Y"), ".csv", sep = "")
    if (file.exists(filename)) 
    {
      data <- read_csv(filename)
    }
    else {
      print("File does not exist locally. Attempting to download.")
      try(
        foo <- download.file(url = paste(baseURL, format(theDate, "%m-%d-%Y") , ".csv", sep=""), destfile = filename, method="auto", quiet = TRUE),
        silent = TRUE)
      if (file.exists(filename)) 
      {
        data <- read_csv(filename)
      } else
      {
        print("Download failed")
        break
      }
    }
    
    ## alter the data structure
    if (theDate < as.Date("22-03-2020", format="%d-%m-%Y")) {
      data <- data %>% select("Province/State", "Country/Region", "Confirmed", "Deaths", "Recovered") %>% add_column("Measurement_Date" = theDate)
    }
    else {
      data <- data %>% select("Province_State", "Country_Region", "Confirmed", "Deaths", "Recovered") %>% add_column("Measurement_Date" = theDate)
      data <- data %>% rename("Province/State" = "Province_State") %>% rename("Country/Region" = "Country_Region")
      
    }
    if (!exists("BigData")) {
      BigData <- data
    }
    else {
      BigData <- bind_rows(BigData, data)
    }
    #print(filename)
    theDate <- theDate + 1
  }
}

# Now data is available
#DutchData <- BigData %>% filter(`Country/Region` == "Netherlands") %>% group_by(Measurement_Date) %>% summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered)) %>% gather(type, count, Confirmed:Recovered)
#country <- c("Mainland China", "China")
country <- c("Spain", "Italy", "France", "Germany")
plotData <- BigData %>% filter(`Country/Region` %in% country) %>% group_by(Measurement_Date) %>% summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered), Infected = sum(Confirmed) - sum(Recovered) - sum(Deaths))

# generate a plot of development in Netherlands over time?
#p <- ggplot(summarize(DutchData, Confirmed = sum(Confirp med)), aes(x=Measurement_Date, y=Confirmed)) + geom_point() + geom_line()
p <- ggplot(plotData, aes(x=Measurement_Date)) +
  geom_line(aes(y=Confirmed, color="Confirmed"), size=2) +
  #geom_point(aes(y=Confirmed)) 
  geom_line(aes(y=Deaths, color="Deaths"), size=2) +
  #geom_point(aes(y=Deaths, color="black") + 
  geom_line(aes(y=Recovered, color="Recovered"),size=2) +
  geom_line(aes(y=Infected, color = "Infected"),size=2) +
  labs(title = paste("COVID-19 for", paste(country,collapse="/")), x = 'Date', y = '# people') +
  scale_colour_manual(name = "Legend", values = c("skyblue", "red", "gold", "chartreuse")) +
  theme(legend.position = "bottom")

p

parameters <- c(
  BirthRate = 0,
  DeathRate = 0,
  RecoveryRate = 0.3,
  ContactRate = 1,
  IncubationRate = 1,
  DeceaseRate = 0.0
)

InitialState <- c(
  Susceptible = 999,
  Exposed = 0,
  Infected = 1,
  Recovered = 0
)

E_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    Total <- Susceptible + Exposed + Infected + Recovered
    dSusceptible <- BirthRate - DeathRate * Susceptible - ContactRate * Infected / Total * Susceptible
    dExposed <- ContactRate * Infected / Total * Susceptible - (DeathRate + IncubationRate) * Exposed
    dInfected <- IncubationRate * Exposed - (RecoveryRate + DeathRate + DeceaseRate) * Infected
    dRecovered <- RecoveryRate * Infected - DeathRate * Recovered
    return(list(c(dSusceptible, dExposed, dInfected, dRecovered)))  })
}

Time <- seq(0, 100, by = 0.1)

Out <- as.data.frame(ode(InitialState, Time, E_model, parameters),stringsAsFactors = FALSE)


Out$PopTotal <- rowSums(Out[,2:5])
#plot(Out[-c(1)])
#plot(Out$time, Out$Susceptible, type = 'l')
#sapply(3:6,function(x) lines(Out[1],Out[x], col = x))

# LVmod <- function(Time, State, Pars) {
#   with(as.list(c(State, Pars)), {
#     Ingestion    <- rIng  * Prey * Predator
#     GrowthPrey   <- rGrow * Prey * (1 - Prey/K)
#     MortPredator <- rMort * Predator
#     
#     dPrey        <- GrowthPrey - Ingestion
#     dPredator    <- Ingestion * assEff - MortPredator
#     
#     return(list(c(dPrey, dPredator)))
#   })
# }
# 
# pars  <- c(rIng   = 0.2,    # /day, rate of ingestion
#            rGrow  = 1.0,    # /day, growth rate of prey
#            rMort  = 0.2 ,   # /day, mortality rate of predator
#            assEff = 0.5,    # -, assimilation efficiency
#            K      = 10)     # mmol/m3, carrying capacity
# 
# yini  <- c(Prey = 1, Predator = 2)
# times <- seq(0, 100, by = 0.1)
# out   <- ode(yini, times, LVmod, pars)
# summary(out)