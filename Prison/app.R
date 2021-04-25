#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidycensus)
library(shinyWidgets)
library(gtsummary)
library(shinythemes)
library(gt)
library(bslib)
library(broom.mixed)

library(readr)
source("make_plots.R")

ui <- navbarPage(
    "Incarceration Numbers by State",
    tabPanel("Overview",
             fluidPage(theme = shinytheme("slate"),
                 titlePanel("Prisoner Trends Throughout States"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c(state_crime$name)
                         )),
                     mainPanel(plotOutput("state_crime"),
                               plotOutput("mortality_plot"))
             ))),
    tabPanel("Model",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them"),
             gt_output("fit_prisoner")),
    tabPanel("Deeper Dive into Crime Type",
             titlePanel("Discussion Title"), 
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "state",
                   "State",
                   c(crime_w_bins$jurisdiction)
                 ),
                 selectInput(
                   "crime",
                   "Crime",
                   c("Robbery","Vehicle Theft", "Murder Manslaughter",
                     "Rape", "Aggrevated Assault","Burglary", "Larceny"
                     ))
                 ),
               mainPanel(plotOutput("crime_per_state"))),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("I have always been interested in criminal justice, 
                and this project allows me to explore data revolving 
                the topic and explore the most common trends between 
                states."),
             p("So far I have loaded all of my data sets into this project. 
               I have not gotten to process my data yet, but my plan is 
               to filter out by jurisdiction, year, violent crime total 
               and by each type of crime. I want to show the trends 
               over the years and how crime trends have differed. I also want
               to load the covid prison rates and examine the differences 
               between every state. "),
      
             h3("About Me"),
             p("My name is Caroline Behrens and I study Economics. 
             You can reach me at Cbehrens@college.harvard.edu."),
             p(tags$a(href ="https://github.com/CarolineBehrens/practice_with_preceptor"))))
             
                         

#Define server logic required to draw a histogram

server <- function(input, output){
 output$state_crime <- renderPlot({ 
 plot_1 <- state_crime %>%
   filter(name == input$plot_type) %>%
    ggplot(aes(x = year, y = total/1000, color = type)) +
   geom_point(size = 5)
  plot_1
})
 
 
 output$mortality_plot <- renderPlot(mortality_plot)
 output$fit_prisoner <- render_gt(fit_prisoner)
 
 output$crime_per_state <- renderPlot({
   case_when(
     input$crime == "Robbery" ~ list(crime_w_bins %>%
         filter(jurisdiction == input$state) %>%
          ggplot(aes(y = robbery, x = year)) + 
          geom_line()),
     input$crime == "Vehicle Theft" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = vehicle_theft, x = year)) + 
                                       geom_line()),
     input$crime == "Murder Manslaughter" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = murder_manslaughter, x = year)) + 
                                       geom_line()),
     input$crime == "Rape" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = rape_legacy, x = year)) + 
                                       geom_line()),
     input$crime == "Aggrevated Assault" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = agg_assault, x = year)) + 
                                       geom_line()),
     input$crime == "Burglary" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = burglary, x = year)) + 
                                       geom_line()),
     input$crime == "Larceny" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = larceny, x = year)) + 
                                       geom_line())
  
   )
   
   
   
   # crime_per_state <- crime_w_bins %>%
   #   filter(jurisdiction == input$state) %>%
   #   ggplot(aes(y = input$crime, x = year)) + 
   #   geom_point()
   # crime_per_state
 })
} 

  

# Run the application 
shinyApp(ui = ui, server = server)



