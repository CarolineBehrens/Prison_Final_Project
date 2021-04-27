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
library(wesanderson)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(dplyr)
library(forcats)

library(readr)
source("make_plots.R")

ui <- navbarPage(
    "Behind Bars",
    tabPanel("Overview",
             fluidPage(theme = shinytheme("sandstone"),
                 titlePanel("Crime and Mortality Trends from 2001-2016"),
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
             titlePanel("Model Discussion"),
             h3("Preliminary Thoughts"),
             p("From my initial data exploration" ),
             
             h3("Mathematical Model"),
             #withMathJax(),
             #helpText('$$ prisoner\_count\_in\_thousands_i = \beta_0 + 
                     # \beta_1 population\_bins_i + \beta_2 south_i + 
                     # \beta_3 violent\_crime\_in\_thousands_i + 
                     # \beta_4 south*violent\_crime\_in\_thousands_i + 
                    #  \epsilon_i$$'),
             h3("Table"),
             gt_output("fit_prisoner")),
    
    h3("Analysis"),
    p("As we can see from the table of this predictive model,"),
    
    
    tabPanel("Deeper Dive into Crime Type",
             titlePanel("Crime Trends Per State"), 
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
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background Motivations"),
             p("So far"),
      
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
   geom_point(size = 3)
  plot_1
})
 
 
 output$mortality_plot <- renderPlot(mortality_plot)
 output$fit_prisoner <- render_gt(fit_prisoner)
 
 output$crime_per_state <- renderPlot({
   case_when(
     input$crime == "Robbery" ~ list(crime_w_bins %>%
         filter(jurisdiction == input$state) %>%
          ggplot(aes(y = robbery, x = year)) + 
          geom_point(color = 'purple') + 
            geom_smooth()),
     input$crime == "Vehicle Theft" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = vehicle_theft, x = year)) + 
                                       geom_point(color = 'blue') + 
                                          geom_smooth()),
     input$crime == "Murder Manslaughter" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = murder_manslaughter, x = year)) + 
                                       geom_point(color = 'red') +
                                          geom_smooth()),
     input$crime == "Rape" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = rape_legacy, x = year)) + 
                                       geom_point(color = 'green') +
                                     geom_smooth()),
     input$crime == "Aggrevated Assault" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = agg_assault, x = year)) + 
                                       geom_point(color = 'blue') +
                                          geom_smooth()),
     input$crime == "Burglary" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = burglary, x = year)) + 
                                       geom_point(color = 'red') +
                                         geom_smooth()),
     input$crime == "Larceny" ~ list(crime_w_bins %>%
                                       filter(jurisdiction == input$state) %>%
                                       ggplot(aes(y = larceny, x = year)) + 
                                       geom_point(color = 'green') +
                                        geom_smooth())
  
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



