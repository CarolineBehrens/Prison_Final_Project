library(tidyverse)
library(tidycensus)
library(usethis)
library(shiny)
library(shinyWidgets)
library(gtsummary)
library(shinythemes)
library(gt)
library(broom.mixed)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(dplyr)
library(forcats)
library(wesanderson)



library(readr)
total_mortality <- msfp0116stt12 <- read_csv("raw_data/msfp0116stt12.csv",
                                             col_types = cols(
                                               .default = col_character(),
                                               `2001` = col_double(),
                                               `2006` = col_double(),
                                               `2007` = col_double(),
                                               `2008` = col_double(),
                                               `2009` = col_double(),
                                               `2010` = col_double(),
                                               `2011` = col_double(),
                                               `2012` = col_double(),
                                               `2013` = col_double(),
                                               `2014` = col_double(),
                                               `2015` = col_double(),
                                               `2016` = col_double()
                                             ))
#View(msfp0116stt12)
crime_and_incarceration_by_state <- read_csv("raw_data/crime_and_incarceration_by_state.csv",
                                             col_types = cols(
                                               jurisdiction = col_character(),
                                               includes_jails = col_logical(),
                                               year = col_double(),
                                               prisoner_count = col_double(),
                                               crime_reporting_change = col_logical(),
                                               crimes_estimated = col_logical(),
                                               state_population = col_double(),
                                               violent_crime_total = col_double(),
                                               murder_manslaughter = col_double(),
                                               rape_legacy = col_double(),
                                               rape_revised = col_double(),
                                               robbery = col_double(),
                                               agg_assault = col_double(),
                                               property_crime_total = col_double(),
                                               burglary = col_double(),
                                               larceny = col_double(),
                                               vehicle_theft = col_double()
                                             )) 


fit_prisoner_data <- readRDS("raw_data/prisoner_fit.RDS")

#total_mortality %>%
  #rename(State/Federal == jurisdiction)


#crime_and_incarceration_by_state %>%
  #mutate(jurisdiction = str_to_title(jurisdiction)) %>%
 # filter(jurisdiction != "Federal")

total_mortality <- total_mortality %>%
  rename(jurisdiction = `State/Federal`) %>%
  filter(!(jurisdiction %in% c("Federal", "State"))) %>%
  select(-"2001 caveat", -"2006 caveat", -"2007 caveat", -"2008 caveat", -"2009 caveat",
         -"2010 caveat", -"2011 caveat", -"2012 caveat", -"2013 caveat", -"2014 caveat",
         -"2015 caveat", -"2016 caveat")

state_crime <- crime_and_incarceration_by_state %>%
  sample_n(1000, replace = TRUE) %>%
  mutate(name = str_to_sentence(jurisdiction)) %>%
  left_join(total_mortality, by = "jurisdiction") %>%
  select(name, year, violent_crime_total,agg_assault, 
         murder_manslaughter, robbery, property_crime_total,
         larceny, vehicle_theft) %>%
  pivot_longer(names_to = "type",
               values_to = "total",
               cols = c(property_crime_total, violent_crime_total))

#state_crime

#plot_1 <- state_crime %>%
  #filter(name.x == input$name) %>%
  #ggplot(aes(x = year, y = total/1000, color = type)) +
  #geom_point(size = 5)




# #fit_1 <- stan_glm(prisoner_count_in_thousands ~ population_in_thousands + south + violent_crime_in_thousands + south*violent_crime_in_thousands,
#  #                 data = clean_crime, 
#                   seed = 17,
#                   refresh = 0)
# print(fit_1, digits = 4)


#saveRDS(fit_1, file = "prisoner_fit.RDS")


mortality_numbers <- total_mortality %>%
  pivot_longer(names_to = "Year",
               values_to = "Mortality_Number",
               cols = c(`2001`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`,
                        `2013`, `2014`,`2015`,`2016`)) %>%
  mutate(south = ifelse(jurisdiction %in% c("Alabama", "Florida", "Georgia", "Kentucky",
                                            "Louisiana", "Maryland", "Mississippi",
                                            "North Carolina", "Oklahoma", "South Carolina",
                                            "Tennesse", "Texas", "Virginia", "West Virginia"),
                        TRUE, FALSE))



# mortality_plot <- ggplot(data = mortality_numbers, 
#                          mapping = aes(x = Year, 
#                                        y = Mortality_Number, 
#                                        color = south)) +
#   geom_point(alpha = 0.5) + 
#   labs(Title = "Mortality Numbers in Prisons",
#        subtitle = "Typically Higher Mortality in Southern State Prisons",
#        x = "Year", y = "Mortality Number")

#mortality_plot


 mortality_plot <- qplot(Year, Mortality_Number, data = mortality_numbers, 
        geom= "violin", fill = south, outlier.color = "transparent") 








fit_prisoner <-tbl_regression(fit_prisoner_data,
                              intercept = TRUE,
                              estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  as_gt() %>%
  tab_header(title = "Prisoner Count Varies Greatly by Population", 
             subtitle = "Southern States Have More Crime  ") %>%
  tab_source_note(md("Source: "))

#fit_prisoner

clean_crime <- crime_and_incarceration_by_state %>%
  mutate(south = ifelse(jurisdiction %in% c("Alabama", "Florida", "Georgia", "Kentucky",
                                            "Louisiana", "Maryland", "Mississippi",
                                            "North Carolina", "Oklahoma", "South Carolina",
                                            "Tennesse", "Texas", "Virginia", "West Virginia"),
                        TRUE, FALSE)) %>%
  mutate(population_in_thousands = state_population/1000) %>%
  mutate(prisoner_count_in_thousands = prisoner_count/1000) %>%
  mutate(violent_crime_in_thousands = violent_crime_total/1000) %>%
  drop_na(population_in_thousands) %>%
  drop_na(prisoner_count_in_thousands) %>%
  drop_na(violent_crime_in_thousands)


crime_w_bins <- clean_crime %>%
  mutate(population_bins = case_when(population_in_thousands < 8000 ~ 1,
                                     population_in_thousands >= 8000 & population_in_thousands <16000 ~ 2,
                                     population_in_thousands >= 16000 & population_in_thousands <24000 ~ 3,
                                     population_in_thousands >= 24000 & population_in_thousands < 32000 ~ 4,
                                     population_in_thousands >= 32000 & population_in_thousands < 40000 ~ 5))



