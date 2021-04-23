library(tidyverse)
library(tidycensus)
library(usethis)
library(rstanarm)




library(readr)
total_mortality <- msfp0116stt12 <- read_csv("raw_data/msfp0116stt12.csv")
View(msfp0116stt12)
crime_and_incarceration_by_state <- read_csv("raw_data/crime_and_incarceration_by_state.csv") 


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

state_crime

#plot_1 <- state_crime %>%
  #filter(name.x == input$name) %>%
  #ggplot(aes(x = year, y = total/1000, color = type)) +
  #geom_point(size = 5)




fit_1 <- stan_glm(violent_crime_total ~ year,
                  data = crime_and_incarceration_by_state, 
                  seed = 17,
                  refresh = 0)


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



mortality_plot <- ggplot(data = mortality_numbers, 
                         mapping = aes(x = Mortality_Number, 
                                       y = Year, 
                                       color = south)) +
  geom_point(alpha = 0.5) 

mortality_plot
