library(tidyverse)
library(tidycensus)
library(usethis)
library(rstanarm)




library(readr)
covid_prison_rates <- read_csv("milestone-3/raw_data/")
crime_and_incarceration_by_state <- read_csv("crime_and_incarceration_by_state.csv") 

state_crime <- crime_and_incarceration_by_state %>%
  mutate(name = str_to_sentence(jurisdiction)) %>%
  #inner_join(covid_prison_rates, by = "name") %>%
  select(name, year, violent_crime_total,agg_assault, 
         murder_manslaughter, robbery, property_crime_total,
         larceny, vehicle_theft, prisoner_cases_pct, prisoner_deaths_pct) %>%
  pivot_longer(names_to = "type",
               values_to = "total",
               cols = c(property_crime_total, violent_crime_total))

plot_1 <- state_crime %>%
  filter(name.x == input$name) %>%
  ggplot(aes(x = year, y = total/1000, color = type)) +
  geom_point(size = 5)




fit_1 <- stan_glm(violent_crime_total ~ year,
                  data = crime_and_incarceration_by_state, 
                  seed = 17,
                  refresh = 0)


vc_posterior <- fit_1 %>% 
  as_tibble() %>% 
  ggplot(aes(x = year, fill = type)) +
  geom_histogram(aes(y = after_stat(count/sum(count))),
                 alpha = 0.5, 
                 bins = 100, 
                 position = "identity") +
  labs(title = "Posterior for Average Age",
       subtitle = "More data allows for a more precise posterior for Democrats",
       x = "Age",
       y = "Probability") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic()
