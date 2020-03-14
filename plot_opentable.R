library(readxl)
library(tidyverse)

mycountry = "Canada" # Could be "United States"

state_opentable <- read_excel("opentable_data.xlsx", 
                               sheet = "state", 
                               col_types = c("text", "text", "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))
city_opentable <- read_excel("opentable_data.xlsx", 
                              sheet = "city", skip = 1,
                              col_types = c("text", "text", "text",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric"))

city_opentable_filtered = city_opentable %>% group_by(City, State, Country) %>% 
  gather(date, change, -City, -State, -Country) %>%
  mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>%
  filter(Country == mycountry) 

state_opentable_filtered = state_opentable %>% group_by(State, Country) %>% 
  gather(date, change, -State, -Country) %>%
  mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>%
  filter(Country == mycountry) 

if ( mycountry == "United States" ) {
  filter <- c("New York", "Washington", "California")
} else {
  filter <- c("Ontario", "Quebec")
}

simple_mean = state_opentable_filtered %>% group_by(date) %>% summarize(change = mean(change)) %>%
  mutate(State = "Average")
# get max for one of the labels
state_opentable_filtered %>% filter(State %in% filter[-1] ) %>% summarize(max = max(change)) -> max_state
state_opentable_filtered %>% 
  filter(State %in% filter[-1] ) %>% 
  filter(change ==  max_state$max) -> max_state_label

max_state_label

state_labels = state_opentable_filtered %>% filter(State %in% filter[1:length(filter)-1] ) %>%
  filter(row_number() == n()) %>%
  bind_rows(max_state_label) %>%
  bind_rows(simple_mean %>% filter(row_number() == n())) 

ggplot() + 
  scale_y_continuous(labels = scales::percent, 
                     limits = c(-.50,0.50)) +
  geom_line(data = state_opentable_filtered, 
            aes(y = change, x = date,
            color=as.factor(State)), alpha = 0.2, show=FALSE) +
  geom_line(data = state_opentable_filtered %>% filter(State %in% filter), 
            aes(y = change, x = date,
            color=as.factor(State)), show=FALSE) +
  geom_line(data = simple_mean, aes(y = change, x = date))+
  geom_text(data = state_labels, aes(y = change, x = date, label = State), nudge_x = 1.5) +
  theme_classic() +
  labs(x = "Date",
           y = "",
           title="Restaurant reservations from Opentable",
           subtitle = "Year-on-year change in diners",
           note = "Includes phone, online, and walk-in diners. All breakouts have 50 or more restaurants in the sample set."
           ) +
  geom_hline(yintercept = 0, linetype=2)

