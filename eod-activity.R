library(tidyverse)
library(here)
library(janitor)

meterological_data <- read_csv(here("data", "ntl20_v6.csv"))%>%
  clean_names()

ice_cover <- read_csv(here("data", "ntl33_v7.csv"), na = c("-999", ""))%>%
  clean_names()

ice_duration_lakes <- ice_cover %>%
  group_by(lakeid, year4) %>%
  summarize(mean_ice = mean(ice_duration, na.rm = TRUE))

ggplot(data = ice_duration_lakes, aes (x = year4, y = mean_ice, color = lakeid))+
  geom_jitter()


ice_duration_lakes <- ice_cover %>%
  group_by(year4) %>%
  summarize(mean_ice = mean(ice_duration, na.rm = TRUE))

ggplot(data = ice_duration_lakes, aes (x = year4, y = mean_ice))+
  geom_jitter() +
  labs(x = "Year", 
       y = "Mean Ice Cover Duration (days)",
       title = "Madison Lake Area Ice Cover v Time",
       caption = "As time continues, the average ice cover duration in Madison Lake Area decreases")


mean_air <- meterological_data %>%
  filter(month %in% c(12,1,2)) %>%
  group_by(year4)%>%
  summarize(mean_air = mean(ave_air_temp_adjusted, na.rm = TRUE))

ggplot(data= mean_air, aes(x = year4, y = mean_air))+
  geom_jitter()+
  labs(x = "Year", y = "Average Daily Air Temperature",
                    title = "Average Daily Air Temperature over time in Winter Months",
                    caption = "As time continues, the average daily air temperature generally increases")


join_temp_ice <- full_join(mean_air, ice_duration_lakes)

ggplot(data = join_temp_ice, aes(x = mean_air, y = mean_ice))+
  geom_point()+
  labs(x = "Mean Temperature (Celcius)",
       y = "Mean Ice Cover Duration (days)",
       caption = "As mean temperature decreases, the number of days with ice cover increases")


thaw_1970_on <- ice_cover %>%
  filter(ice_off >= "1970-1-1")%>%
  mutate(ice_off = lubridate::yday(ice_off))%>%
  mutate(day_of_year = lubridate::yday(ice_off))
  
ggplot(data = thaw_1970_on, aes(x = year4, y = ice_off))
