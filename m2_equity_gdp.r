library(tidyverse)
library(fredr)

fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

gdp <- fredr(series_id = "GDP") %>% 
  select(date, gdp = value)
m2 <- fredr(series_id = "M2SL")          # M2 money supply
nasdaq <- fredr(series_id = "NASDAQCOM") %>%  # Wilshire 5000 index
  select(date, nasdaq = value)

m2_nasdaq <- m2 %>% 
  select(date, m2 = value) %>% 
  left_join(., nasdaq, by = "date") %>% 
  left_join(.,gdp, by = "date") %>% 
  na.omit() 

initial_data <- m2_nasdaq %>% 
  filter(date == min(m2_nasdaq$date))

initial_m2 <- initial_data$m2
initial_nasdaq <- initial_data$nasdaq
initial_gdp <- initial_data$gdp

m2_nasdaq %>% 
  mutate(m2_index = (m2/initial_m2) * 100,
         nasdaq_index = (nasdaq/initial_nasdaq) * 100,
         gdp_index = (gdp/initial_gdp) * 100) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = m2_index, colour = "M2")) +
  geom_line(aes(y = nasdaq_index, colour = "Nasdaq")) +
  geom_line(aes(y = gdp_index, colour = "GDP"))
  
  
