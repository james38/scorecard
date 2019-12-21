rm(list=ls())
library('tidyverse')
library('magrittr')
library('lubridate')

# data import and tidying ####
countries_gdp <- read_csv(
  "countries_gdp.csv"
)

countries_gdp %<>%
  gather(
    key = "Year",
    value = "GDP",
    `2000`:`2017`
  )

countries_pop <- read_csv(
  "countries_pop.csv"
)

countries_pop %<>%
  gather(
    key = "Year",
    value = "People",
    `2000`:`2017`
  )

countries_gdp %<>%
  select(
    -`Pop Rank`
  )

# data wrangling ####

# join population with GDP
country_data <- countries_pop %>%
  left_join(
    countries_gdp
  )

# calculate year by year statistics
year_data <- country_data %>%
  drop_na(
    GDP,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_gdp = weighted.mean(GDP, People),
    sd_gdp = sd(GDP)
  ) %>% 
  mutate(
    sd_mn = sd_gdp/mn_gdp
  )

# cumulative weighted change in GDP
countries_chg = (
  year_data$mn_gdp[length(year_data$mn_gdp)]
  / year_data$mn_gdp[1]
)
# weighted ratio of standard deviation to mean
wtd_sd_mn = weighted.mean(year_data$sd_mn, year_data$mn_gdp)
line_alpha = min((1/wtd_sd_mn), 0.75)

country_data %<>%
  drop_na(
    GDP
  ) %>%
  left_join(
    year_data
  )

# calculate country based statistics
gdp_year_range <- country_data %>%
  group_by(
    Country
  ) %>%
  summarise(
    first_year = min(Year),
    last_year = max(Year)
  ) %>%
  gather(
    key = "endpt",
    value = "Year",
    first_year:last_year
  ) %>% 
  left_join(
    y=country_data
  )

gdp_rates <- gdp_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = GDP[endpt=='last_year']/GDP[endpt=='first_year']
  ) %>% 
  mutate(
    rel_progress = progress/countries_chg - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

country_data %<>%
  left_join(
    gdp_rates
  )

country_data_inc <- country_data %>% 
  filter(
    is_rel_progress == TRUE
  )

country_data_dec <- country_data %>% 
  filter(
    is_rel_progress == FALSE
  )

tot_pop_ct <- country_data %>% 
  group_by(
    Year
  ) %>% 
  summarise(
    tot_pop = sum(People)
  ) %>% 
  ungroup() %>% 
  summarise(
    avg_pop = mean(tot_pop)
  )

total_pop_ct <- tot_pop_ct$avg_pop

# plot data ####
ggplot(
  
) +
  geom_jitter(
    data = country_data_inc,
    mapping = aes(
      x = Year,
      y = GDP,
      size = People,
      color = 'red',
      alpha = alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = country_data_dec,
    mapping = aes(
      x = Year,
      y = GDP,
      size = People,
      color = 'green',
      alpha = alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = year_data,
    mapping = aes(
      x = Year,
      y = mn_gdp,
      group = 1,
      size = total_pop_ct
    ),
    color = 'blue',
    alpha = line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))

