library('tidyverse')
library('magrittr')
library('lubridate')
rm(list=ls())

# data import and tidying ####
prepare_data <- function(filename, var_type, year_init, year_fin){
  data <- read_csv(
    filename
  ) %>% 
    gather(
      key = "Year",
      value = var_type,
      year_init:year_fin
    ) %>% 
    select(
      -`Pop Rank`
    )
}

people_df <- prepare_data('countries_pop.csv', 'People', 2000, 2017)
gdp_df <- prepare_data('countries_gdp.csv', 'GDP', 2000, 2017)
econ_freedom_df <- prepare_data('countries_econ_freedom.csv', 'econ_freedom', 2001, 2018)
pol_rights_df <- prepare_data('countries_pol_rights.csv', 'pol_rights', 2000, 2016)
gdp_df <- prepare_data('countries_gdp.csv', 'GDP', 2000, 2017)
econ_freedom_df <- prepare_data('countries_econ_freedom.csv', 'econ_freedom', 2001, 2018)
press_freedom_df <- prepare_data('countries_press_freedom.csv', 'press_freedom', 2000, 2017)
gdp_df <- prepare_data('countries_gdp.csv', 'GDP', 2000, 2017)
econ_freedom_df <- prepare_data('countries_econ_freedom.csv', 'econ_freedom', 2001, 2018)
press_freedom_df <- prepare_data('countries_press_freedom.csv', 'press_freedom', 2000, 2017)
gdp_df <- prepare_data('countries_gdp.csv', 'GDP', 2000, 2017)
econ_freedom_df <- prepare_data('countries_econ_freedom.csv', 'econ_freedom', 2001, 2018)
press_freedom_df <- prepare_data('countries_press_freedom.csv', 'press_freedom', 2000, 2017)
