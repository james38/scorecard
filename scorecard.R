library('tidyverse')
library('magrittr')
library('lubridate')
rm(list=ls())

# data import and tidying ####
# GDP
countries_gdp <- read_csv(
  "gdp.csv"
)

countries_gdp %<>%
  gather(
    key = "Year",
    value = "GDP",
    `2000`:`2018`
  ) %>% 
  select(
    -`Pop Rank`
  )

countries_pop <- read_csv(
  "countries_pop.csv"
)

countries_pop %<>%
  gather(
    key = "Year",
    value = "People",
    `2000`:`2019`
  )

# Econ Freedom
countries_econ_freedom <- read_csv(
  "countries_econ_freedom.csv"
)

# Years for Econ Freedom?? 2001-2018 or 2002-2019
countries_econ_freedom %<>% 
  gather(
    key = "Year",
    value = "econ_freedom",
    `2002`:`2019`
  )

countries_press_freedom <- read_csv(
  "countries_press_freedom.csv"
)

countries_press_freedom <- countries_press_freedom %>% 
  gather(
    key = "Year",
    value = "press_freedom",
    `2000`:`2017`
  )

countries_pol_rights <- read_csv(
  "countries_pol_rights.csv"
)
countries_pol_rights <- countries_pol_rights %>% 
  gather(
    key = "Year",
    value = "pol_rights",
    `2000`:`2016`
  )

countries_civil_liberty <- read_csv(
  "countries_civil_liberty.csv"
)
countries_civil_liberty <- countries_civil_liberty %>% 
  gather(
    key = "Year",
    value = "civil_liberty",
    `2000`:`2016`
  )

# Changed 2013-14 to 2013
countries_internet_freedom <- read_csv(
  "countries_internet_freedom.csv"
) %>% 
  select(
    -`Pop Rank`
  )
countries_internet_freedom <- countries_internet_freedom %>% 
  gather(
    key = "Year",
    value = "internet_freedom",
    `2009`:`2018`
  )

countries_corruption_percept <- read_csv(
  "countries_corruption_percept.csv"
) %>% 
  select(
    -`Pop Rank`
  )
# modified Sudan 2002 to be blank
countries_corruption_percept <- countries_corruption_percept %>% 
  gather(
    key = "Year",
    value = "corruption_percept",
    `2000`:`2019`
  ) %>% 
  mutate(
    Year = as.integer(Year),
    corruption_percept = case_when(
      Year > 2011 ~ corruption_percept/10,
      Year <= 2011 ~ corruption_percept
    ),
    Year = as.character(Year)
  )

countries_infant_mortality <- read_csv(
  "countries_infant_mortality.csv"
) %>% 
  select(
    -`Pop Rank`
  )

countries_infant_mortality <- countries_infant_mortality %>% 
  gather(
    key = "Year",
    value = "infant_mortality",
    `2000`:`2018`
  )

# blanks for Nigeria in 2000, 2001, 2002
countries_human_dev <- read_csv(
  "countries_human_dev.csv"
) %>% 
  select(
    -`Pop Rank`
  )
countries_human_dev <- countries_human_dev %>% 
  gather(
    key = "Year",
    value = "human_dev",
    `2000`:`2018`
  )
countries_gender_gap <- read_csv(
  "countries_gender_gap.csv"
) %>% 
  select(
    -`Pop Rank`
  )
countries_gender_gap <- countries_gender_gap %>% 
  gather(
    key = "Year",
    value = "gender_gap",
    `2006`:`2019`
  )
countries_giving <- read_csv(
  "countries_giving.csv"
)
countries_giving <- countries_giving %>% 
  gather(
    key = "Year",
    value = "giving",
    `2010`:`2018`
  )

# Peace changed from 2013-14 to 2013
countries_peace <- read_csv(
  "countries_peace.csv"
) %>% 
  select(
    -`Pop Rank`
  )
countries_peace <- countries_peace %>%
  gather(
    key = "Year",
    value = "peace",
    `2007`:`2019`
  )
countries_environmental <- read_csv(
  "countries_environmental.csv"
)
countries_environmental <- countries_environmental %>% 
  gather(
    key = "Year",
    value = "environmental",
    `2000`:`2018`
  )

countries_happiness <- read_csv(
  "happiness.csv"
) %>% 
  select(
    -`Pop Rank`
  )

countries_happiness <- countries_happiness %>% 
  gather(
    key = "Year",
    value = "happiness",
    `2006`:`2017`
  )

countries_poverty <- read_csv(
  "poverty.csv"
) %>% 
  select(
    -`Pop Rank`
  )

countries_poverty <- countries_poverty %>% 
  gather(
    key = "Year",
    value = "poverty",
    `2000`:`2016`
  )

countries_ecological <- read_csv(
  "ecological.csv"
) %>% 
  select(
    -`Pop Rank`
  )
# Start at 1999 or 2000?
countries_ecological <- countries_ecological %>% 
  gather(
    key = "Year",
    value = "ecological",
    `1999`:`2016`
  )

countries_gases <- read_csv(
  "gases.csv"
) %>% 
  select(
    -`Pop Rank`
  )

countries_gases <- countries_gases %>% 
  gather(
    key = "Year",
    value = "gases",
    `2000`:`2016`
  )

countries_fossil <- read_csv(
  "fossil_fuel.csv"
) %>% 
  select(
    -`Pop Rank`
  )

countries_fossil <- countries_fossil %>% 
  gather(
    key = "Year",
    value = "fossil_fuel",
    `2000`:`2015`
  )

countries_trees <- read_csv(
  "trees.csv"
) %>% 
  select(
    -`Pop Rank`
  )

countries_trees <- countries_trees %>% 
  gather(
    key = "Year",
    value = "trees",
    `2001`:`2018`
  )

# data wrangling ####

# join population with all metrics
countries_data <- countries_pop %>%
  left_join(
    countries_gdp
  ) %>% 
  left_join(
    countries_civil_liberty
  ) %>% 
  left_join(
    countries_corruption_percept
  ) %>% 
  left_join(
    countries_ecological
  ) %>% 
  left_join(
    countries_econ_freedom
  ) %>% 
  left_join(
    countries_environmental
  ) %>% 
  left_join(
    countries_fossil
  ) %>% 
  left_join(
    countries_gases
  ) %>% 
  left_join(
    countries_gender_gap
  ) %>% 
  left_join(
    countries_giving
  ) %>% 
  left_join(
    countries_happiness
  ) %>% 
  left_join(
    countries_human_dev
  ) %>% 
  left_join(
    countries_infant_mortality
  ) %>% 
  left_join(
    countries_internet_freedom
  ) %>% 
  left_join(
    countries_peace
  ) %>% 
  left_join(
    countries_pol_rights
  ) %>% 
  left_join(
    countries_poverty
  ) %>% 
  left_join(
    countries_press_freedom
  ) %>% 
  left_join(
    countries_trees
  )

# select specific metrics from all data
gdp_data <- countries_data %>%
  select(
    Country,
    Year,
    GDP,
    People
  )

civil_liberty_data <- countries_data %>%
  select(
    Country,
    Year,
    civil_liberty,
    People
  )

corruption_percept_data <- countries_data %>%
  select(
    Country,
    Year,
    corruption_percept,
    People
  )

econ_freedom_data <- countries_data %>%
  select(
    Country,
    Year,
    econ_freedom,
    People
  )

environmental_data <- countries_data %>%
  select(
    Country,
    Year,
    environmental,
    People
  )

gender_gap_data <- countries_data %>%
  select(
    Country,
    Year,
    gender_gap,
    People
  )

giving_data <- countries_data %>%
  select(
    Country,
    Year,
    giving,
    People
  )

human_dev_data <- countries_data %>%
  select(
    Country,
    Year,
    human_dev,
    People
  )

infant_mortality_data <- countries_data %>%
  select(
    Country,
    Year,
    infant_mortality,
    People
  )

internet_freedom_data <- countries_data %>%
  select(
    Country,
    Year,
    internet_freedom,
    People
  )

pol_rights_data <- countries_data %>%
  select(
    Country,
    Year,
    pol_rights,
    People
  )

press_freedom_data <- countries_data %>%
  select(
    Country,
    Year,
    press_freedom,
    People
  )

peace_data <- countries_data %>%
  select(
    Country,
    Year,
    peace,
    People
  )

# calculate year by year statistics
gdp_year_data <- gdp_data %>%
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
    sd_mn_gdp = sd_gdp/mn_gdp
  )

#gdp_lm <- lm(gdp_year_data$mn_gdp ~ gdp_year_data$Year)
#summary(gdp_lm)
#?lm()

civil_liberty_year_data <- civil_liberty_data %>%
  drop_na(
    civil_liberty,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_cl = weighted.mean(civil_liberty, People),
    sd_cl = sd(civil_liberty)
  ) %>% 
  mutate(
    sd_mn_cl = sd_cl/mn_cl
  )

corruption_percept_year_data <- corruption_percept_data %>%
  drop_na(
    corruption_percept,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_cp = weighted.mean(corruption_percept, People),
    sd_cp = sd(corruption_percept)
  ) %>% 
  mutate(
    sd_mn_cp = sd_cp/mn_cp
  )

econ_freedom_year_data <- econ_freedom_data %>%
  drop_na(
    econ_freedom,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_ef = weighted.mean(econ_freedom, People),
    sd_ef = sd(econ_freedom)
  ) %>% 
  mutate(
    sd_mn_ef = sd_ef/mn_ef
  )

environmental_year_data <- environmental_data %>%
  drop_na(
    environmental,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_e = weighted.mean(environmental, People),
    sd_e = sd(environmental)
  ) %>% 
  mutate(
    sd_mn_e = sd_e/mn_e
  )

gender_gap_year_data <- gender_gap_data %>%
  drop_na(
    gender_gap,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_gg = weighted.mean(gender_gap, People),
    sd_gg = sd(gender_gap)
  ) %>% 
  mutate(
    sd_mn_gg = sd_gg/mn_gg
  )

giving_year_data <- giving_data %>%
  drop_na(
    giving,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_g = weighted.mean(giving, People),
    sd_g = sd(giving)
  ) %>% 
  mutate(
    sd_mn_g = sd_g/mn_g
  )

human_dev_year_data <- human_dev_data %>%
  drop_na(
    human_dev,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_hd = weighted.mean(human_dev, People),
    sd_hd = sd(human_dev)
  ) %>% 
  mutate(
    sd_mn_hd = sd_hd/mn_hd
  )

infant_mortality_year_data <- infant_mortality_data %>%
  drop_na(
    infant_mortality,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_im = weighted.mean(infant_mortality, People),
    sd_im = sd(infant_mortality)
  ) %>% 
  mutate(
    sd_mn_im = sd_im/mn_im
  )

internet_freedom_year_data <- internet_freedom_data %>%
  drop_na(
    internet_freedom,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_if = weighted.mean(internet_freedom, People),
    sd_if = sd(internet_freedom)
  ) %>% 
  mutate(
    sd_mn_if = sd_if/mn_if
  )

peace_year_data <- peace_data %>%
  drop_na(
    peace,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_p = weighted.mean(peace, People),
    sd_p = sd(peace)
  ) %>% 
  mutate(
    sd_mn_p = sd_p/mn_p
  )

pol_rights_year_data <- pol_rights_data %>%
  drop_na(
    pol_rights,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_pr = weighted.mean(pol_rights, People),
    sd_pr = sd(pol_rights)
  ) %>% 
  mutate(
    sd_mn_pr = sd_pr/mn_pr
  )

press_freedom_year_data <- press_freedom_data %>%
  drop_na(
    press_freedom,
    People
  ) %>%
  group_by(
    Year
  ) %>%
  summarise(
    mn_pf = weighted.mean(press_freedom, People),
    sd_pf = sd(press_freedom)
  ) %>% 
  mutate(
    sd_mn_pf = sd_pf/mn_pf
  )

# cumulative weighted change
gdp_chg_years = (
  gdp_year_data$mn_gdp[length(gdp_year_data$mn_gdp)]
  / gdp_year_data$mn_gdp[1]
)

civil_liberty_chg_years = (
  civil_liberty_year_data$mn_cl[length(civil_liberty_year_data$mn_cl)]
  / civil_liberty_year_data$mn_cl[1]
)

corruption_percept_chg_years = (
  corruption_percept_year_data$mn_cp[length(corruption_percept_year_data$mn_cp)]
  / corruption_percept_year_data$mn_cp[1]
)

econ_freedom_chg_years = (
  econ_freedom_year_data$mn_ef[length(econ_freedom_year_data$mn_ef)]
  / econ_freedom_year_data$mn_ef[1]
)

environmental_chg_years = (
  environmental_year_data$mn_e[length(environmental_year_data$mn_e)]
  / environmental_year_data$mn_e[1]
)

gender_gap_chg_years = (
  gender_gap_year_data$mn_gg[length(gender_gap_year_data$mn_gg)]
  / gender_gap_year_data$mn_gg[1]
)

giving_chg_years = (
  giving_year_data$mn_g[length(giving_year_data$mn_g)]
  / giving_year_data$mn_g[1]
)

human_dev_chg_years = (
  human_dev_year_data$mn_hd[length(human_dev_year_data$mn_hd)]
  / human_dev_year_data$mn_hd[1]
)

infant_mortality_chg_years = (
  infant_mortality_year_data$mn_im[1]
  / infant_mortality_year_data$mn_im[length(infant_mortality_year_data$mn_im)]
)

internet_freedom_chg_years = (
  internet_freedom_year_data$mn_if[length(internet_freedom_year_data$mn_if)]
  / internet_freedom_year_data$mn_if[1]
)

peace_chg_years = (
  peace_year_data$mn_p[length(peace_year_data$mn_p)]
  / peace_year_data$mn_p[1]
)

pol_rights_chg_years = (
  pol_rights_year_data$mn_pr[length(pol_rights_year_data$mn_pr)]
  / pol_rights_year_data$mn_pr[1]
)

press_freedom_chg_years = (
  press_freedom_year_data$mn_pf[length(press_freedom_year_data$mn_pf)]
  / press_freedom_year_data$mn_pf[1]
)

# weighted ratio of standard deviation to mean
wtd_sd_mn_gdp = weighted.mean(gdp_year_data$sd_mn_gdp, gdp_year_data$mn_gdp)
gdp_line_alpha = min((1/wtd_sd_mn_gdp), 0.75)

gdp_data %<>%
  drop_na(
    GDP
  ) %>%
  left_join(
    gdp_year_data
  )

wtd_sd_mn_cl = weighted.mean(
  civil_liberty_year_data$sd_mn_cl,
  civil_liberty_year_data$mn_cl
)
civil_liberty_line_alpha = min((1/wtd_sd_mn_cl), 0.75)

civil_liberty_data %<>%
  drop_na(
    civil_liberty
  ) %>%
  left_join(
    civil_liberty_year_data
  )

wtd_sd_mn_cp = weighted.mean(
  corruption_percept_year_data$sd_mn_cp,
  corruption_percept_year_data$mn_cp
)
corruption_percept_line_alpha = min((1/wtd_sd_mn_cp), 0.75)

corruption_percept_data %<>%
  drop_na(
    corruption_percept
  ) %>%
  left_join(
    corruption_percept_year_data
  )

wtd_sd_mn_ef = weighted.mean(
  econ_freedom_year_data$sd_mn_ef,
  econ_freedom_year_data$mn_ef
)
econ_freedom_line_alpha = min((1/wtd_sd_mn_ef), 0.75)

econ_freedom_data %<>%
  drop_na(
    econ_freedom
  ) %>%
  left_join(
    econ_freedom_year_data
  )

wtd_sd_mn_e = weighted.mean(
  environmental_year_data$sd_mn_e,
  environmental_year_data$mn_e
)
environmental_line_alpha = min((1/wtd_sd_mn_e), 0.75)

environmental_data %<>%
  drop_na(
    environmental
  ) %>%
  left_join(
    environmental_year_data
  )

wtd_sd_mn_gg = weighted.mean(
  gender_gap_year_data$sd_mn_gg,
  gender_gap_year_data$mn_gg
)
gender_gap_line_alpha = min((1/wtd_sd_mn_gg), 0.75)

gender_gap_data %<>%
  drop_na(
    gender_gap
  ) %>%
  left_join(
    gender_gap_year_data
  )

wtd_sd_mn_g = weighted.mean(
  giving_year_data$sd_mn_g,
  giving_year_data$mn_g
)
giving_line_alpha = min((1/wtd_sd_mn_g), 0.75)

giving_data %<>%
  drop_na(
    giving
  ) %>%
  left_join(
    giving_year_data
  )

wtd_sd_mn_hd = weighted.mean(
  human_dev_year_data$sd_mn_hd,
  human_dev_year_data$mn_hd
)
human_dev_line_alpha = min((1/wtd_sd_mn_hd), 0.75)

human_dev_data %<>%
  drop_na(
    human_dev
  ) %>%
  left_join(
    human_dev_year_data
  )

wtd_sd_mn_im = weighted.mean(
  infant_mortality_year_data$sd_mn_im,
  infant_mortality_year_data$mn_im
)
infant_mortality_line_alpha = min((1/wtd_sd_mn_im), 0.75)

infant_mortality_data %<>%
  drop_na(
    infant_mortality
  ) %>%
  left_join(
    infant_mortality_year_data
  )

wtd_sd_mn_if = weighted.mean(
  internet_freedom_year_data$sd_mn_if,
  internet_freedom_year_data$mn_if
)
internet_freedom_line_alpha = min((1/wtd_sd_mn_if), 0.75)

internet_freedom_data %<>%
  drop_na(
    internet_freedom
  ) %>%
  left_join(
    internet_freedom_year_data
  )

wtd_sd_mn_p = weighted.mean(
  peace_year_data$sd_mn_p,
  peace_year_data$mn_p
)
peace_line_alpha = min((1/wtd_sd_mn_p), 0.75)

peace_data %<>%
  drop_na(
    peace
  ) %>%
  left_join(
    peace_year_data
  )

wtd_sd_mn_pr = weighted.mean(
  pol_rights_year_data$sd_mn_pr,
  pol_rights_year_data$mn_pr
)
pol_rights_line_alpha = min((1/wtd_sd_mn_pr), 0.75)

pol_rights_data %<>%
  drop_na(
    pol_rights
  ) %>%
  left_join(
    pol_rights_year_data
  )

wtd_sd_mn_pf = weighted.mean(
  press_freedom_year_data$sd_mn_pf,
  press_freedom_year_data$mn_pf
)
press_freedom_line_alpha = min((1/wtd_sd_mn_pf), 0.75)

press_freedom_data %<>%
  drop_na(
    press_freedom
  ) %>%
  left_join(
    press_freedom_year_data
  )

# calculate country based statistics
gdp_year_range <- gdp_data %>%
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
    y=countries_gdp
  )

gdp_rates <- gdp_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = GDP[endpt=='last_year']/GDP[endpt=='first_year']
  ) %>% 
  mutate(
    rel_progress = progress/gdp_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    gdp_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

gdp_data %<>%
  left_join(
    gdp_rates
  )

countries_gdp_inc <- gdp_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_gdp_dec <- gdp_data %>% 
  filter(
    is_rel_progress == FALSE
  )

gdp_tot_pop_ct <- gdp_data %>% 
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

gdp_total_pop_ct <- gdp_tot_pop_ct$avg_pop

cl_year_range <- civil_liberty_data %>%
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
    y=countries_civil_liberty
  )

cl_rates <- cl_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = civil_liberty[endpt=='last_year']/civil_liberty[endpt=='first_year']
  ) %>% 
  mutate(
    rel_progress = progress/civil_liberty_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    cl_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

civil_liberty_data %<>%
  left_join(
    cl_rates
  )

countries_cl_inc <- civil_liberty_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_cl_dec <- civil_liberty_data %>% 
  filter(
    is_rel_progress == FALSE
  )

cl_tot_pop_ct <- civil_liberty_data %>% 
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

cl_total_pop_ct <- cl_tot_pop_ct$avg_pop


cp_year_range <- corruption_percept_data %>%
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
    y=countries_corruption_percept
  )

cp_rates <- cp_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = (
      corruption_percept[endpt=='last_year']
        /corruption_percept[endpt=='first_year']
    )
  ) %>% 
  mutate(
    rel_progress = progress/corruption_percept_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    cp_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

corruption_percept_data %<>%
  left_join(
    cp_rates
  )

countries_cp_inc <- corruption_percept_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_cp_dec <- corruption_percept_data %>% 
  filter(
    is_rel_progress == FALSE
  )

cp_tot_pop_ct <- corruption_percept_data %>% 
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

cp_total_pop_ct <- cp_tot_pop_ct$avg_pop


ef_year_range <- econ_freedom_data %>%
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
    y=countries_econ_freedom
  )

ef_rates <- ef_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = (
      econ_freedom[endpt=='last_year']
      /econ_freedom[endpt=='first_year']
    )
  ) %>% 
  mutate(
    rel_progress = progress/econ_freedom_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    ef_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

econ_freedom_data %<>%
  left_join(
    ef_rates
  )

countries_ef_inc <- econ_freedom_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_ef_dec <- econ_freedom_data %>% 
  filter(
    is_rel_progress == FALSE
  )

ef_tot_pop_ct <- econ_freedom_data %>% 
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

ef_total_pop_ct <- ef_tot_pop_ct$avg_pop


e_year_range <- environmental_data %>%
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
    y=countries_environmental
  )

e_rates <- e_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = (
      environmental[endpt=='last_year']
      /environmental[endpt=='first_year']
    )
  ) %>% 
  mutate(
    rel_progress = progress/environmental_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    e_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

environmental_data %<>%
  left_join(
    e_rates
  )

countries_e_inc <- environmental_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_e_dec <- environmental_data %>% 
  filter(
    is_rel_progress == FALSE
  )

e_tot_pop_ct <- environmental_data %>% 
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

e_total_pop_ct <- e_tot_pop_ct$avg_pop


gg_year_range <- gender_gap_data %>%
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
    y=countries_gender_gap
  )

gg_rates <- gg_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = (
      gender_gap[endpt=='last_year']
      /gender_gap[endpt=='first_year']
    )
  ) %>% 
  mutate(
    rel_progress = progress/gender_gap_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    gg_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

gender_gap_data %<>%
  left_join(
    gg_rates
  )

countries_gg_inc <- gender_gap_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_gg_dec <- gender_gap_data %>% 
  filter(
    is_rel_progress == FALSE
  )

gg_tot_pop_ct <- gender_gap_data %>% 
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

gg_total_pop_ct <- gg_tot_pop_ct$avg_pop


g_year_range <- giving_data %>%
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
    y=countries_giving
  )

g_rates <- g_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = (
      giving[endpt=='last_year']
      /giving[endpt=='first_year']
    )
  ) %>% 
  mutate(
    rel_progress = progress/giving_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    g_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

giving_data %<>%
  left_join(
    g_rates
  )

countries_g_inc <- giving_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_g_dec <- giving_data %>% 
  filter(
    is_rel_progress == FALSE
  )

g_tot_pop_ct <- giving_data %>% 
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

g_total_pop_ct <- g_tot_pop_ct$avg_pop


hd_year_range <- human_dev_data %>%
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
    y=countries_human_dev
  )

hd_rates <- hd_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = (
      human_dev[endpt=='last_year']
      /human_dev[endpt=='first_year']
    )
  ) %>% 
  mutate(
    rel_progress = progress/human_dev_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    hd_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

human_dev_data %<>%
  left_join(
    hd_rates
  )

countries_hd_inc <- human_dev_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_hd_dec <- human_dev_data %>% 
  filter(
    is_rel_progress == FALSE
  )

hd_tot_pop_ct <- human_dev_data %>% 
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

hd_total_pop_ct <- hd_tot_pop_ct$avg_pop

# lower numbers are better here
im_year_range <- infant_mortality_data %>%
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
    y=countries_infant_mortality
  )

im_rates <- im_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = (
      infant_mortality[endpt=='first_year'] #different than others
      /infant_mortality[endpt=='last_year']
    )
  ) %>% 
  mutate(
    rel_progress = progress/infant_mortality_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    im_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

infant_mortality_data %<>%
  left_join(
    im_rates
  )

countries_im_inc <- infant_mortality_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_im_dec <- infant_mortality_data %>% 
  filter(
    is_rel_progress == FALSE
  )

im_tot_pop_ct <- infant_mortality_data %>% 
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

im_total_pop_ct <- im_tot_pop_ct$avg_pop


if_year_range <- internet_freedom_data %>%
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
    y=countries_internet_freedom
  )

if_rates <- if_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = (
      internet_freedom[endpt=='last_year']
      /internet_freedom[endpt=='first_year']
    )
  ) %>% 
  mutate(
    rel_progress = progress/internet_freedom_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    if_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

internet_freedom_data %<>%
  left_join(
    if_rates
  )

countries_if_inc <- internet_freedom_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_if_dec <- internet_freedom_data %>% 
  filter(
    is_rel_progress == FALSE
  )

if_tot_pop_ct <- internet_freedom_data %>% 
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

if_total_pop_ct <- if_tot_pop_ct$avg_pop


p_year_range <- peace_data %>%
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
    y=countries_peace
  )

p_rates <- p_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = (
      peace[endpt=='last_year']
      /peace[endpt=='first_year']
    )
  ) %>% 
  mutate(
    rel_progress = progress/peace_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    p_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

peace_data %<>%
  left_join(
    p_rates
  )

countries_p_inc <- peace_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_p_dec <- peace_data %>% 
  filter(
    is_rel_progress == FALSE
  )

p_tot_pop_ct <- peace_data %>% 
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

p_total_pop_ct <- p_tot_pop_ct$avg_pop


pr_year_range <- pol_rights_data %>%
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
    y=countries_pol_rights
  )

pr_rates <- pr_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = (
      pol_rights[endpt=='last_year']
      /pol_rights[endpt=='first_year']
    )
  ) %>% 
  mutate(
    rel_progress = progress/pol_rights_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    pr_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

pol_rights_data %<>%
  left_join(
    pr_rates
  )

countries_pr_inc <- pol_rights_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_pr_dec <- pol_rights_data %>% 
  filter(
    is_rel_progress == FALSE
  )

pr_tot_pop_ct <- pol_rights_data %>% 
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

pr_total_pop_ct <- pr_tot_pop_ct$avg_pop


pf_year_range <- press_freedom_data %>%
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
    y=countries_press_freedom
  )

pf_rates <- pf_year_range %>%
  group_by(
    Country
  ) %>% 
  summarise(
    progress = (
      press_freedom[endpt=='last_year']
      /press_freedom[endpt=='first_year']
    )
  ) %>% 
  mutate(
    rel_progress = progress/press_freedom_chg_years - 1,
    is_rel_progress = case_when(
      rel_progress>=0 ~ TRUE,
      rel_progress<0 ~ FALSE
    ),
    pf_alpha_var = (abs(rel_progress)/max(abs(rel_progress)))^(1/2)
  )

press_freedom_data %<>%
  left_join(
    pf_rates
  )

countries_pf_inc <- press_freedom_data %>% 
  filter(
    is_rel_progress == TRUE
  )

countries_pf_dec <- press_freedom_data %>% 
  filter(
    is_rel_progress == FALSE
  )

pf_tot_pop_ct <- press_freedom_data %>% 
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

pf_total_pop_ct <- pf_tot_pop_ct$avg_pop

# plot data ####
ggplot(
  
) +
  geom_jitter(
    data = countries_gdp_inc,
    mapping = aes(
      x = Year,
      y = log(GDP+1),
      size = People,
      color = 'red',
      alpha = gdp_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_gdp_dec,
    mapping = aes(
      x = Year,
      y = log(GDP+1),
      size = People,
      color = 'green',
      alpha = gdp_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = gdp_year_data,
    mapping = aes(
      x = Year,
      y = log(mn_gdp+1),
      group = 1,
      size = gdp_total_pop_ct
    ),
    color = 'blue',
    alpha = gdp_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_gdp.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_cl_inc,
    mapping = aes(
      x = Year,
      y = civil_liberty,
      size = People,
      color = 'red',
      alpha = cl_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_cl_dec,
    mapping = aes(
      x = Year,
      y = civil_liberty,
      size = People,
      color = 'green',
      alpha = cl_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = civil_liberty_year_data,
    mapping = aes(
      x = Year,
      y = mn_cl,
      group = 1,
      size = cl_total_pop_ct
    ),
    color = 'blue',
    alpha = civil_liberty_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_cl.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_cp_inc,
    mapping = aes(
      x = Year,
      y = corruption_percept,
      size = People,
      color = 'red',
      alpha = cp_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_cp_dec,
    mapping = aes(
      x = Year,
      y = corruption_percept,
      size = People,
      color = 'green',
      alpha = cp_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = corruption_percept_year_data,
    mapping = aes(
      x = Year,
      y = mn_cp,
      group = 1,
      size = cp_total_pop_ct
    ),
    color = 'blue',
    alpha = corruption_percept_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_cp.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_e_inc,
    mapping = aes(
      x = Year,
      y = environmental,
      size = People,
      color = 'red',
      alpha = e_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_e_dec,
    mapping = aes(
      x = Year,
      y = environmental,
      size = People,
      color = 'green',
      alpha = e_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = environmental_year_data,
    mapping = aes(
      x = Year,
      y = mn_e,
      group = 1,
      size = e_total_pop_ct
    ),
    color = 'blue',
    alpha = environmental_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_e.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_ef_inc,
    mapping = aes(
      x = Year,
      y = econ_freedom,
      size = People,
      color = 'red',
      alpha = ef_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_ef_dec,
    mapping = aes(
      x = Year,
      y = econ_freedom,
      size = People,
      color = 'green',
      alpha = ef_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = econ_freedom_year_data,
    mapping = aes(
      x = Year,
      y = mn_ef,
      group = 1,
      size = ef_total_pop_ct
    ),
    color = 'blue',
    alpha = econ_freedom_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_ef.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_g_inc,
    mapping = aes(
      x = Year,
      y = giving,
      size = People,
      color = 'red',
      alpha = g_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_g_dec,
    mapping = aes(
      x = Year,
      y = giving,
      size = People,
      color = 'green',
      alpha = g_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = giving_year_data,
    mapping = aes(
      x = Year,
      y = mn_g,
      group = 1,
      size = g_total_pop_ct
    ),
    color = 'blue',
    alpha = giving_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_g.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_gg_inc,
    mapping = aes(
      x = Year,
      y = gender_gap,
      size = People,
      color = 'red',
      alpha = gg_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_gg_dec,
    mapping = aes(
      x = Year,
      y = gender_gap,
      size = People,
      color = 'green',
      alpha = gg_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = gender_gap_year_data,
    mapping = aes(
      x = Year,
      y = mn_gg,
      group = 1,
      size = gg_total_pop_ct
    ),
    color = 'blue',
    alpha = gender_gap_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_gg.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_hd_inc,
    mapping = aes(
      x = Year,
      y = human_dev,
      size = People,
      color = 'red',
      alpha = hd_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_hd_dec,
    mapping = aes(
      x = Year,
      y = human_dev,
      size = People,
      color = 'green',
      alpha = hd_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = human_dev_year_data,
    mapping = aes(
      x = Year,
      y = mn_hd,
      group = 1,
      size = hd_total_pop_ct
    ),
    color = 'blue',
    alpha = human_dev_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_hd.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_if_inc,
    mapping = aes(
      x = Year,
      y = internet_freedom,
      size = People,
      color = 'red',
      alpha = if_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_if_dec,
    mapping = aes(
      x = Year,
      y = internet_freedom,
      size = People,
      color = 'green',
      alpha = if_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = internet_freedom_year_data,
    mapping = aes(
      x = Year,
      y = mn_if,
      group = 1,
      size = if_total_pop_ct
    ),
    color = 'blue',
    alpha = internet_freedom_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_if.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_im_inc,
    mapping = aes(
      x = Year,
      y = infant_mortality,
      size = People,
      color = 'red',
      alpha = im_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_im_dec,
    mapping = aes(
      x = Year,
      y = infant_mortality,
      size = People,
      color = 'green',
      alpha = im_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = infant_mortality_year_data,
    mapping = aes(
      x = Year,
      y = mn_im,
      group = 1,
      size = im_total_pop_ct
    ),
    color = 'blue',
    alpha = infant_mortality_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_im.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_p_inc,
    mapping = aes(
      x = Year,
      y = peace,
      size = People,
      color = 'red',
      alpha = p_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_p_dec,
    mapping = aes(
      x = Year,
      y = peace,
      size = People,
      color = 'green',
      alpha = p_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = peace_year_data,
    mapping = aes(
      x = Year,
      y = mn_p,
      group = 1,
      size = p_total_pop_ct
    ),
    color = 'blue',
    alpha = peace_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_p.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_pf_inc,
    mapping = aes(
      x = Year,
      y = press_freedom,
      size = People,
      color = 'red',
      alpha = pf_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_pf_dec,
    mapping = aes(
      x = Year,
      y = press_freedom,
      size = People,
      color = 'green',
      alpha = pf_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = press_freedom_year_data,
    mapping = aes(
      x = Year,
      y = mn_pf,
      group = 1,
      size = pf_total_pop_ct
    ),
    color = 'blue',
    alpha = press_freedom_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_pf.png')

ggplot(
  
) +
  geom_jitter(
    data = countries_pr_inc,
    mapping = aes(
      x = Year,
      y = pol_rights,
      size = People,
      color = 'red',
      alpha = pr_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_jitter(
    data = countries_pr_dec,
    mapping = aes(
      x = Year,
      y = pol_rights,
      size = People,
      color = 'green',
      alpha = pr_alpha_var
    ),
    show.legend = FALSE
  ) +
  geom_line(
    data = pol_rights_year_data,
    mapping = aes(
      x = Year,
      y = mn_pr,
      group = 1,
      size = pr_total_pop_ct
    ),
    color = 'blue',
    alpha = pol_rights_line_alpha,
    show.legend = FALSE
  ) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave('plot_pr.png')

