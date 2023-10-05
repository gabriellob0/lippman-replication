#imports ----
#source("srcR//00_install_packages.R")
library(dplyr)
library(tidyr)
library(haven)


gsoep_raw <- read_dta("data//raw//gsoep.dta")

#data cleaning ----
remove_negative_values <- function(dat) {
  #NAs do not seems to be an issue across important variables
  
  clean_dat <- dat |>
    filter(
      if_all(whweek:hobbies, ~. >= 0),
      edu4 > 0
    )
  
  return(clean_dat)
}

create_marriage_dummies <- function(dat) {
  
  dat_with_dummy <- dat |>
    #checks if married in given wave
    group_by(cpf_hid, wave) |>
    mutate(
      married = if_else(n() == 2, 1, 0),
      straight = if_else(length(unique(female)) == 2, 1, 0),
      east = if_else(all(loc89 == 1), 1, 0),
      mixed_origin = if_else(length(unique(loc89)) == 2, 1, 0),
      west = if_else(east == 0 & mixed_origin == 0, 1, 0)
    ) |>
    ungroup()
  
  return(dat_with_dummy)
}

generate_salary_metrics <- function(dat) {
  #there are two wage metrics here, so I made an arbitrary choice
  
  dat_with_dummy <- dat |>
    group_by(cpf_hid, wave) |>
    arrange(cpf_hid, wave, female) |>
    mutate(
      wife_earns_more = case_when(
        married == 1 & straight == 1 & (incjob1_mn < lead(incjob1_mn)) ~ 1,
        married == 1 & straight == 1 & (incjob1_mn >= lead(incjob1_mn)) ~ 0,
        TRUE ~ NA
      )
    ) |>
    fill(wife_earns_more)
  
  dat_with_share <- dat_with_dummy |>
    #I can also calculate the income share for each individual than just females
    #for simplicity
    mutate(
      female_income_share = case_when(
        married == 1 & straight == 1 & female == 1 ~
          (incjob1_mn / sum(incjob1_mn))
      ),
      income_share = case_when(
        married == 1 & straight == 1 ~
          (incjob1_mn / sum(incjob1_mn))
      )
    ) |>
    fill(female_income_share, .direction = "up") |>
    ungroup()

  return(dat_with_share)
}

gsoep_clean <- gsoep_raw |>
  remove_negative_values() |>
  create_marriage_dummies() |>
  generate_salary_metrics()

test_df <- gsoep_clean

#descriptive statistics ----
#FIGURE 2
test_df |>
  filter(
    !is.na(female_income_share),
    married == 1,
    straight == 1,
    west == 1,
    female == 1
  ) |>
  pull(female_income_share) |>
  density() |>
  plot()

test_df |>
  filter(
    !is.na(female_income_share),
    married == 1,
    straight == 1,
    east == 1,
    female == 1
  ) |>
  pull(female_income_share) |>
  density() |>
  plot()

#TABLE 2
#in the paper there are 6104 couples both from either west or east
#1976 from east and 4129 from west
test_df |>
  filter(married == 1, straight == 1, (east == 1 | west == 1)) |>
  mutate(couple_origin = if_else(east == 1, "east", "west")) |>
  distinct(cpf_hid, .keep_all = TRUE) |>
  group_by(couple_origin) |>
  summarise(n = n())

#34,205 obs, 22,091 west and 12,114 east
test_df |>
  filter(married == 1, female == 1, straight == 1, (east == 1 | west == 1)) |>
  mutate(couple_origin = if_else(east == 1, "east", "west")) |>
  group_by(couple_origin) |>
  summarise(n = n())

#data analysis ----

#TODO: the prep for the regression is largely unfinished.
prepare_for_regression <- function(dat) {
  
  dat_mutated <- dat |>
    group_by(cpf_hid, wave) |>
    mutate(
      hhd_inc = sum(incjob1_mn),
      lhdd_inc = log(hhd_inc),
      age2 = age ^ 2,
      children = if_else(kidsn_hh17 != 0, 1, 0),
      east_wife_interaction = east * wife_earns_more
    ) |>
    ungroup()
  
  filtered_dat <- dat_mutated |>
    filter(
      married == 1,
      (east == 1 | west == 1)
    )
  
  return(filtered_dat)
}

regression_df <- test_df |>
  prepare_for_regression() |>
  mutate(across(c(pid, cpf_hid, wavey, state), as.character)) |>
  filter(female == 1)

simple_specification <- lm(hwork ~ wife_earns_more + east_wife_interaction, data = regression_df)
simple_specification2 <- lm(hwork ~ wife_earns_more + wife_earns_more:east + age + I(age^2) + wavey, data = regression_df)

simple_specification
simple_specification2
