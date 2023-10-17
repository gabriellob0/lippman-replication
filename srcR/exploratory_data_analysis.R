library(haven)
library(dplyr)
library(tidyr)

gsoep_raw <- read_dta("data//raw//gsoep.dta")

household_sizes <- gsoep_raw |>
  summarise(count = n(), .by = c(cpf_hid, wave)) |>
  arrange(desc(count))

#EDA ----
#TODO: there are negative hours worked and things like that,
#should probably cut it out.

#main explanatory variables ----
two_person_hhs <- gsoep_raw |>
  group_by(cpf_hid, wave) |>
  filter(n() == 2) |>
  mutate(marriage_role = if_else(female == 1, "wife", "husband"))

gay_couples <- two_person_hhs |>
  select(cpf_hid, wave, incjob1_mg, marriage_role) |>
  group_by(cpf_hid, wave, marriage_role) |>
  summarise(n = dplyr::n(), .groups = "drop") |>
  filter(n > 1L)

#there are two income measures here, made an arbitrary choice
hh_salary <- two_person_hhs |>
  select(cpf_hid, wave, incjob1_mg, marriage_role) |>
  filter(!(cpf_hid %in% gay_couples$cpf_hid)) |>
  pivot_wider(names_from = marriage_role, values_from = incjob1_mg) |>
  mutate(wife_earns_more = if_else(wife > husband, 1, 0)) |>
  select(!c(husband, wife))

#in loc89, 1 is east, 2 is west
both_from_east <- two_person_hhs |>
  select(cpf_hid, wave, loc89, marriage_role) |>
  filter(!(cpf_hid %in% gay_couples$cpf_hid)) |>
  pivot_wider(names_from = marriage_role, values_from = loc89) |>
  mutate(east = if_else(husband == 1 & wife == 1, 1, 0)) |>
  select(!c(husband, wife))

main_df <- gsoep_raw |>
  filter(cpf_hid %in% two_person_hhs$cpf_hid,
         !(cpf_hid %in% gay_couples)) |>
  left_join(hh_salary, by = join_by(cpf_hid, wave)) |>
  left_join(both_from_east, by = join_by(cpf_hid, wave))

#main outcome variables ----

#descriptive statistics ----
figure2 <- main_df |>
  #this seems to be already excluded
  filter(age >= 18 & age <= 65) |>
  
