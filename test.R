library(readr)
library(dplyr)
library(dtplyr)
library(forcats)
library(parsnip)
library(workflows)
library(recipes)

raw_df <- read_csv("data//raw//gsoep.csv")

subsample1 <- raw_df |>
  filter(
    whweek >= 0,
    if_all(c(incjob1_mg, incjob1_mn, edu4), ~. > 0),
    !if_all(errand:hobbies, is.na) #not sure if this should be here
  )

subsample2 <- subsample1 |>
  group_by(wave, cpf_hid) |>
  filter(n() == 2, length(unique(female)) == 2, length(unique(loc89)) == 1) |>
  mutate(total_inc = sum(incjob1_mn),
         partner_age = sum(age) - age,
         partner_educ = case_when(
           female == 1 ~ lag(edu4, order_by = female),
           female == 0 ~ lead(edu4, order_by = female)
         ),
         hwork_gap = if_else(female == 1, 2*hwork - sum(hwork), NA)
         ) |>
  ungroup()

subsample3 <- subsample2 |>
  mutate(
    partner_inc = total_inc - incjob1_mn,
    female_inc_share = case_when(
      female == 1 ~ incjob1_mn / total_inc,
      female == 0 ~ partner_inc / total_inc
    ),
    wife_earns_more = if_else(
      female_inc_share > 0.5, 1, 0
    ),
    east = if_else(loc89 == 1, 1, 0),
    kids = if_else(kidsn_hh17 != 0, 1, 0)
  )

generate_figure2 <- function(dat, loc) {
  
  figure2 <- dat |>
    filter(east == loc) |>
    pull(female_inc_share) |>
    density() |>
    plot()
  
  return(figure2)
}

subsample4 <- subsample3 |>
  mutate(across(c(pid, cpf_hid, wave, wavey, state, edu4, partner_educ), as_factor))

panel_A1 <- subsample4 |>
  filter(east == 0, female == 1) |>
  lm(
    hwork ~ wife_earns_more + female_inc_share +
      log(total_inc) + log(incjob1_mn) + log(partner_inc) +
      poly(age, 2) + poly(partner_age, 2) + kids +
      edu4 + partner_educ + wavey + state,
    data = _
  )


# tests ----


pooled_ols <- linear_reg() |>
  set_engine("lm")

lm_wflow <- workflow() |>
  add_model(pooled_ols) |>
  add_formula(hwork ~ wife_earns_more + female_inc_share)

fitted_pols <- fit(lm_wflow, subsample4)

