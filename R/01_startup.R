#### 01 STARTUP ################################################################


# Optionally install packages from GitHub ---------------------------------

# remotes::install_github("UPGo-McGill/upgo")
# remotes::install_github("UPGo-McGill/strr")
# remotes::install_github("UPGo-McGill/matchr")


# Load packages -----------------------------------------------------------

library(tidyverse)
library(upgo)
library(strr)
library(sf)
library(mapview)
library(future)
library(progressr)
library(slider)
library(patchwork)
library(qs)
library(cancensus)
library(tsibble)
library(feasts)
library(fable)
plan(multisession)
handlers(global = TRUE)


# Set global variables ----------------------------------------------------

col_palette <- c("#d55e00", "#C800D5", "#0077D5", "#D5C900", "#0CD500", 
                 "#D5000D")

mutate_days <- function(x) {
  x |> 
    mutate(days = days_in_month(month(as.Date(month))))
}

mutate_type <- function(x) {
  if (sum(x$city_type == "Total") > 0) {
    x |> 
      mutate(city_type = factor(city_type, levels = c(
        "Total", "City of Toronto", "City of Ottawa", "Toronto region", 
        "Other urban", "Non-urban")))
  } else x |> 
    mutate(city_type = factor(city_type, levels = c(
      "City of Toronto", "City of Ottawa", "Toronto region", "Other urban", 
      "Non-urban")))
}

impute <- function(x) {
  x |> 
    inner_join(monthly_impute, by = c("id", "year", "rent", "universe")) |> 
    mutate(rent = coalesce(rent, rent_new), 
           universe = coalesce(universe, univ_new)) |> 
    select(-rent_new, -univ_new)
}
