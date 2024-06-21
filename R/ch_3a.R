#### Chapter 2 #################################################################

source("R/01_startup.R")
monthly <- qread("output/monthly.qs", nthreads = availableCores())
qload("output/ia.qsm", nthreads = availableCores())
GH <- qread("output/GH.qs")
CSD <- qread("output/CSD.qs")
CD <- qread("output/CD.qs")
CT <- qread("output/CT.qs")
DA <- qread("output/DA.qs")
province <- qread("output/province.qs")
water <- qread("output/water.qs")


# The impact of STRs on housing availability in Ontario -------------------

city_dwellings <- 
  CSD |> 
  st_drop_geometry() |> 
  mutate_city_type() |> 
  summarize(dwellings = sum(dwellings_CSD), .by = city_type)

FREH_total <- 
  monthly |> 
  filter(month >= yearmonth("2016-01-01")) |> 
  summarize(FREH = sum(FREH), FREH_3 = sum(FREH_3), 
            .by = c(city_type, month)) |> 
  bind_rows(monthly_ia |> 
              summarize(n = n(), .by = c(city_type, month)) |> 
              inner_join(coefs_type) |> 
              mutate(FREH = FREH * n, FREH_3 = FREH_3 * n) |> 
              select(city_type, month, FREH, FREH_3) |> 
              filter(month > yearmonth("2022-09")) |> 
              filter(city_type != "Toronto" | month >= yearmonth("2023-07")))

GH_total <-
  GH |> 
  select(ghost_ID, month, city_type, housing_units) |> 
  st_drop_geometry() |> 
  summarize(GH = sum(housing_units), .by = c(city_type, month)) |> 
  filter(city_type == "Toronto" | month <= yearmonth("2022-09")) |> 
  bind_rows(GH_ia |> 
              select(ghost_ID, month, city_type, housing_units) |> 
              st_drop_geometry() |> 
              summarize(GH = sum(housing_units), .by = c(city_type, month)) |> 
              filter(month > yearmonth("2022-09")) |> 
              filter(city_type != "Toronto" | month >= yearmonth("2023-07")))

housing_loss <-
  FREH_total |> 
  left_join(GH_total, by = c("city_type", "month")) |> 
  mutate(GH = coalesce(GH, 0))  

housing_loss_seasonal <-
  FREH_total |> 
  tsibble::as_tsibble(key = city_type, index = month) |> 
  model(STL(FREH, robust = TRUE)) |> 
  components() |> 
  as_tibble() |> 
  select(city_type, month, FREH = season_adjust) |> 
  left_join(GH_total, by = c("city_type", "month")) |> 
  mutate(GH = coalesce(GH, 0)) |> 
  rename(`Entire home/apt` = FREH, `Private room` = GH) |> 
  pivot_longer(c(`Entire home/apt`, `Private room`), 
               names_to = "type",
               values_to = "units") |> 
  mutate(type = factor(type, levels = c("Private room", "Entire home/apt")))  

(freh_2023 <-
  housing_loss_seasonal |> 
  filter(month == yearmonth("2023-12")) |> 
  filter(type == "Entire home/apt") |> 
  summarize(FREH = sum(units)) |> 
  pull(FREH) |> 
  scales::comma(10))

(gh_units_2023 <- 
  housing_loss_seasonal |> 
  filter(month == yearmonth("2023-12")) |> 
  filter(type == "Private room") |> 
  summarize(GH = sum(units)) |> 
  pull(GH) |> 
  scales::comma(10))

(housing_loss_2023 <- 
  housing_loss_seasonal |> 
  filter(month == yearmonth("2023-12")) |> 
  summarize(total = sum(units, na.rm = TRUE)) |> 
  pull() |> 
  sum() |> 
  scales::comma(10))

(housing_loss_to_region_2023 <- 
  housing_loss_seasonal |> 
  filter(month == yearmonth("2023-12"), city_type == "Other GTHA") |> 
  summarize(total = sum(units, na.rm = TRUE)) |> 
  pull() |> 
  sum() |> 
  scales::comma(10))

(housing_loss_to_region_2020 <- 
    housing_loss_seasonal |> 
    filter(month == yearmonth("2020-02"), city_type == "Other GTHA") |> 
    summarize(total = sum(units, na.rm = TRUE)) |> 
    pull() |> 
    sum() |> 
    scales::comma(10))

(housing_loss_to_region_change <-
  housing_loss_seasonal |> 
  filter(month %in% c(yearmonth("2020-02"), yearmonth("2023-12")), 
         city_type == "Other GTHA") |> 
  summarize(units = sum(units), .by = month) |> 
  summarize(change = (max(units) - min(units)) / min(units)) |> 
  pull() |> 
  sum() |> 
  scales::percent(0.1))


# Figure 2.1 --------------------------------------------------------------

fig_2_1 <-
  housing_loss_seasonal |>
  summarize(city_type = "Total", units = sum(units), .by = c(month, type)) |> 
  bind_rows(housing_loss_seasonal) |> 
  mutate_type() |> 
  filter(month <= yearmonth("2023-12")) |> 
  ggplot(aes(month, units, fill = type)) +
  geom_col(lwd = 0) +
  scale_fill_manual(name = "Listing type", values = col_palette[c(1, 3)]) +
  scale_x_yearmonth(name = NULL, limits = c(as.Date("2017-07-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  facet_wrap(~city_type, scales = "free_y", nrow = 2) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_2_1.png", fig_2_1, width = 10, height = 5)


# Table 3 -----------------------------------------------------------------

housing_loss_seasonal |> 
  summarize(loss_2023 = sum(units[month == yearmonth("2023-12")]),
            loss_2022 = sum(units[month == yearmonth("2022-12")]),
            .by = city_type) |> 
  inner_join(city_dwellings) |> 
  summarize(city_type = "Total", across(loss_2023:dwellings, sum)) |> 
  bind_rows(
    housing_loss_seasonal |> 
      summarize(loss_2023 = sum(units[month == yearmonth("2023-12")]),
                loss_2022 = sum(units[month == yearmonth("2022-12")]),
                .by = city_type) |> 
      inner_join(city_dwellings)) |> 
  mutate(yoy = (loss_2023 - loss_2022) / loss_2022,
         loss_dwelling = loss_2023 / dwellings) |> 
  mutate_type() |> 
  arrange(city_type) |> 
  mutate(across(loss_2023:loss_2022, \(x) scales::comma(x, 10)),
         yoy = scales::percent(yoy, 0.1),
         loss_dwelling = scales::percent(loss_dwelling, 0.1)) |> 
  select(-dwellings) |> 
  gt::gt()


# Housing loss model ------------------------------------------------------

# Get monthly housing loss
housing_loss_monthly_series <- 
  housing_loss_seasonal |>  
  summarize(units = sum(units, na.rm = TRUE), 
            .by = c(city_type, month)) |> 
  tsibble::as_tsibble(key = city_type, index = month)

# Create housing loss model
housing_loss_model <- 
  housing_loss_monthly_series |> 
  filter(month <= yearmonth("2020-02")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), RW(season_adjust ~ drift())))

# Create housing loss forecast
housing_loss_forecast <-
  housing_loss_model |> 
  forecast(h = "58 months") |> 
  as_tibble() |> 
  select(city_type, month, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series <- 
  housing_loss_monthly_series |>  
  full_join(housing_loss_forecast, by = c("city_type", "month"))

# Add decay to growth rate
housing_loss_monthly_decay <-
  housing_loss_monthly_series |> 
  mutate(decay = 0.97 ^ (as.numeric(month) - 602)) |> 
  group_by(city_type) |> 
  mutate(
    lag = slide_dbl(units_trend_month, \(x) x[2] - x[1], .before = 1),
    delta = lag * decay,
    delta = if_else(month > yearmonth("Mar 2020"), delta, 0),
    delta_cum = slide_dbl(delta, \(x) sum(x), .before = 100),
    units_trend_month = units_trend_month[month == yearmonth("Mar 2020")] + 
      delta_cum) |> 
  ungroup()

# Integrate forecast into monthly data
housing_loss_monthly_series <- 
  housing_loss_monthly_decay |> 
  select(city_type, month, units, units_trend = units_trend_month) |> 
  mutate(units_trend = if_else(month >= yearmonth("2020-03"), 
                               units_trend, NA_real_))

# Create housing loss forecast from post-Covid data
housing_loss_forecast_2 <-
  housing_loss_seasonal |>  
  summarize(units = sum(units, na.rm = TRUE), 
            .by = c(city_type, month)) |> 
  tsibble::as_tsibble(key = city_type, index = month) |> 
  filter(month >= yearmonth("2021-03"), month <= yearmonth("2023-12")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), RW(season_adjust ~ drift()))) |> 
  forecast(h = "12 months") |> 
  as_tibble() |> 
  select(city_type, month, post_covid_trend = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series <- 
  housing_loss_monthly_series |> 
  full_join(housing_loss_forecast_2, by = c("city_type", "month"))

# Apply seasonal smoothing to `units`
housing_loss_monthly_seasonal <- 
  housing_loss_monthly_series |> 
  filter(!is.na(units)) |> 
  tsibble::as_tsibble(key = city_type, index = month) |> 
  model(STL(units, robust = TRUE)) |> 
  components() |> 
  as_tibble() |> 
  select(city_type, month, units = season_adjust)

# Apply seasonal smoothing to `units_trend`
housing_loss_monthly_seasonal <- 
  housing_loss_monthly_series |> 
  mutate(units = coalesce(units_trend, units)) |> 
  tsibble::as_tsibble(key = city_type, index = month) |> 
  model(STL(units, robust = TRUE)) |> 
  components() |> 
  as_tibble() |> 
  select(city_type, month, units_trend = season_adjust) |> 
  mutate(units_trend = if_else(month < yearmonth("2020-03"), NA, 
                               units_trend)) |> 
  full_join(housing_loss_monthly_seasonal, by = c("city_type", "month")) |> 
  mutate(units_trend = if_else(month == yearmonth("2020-02"), units, 
                               units_trend))

# Apply seasonal smoothing to `post_covid_trend`
housing_loss_monthly_seasonal <- 
  housing_loss_monthly_series |> 
  mutate(units = coalesce(post_covid_trend, units)) |> 
  tsibble::as_tsibble(key = city_type, index = month) |> 
  model(STL(units, robust = TRUE)) |> 
  components() |> 
  as_tibble() |> 
  select(city_type, month, post_covid_trend = season_adjust) |> 
  mutate(post_covid_trend = if_else(month < yearmonth("2024-01"), NA, 
                                    post_covid_trend)) |> 
  full_join(housing_loss_monthly_seasonal, by = c("city_type", "month")) |> 
  mutate(post_covid_trend = if_else(month == yearmonth("2023-12"), units, 
                                    post_covid_trend))

# Quick facts
(housing_loss_cities_trend_2023 <- 
  housing_loss_monthly_seasonal |> 
  filter(city_type %in% c("Toronto", "Ottawa", "Kingston"),
         month == yearmonth("2023 Dec")) |> 
  pull(units_trend) |> 
  sum() |> 
  scales::comma(10))

(housing_loss_cities_2023 <-
    housing_loss_monthly_seasonal |> 
    filter(city_type %in% c("Toronto", "Ottawa", "Kingston"),
           month == yearmonth("2023 Dec")) |> 
    pull(units) |> 
    sum() |> 
    scales::comma(10))

(housing_loss_cities_dif_pct_2023 <- 
  housing_loss_monthly_seasonal |> 
  filter(city_type %in% c("Toronto", "Ottawa", "Kingston"),
         month == yearmonth("2023 Dec")) |> 
  summarize(dif = sum(units) / sum(units_trend)) |> 
  pull(dif) |> 
  scales::percent(0.1))

(housing_loss_trend_2023 <- 
  housing_loss_monthly_seasonal |> 
  as_tibble() |> 
  filter(month == yearmonth("2023 Dec")) |> 
  summarize(units = sum(units), units_trend = sum(units_trend),
            dif = (units_trend - units) / units))


# Figure 2.2 --------------------------------------------------------------

housing_loss_for_figure <- 
  housing_loss_monthly_seasonal |> 
  as_tibble() |> 
  filter(month <= yearmonth("2023-12")) |> 
  select(-post_covid_trend) |> 
  summarize(city_type = "Total", units_trend = sum(units_trend), 
            units = sum(units), .by = month) |> 
  bind_rows( housing_loss_monthly_seasonal |> 
               as_tibble() |> 
               filter(month <= yearmonth("2023-12")) |> 
               select(-post_covid_trend)) |> 
  mutate_type()
  
fig_2_2 <-
  housing_loss_for_figure |> 
  pivot_longer(-c(city_type, month)) |> 
  filter(!is.na(value)) |> 
  mutate(label = case_when(
    city_type == "Total" & month == yearmonth("2020-10") &
      name == "units" ~ "Actual housing loss",
    city_type == "Total" & month == yearmonth("2020-10") &
      name == "units_trend" ~ "Expected housing loss",
    .default = NA_character_)) |>
  ggplot() +
  geom_ribbon(aes(x = month, ymin = units, ymax = units_trend, group = 1),
              data = housing_loss_for_figure, 
              fill = col_palette[1], alpha = 0.3) +
  geom_line(aes(month, value, color = name), lwd = 0.5) +
  geom_label(aes(month, value, label = label, color = name),
             fill = alpha("white", 0.75), size = 3) +
  scale_x_yearmonth(name = NULL, limits = as.Date(c("2017-01-01", NA))) +
  scale_y_continuous(name = NULL, limits = c(0, NA), 
                     label = scales::comma) +
  scale_color_manual(name = NULL, 
                     labels = c("Actual STR housing loss", 
                                "Expected STR housing loss"), 
                     values = col_palette[c(3, 1)]) +
  facet_wrap(~city_type, scales = "free_y", nrow = 2) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_2_2.png", fig_2_2, width = 10, height = 5)
