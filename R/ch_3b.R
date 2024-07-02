#### Chapter 3: The impact of STRs on housing availability and affordability ###
#### Part 2

source("R/01_startup.R")

DA_union <- qread("output/DA_union.qs", nthreads = availableCores())
province <- qread("output/province.qs", nthreads = availableCores())
water <- qread("output/water.qs", nthreads = availableCores())
dc <- qread("data/dc.qs", nthreads = availableCores())
mc <- qread("data/mc.qs", nthreads = availableCores())
qload("data/cmhc.qsm", nthreads = availableCores())
monthly_sept <- qread("data/monthly_sept.qs")

reg <-
  qread("data/reg.qs") |> 
  mutate(reg = if_else(reg == "TBD", FALSE, as.logical(reg))) |> 
  mutate(date = if_else(date >= "2023-01-02", NA, date)) |> 
  mutate(reg = if_else(is.na(date), FALSE, TRUE)) |> 
  inner_join(st_drop_geometry(cmhc_nbhd), by = c("id", "name")) |> 
  select(-c(pop:tenant))

source("R/06_imputation.R")


# Table 4: Regression model -----------------------------------------------

mc$common.1
dc$main |> filter(year >= 2018)

# Figure 3.1: Canada-wide rent change -------------------------------------

effect_yty <- bind_rows(
  model_change(2018),
  model_change(2019),
  model_change(2020),
  model_change(2021),
  model_change(2022))

fig_3_1 <-
  effect_yty |> 
  select(year, str_share, str_pct) |> 
  ggplot(aes(year, str_pct)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 5, colour = col_palette[1]) +
  geom_segment(aes(yend = 0), colour = col_palette[1], lwd = 2) +
  geom_label(
    aes(label = scales::dollar(str_share, 0.1, scale = 1/1000000, suffix = "M")),
    family = "Futura", nudge_y = c(0.007, 0.007, 0.007, 0.007, 0.007),
    colour = col_palette[1]) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL, labels = scales::percent) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"), legend.position = "none")

ggsave("output/figure_3_1.png", fig_3_1, width = 8, height = 4, units = "in")


# Rent calculations -------------------------------------------------------

# Get tenant counts
tenant_count <- 
  monthly_sept |> 
  st_drop_geometry() |> 
  # Impute missing values
  impute() |> 
  # Update tenant_count to reflect trend in universe
  mutate(tenant_count = universe * tenant_count[year == 2021] / 
           universe[year == 2021], .by = id) |> 
  select(id:province, rent, tenant_count)

# Get overall rent numbers
rent_change_prov <-
  tenant_count |> 
  group_by(year, province) |>
  mutate(total_rent = rent * tenant_count) |> 
  summarize(total_rent = sum(total_rent), .groups = "drop") |> 
  arrange(province, year) |>
  group_by(province) |>
  mutate(dif = slide_dbl(total_rent, \(x) x[2] - x[1], .before = 1)) |> 
  ungroup() |> 
  filter(province == "Ontario")

rent_cause_all_vars_on <- 
  map(2018:2022, \(x) {
  model_change(x, province,
               change_FREH = change_val("FREH", x, 1),
               change_non_FREH = change_val("non_FREH", x, 1),
               change_price = change_val("price", x, 1),
  )}) |> bind_rows() |> 
  filter(province == "Ontario")

rent_cause_FREH <- 
  map(2018:2022, \(x) {
    model_change(x, province,
                 change_FREH = change_val("FREH", x, 1),
                 change_non_FREH = change_val("non_FREH", x, 1),
                 change_price = change_val("price", x, 1),
                 use_non_FREH = FALSE,
                 use_price = FALSE,
    )}) |> bind_rows() |> 
  filter(province == "Ontario")

rent_cause_all_vars <- 
  map(2018:2022, \(x) {
    model_change(x, 
                 change_FREH = change_val("FREH", x, 1),
                 change_non_FREH = change_val("non_FREH", x, 1),
                 change_price = change_val("price", x, 1),
    )}) |> bind_rows()

# Total amount of extra rent paid from 2018-2022
rent_cause_all_vars_on |> 
  pull(str_share) |> 
  sum() |> 
  (\(x) x * 12)() |> 
  scales::dollar(accuracy = 0.1, scale = 1 / 1000000000, suffix = " billion")


# Figure 3.2: Share of rent caused by STR activity ------------------------

fig_3_2 <-
  rent_cause_all_vars_on |> 
  mutate(label = scales::dollar(str_share, accuracy = 0.1, 
                                scale = 1 / 1000000, suffix = "M")) |> 
  ggplot(aes(year, str_share)) +
  geom_col(fill = col_palette[1]) +
  geom_label(aes(y = str_share + 2000000, label = label), 
             colour = col_palette[1], family = "Futura") + 
  scale_y_continuous(name = NULL, labels = scales::dollar) +
  scale_x_continuous(name = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

ggsave("output/figure_3_2.png", fig_3_2, width = 8, height = 4, units = "in")


# Table of 2022 rent impacts ----------------------------------------------

reg_2022 <-
  reg |> 
  filter(reg, date <= "2022-07-01", province == "Ontario") |> 
  count(name_CSD) |> 
  mutate(has_reg = TRUE) |> 
  select(-n)

city_rent_table <-
  model_change(2022, id) |> 
  inner_join(select(reg, id, name_CSD, province)) |> 
  filter(province == "Ontario") |> 
  inner_join(tenant_count) |> 
  summarize(
    str_share = sum(str_share),
    total_change = sum(total_change),
    str_pct = str_share / total_change,
    tenant_count = sum(tenant_count),
    mean_str = str_share / tenant_count,
    mean_change = total_change / tenant_count,
    .by = name_CSD) |> 
  arrange(-total_change)

city_rent_table |> 
  arrange(-str_pct) |> 
  print(n = 20)

city_rent_table |> 
  summarize(
    name_CSD = "Ontario",
    str_share = sum(str_share),
    total_change = sum(total_change),
    str_pct = str_share / total_change,
    mean_str = str_share / sum(tenant_count),
    mean_change = total_change / sum(tenant_count)) |> 
  bind_rows(city_rent_table) |> 
  filter(name_CSD %in% c("Ontario", "Toronto", "Ottawa", "Hamilton", "Kingston",
                         "Niagara Falls", "London", "Kitchener", "Brampton",
                         "Mississauga", "Burlington", "Oshawa", 
                         "Prince Edward County")) |> 
  left_join(reg_2022) |> 
  select(name_CSD, str_share, str_pct, mean_str, has_reg) |> 
  mutate(str_share = scales::dollar(str_share, 0.1, scale = 1 / 1000, suffix = "K"),
         str_pct = scales::percent(str_pct, 0.1),
         mean_str = scales::dollar(mean_str, 0.1)) |> 
  gt::gt()

