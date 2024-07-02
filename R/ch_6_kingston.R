#### Chapter 6: Kingston fact sheet ############################################

source("R/01_startup.R")
monthly <- qread("output/monthly.qs", nthreads = availableCores())
qload("output/ia.qsm", nthreads = availableCores())
GH <- qread("output/GH.qs")
CSD <- qread("output/CSD.qs")
CT <- qread("output/CT.qs")
DA <- qread("output/DA.qs")
province <- qread("output/province.qs")
water <- qread("output/water.qs")
DA_union <- qread("output/DA_union.qs", nthreads = availableCores())

dd <- qread("data/dd.qs", nthreads = availableCores())
md <- qread("data/md.qs", nthreads = availableCores())
dr <- qread("data/dr.qs", nthreads = availableCores())
dc <- qread("data/dc.qs", nthreads = availableCores())
mc <- qread("data/mc.qs", nthreads = availableCores())
qload("data/cmhc.qsm", nthreads = availableCores())
monthly_sept <- qread("data/monthly_sept.qs")

source("R/06_imputation.R")

reg <-
  qread("data/reg.qs") |> 
  mutate(reg = if_else(reg == "TBD", FALSE, as.logical(reg))) |> 
  mutate(date = if_else(date >= "2023-01-02", NA, date)) |> 
  mutate(reg = if_else(is.na(date), FALSE, TRUE)) |> 
  inner_join(st_drop_geometry(cmhc_nbhd), by = c("id", "name")) |> 
  select(-c(pop:tenant))

city_vec <- "Kingston"

  
# Active listings and revenue ---------------------------------------------

scraped_type <- 
  monthly |> 
  summarize(scraped = sum(scraped), .by = c(month, city_type)) |> 
  bind_rows(monthly_ia |>
              count(month, city_type) |> 
              left_join(coefs_type) |> 
              mutate(scraped = n * scraped) |>
              filter(month > yearmonth("2022-09")) |>
              filter(city_type != "Toronto" | month >= yearmonth("2023-07")) |> 
              select(month, city_type, scraped))

active_type <- 
  monthly |> 
  summarize(active = sum(R + A) / 30, .by = c(month, city_type)) |> 
  bind_rows(monthly_ia |>
              count(month, city_type) |> 
              left_join(coefs_type) |> 
              mutate(active = n * active) |>
              filter(month > yearmonth("2022-09")) |>
              filter(city_type != "Toronto" | month >= yearmonth("2023-07")) |> 
              select(month, city_type, active))

(scraped_dec_2023 <-
    scraped_type |> 
    filter(month == yearmonth("2023-12")) |> 
    filter(city_type %in% city_vec) |> 
    pull(scraped) |> 
    scales::comma(10))

(active_dec_2023 <-
    active_type |> 
    filter(month == yearmonth("2023-12")) |> 
    filter(city_type %in% city_vec) |> 
    pull(active) |> 
    scales::comma(10))

(active_2023_pct <- 
    active_type |> 
    filter(month == yearmonth("2023-12")) |> 
    filter(city_type %in% city_vec) |> 
    pull(active)) / (scraped_type |> 
                filter(month == yearmonth("2023-12")) |> 
                filter(city_type %in% city_vec) |> 
                pull(scraped))

(rev_july_2023 <-
    monthly_ia |> 
    filter(month == yearmonth("2023-07-01")) |> 
    count(city_type) |> 
    inner_join(coefs_type, by = "city_type") |> 
    mutate(rev = n * rev) |> 
    filter(city_type %in% city_vec) |> 
    pull(rev) |> 
    scales::dollar(0.1, scale = 1/1000000, suffix = " million"))

(rev_avg_july_2023 <-
    monthly_ia |> 
    filter(month == yearmonth("2023-07-01")) |> 
    count(city_type) |> 
    inner_join(coefs_type, by = "city_type") |> 
    mutate(active = active * n, rev = rev * n) |> 
    filter(city_type %in% city_vec) |> 
    summarize(rev_avg = sum(rev) / sum(active)) |> 
    pull(rev_avg) |> 
    scales::dollar(100))

# Kingston rent
(rent_2021 <- get_census("CA21", regions = list(CSD = "3510010"), 
                         vectors = c(rent = "v_CA21_4318")) |> 
    pull(rent))

# Covid change
scraped_type |> 
  filter(city_type %in% city_vec) |> 
  filter(month %in% c(yearmonth("2020-01"),
                      yearmonth("2021-06"))) |> 
  summarize(first = scraped[1], last = scraped[2], change = 1 - last/first)

active_type |> 
  filter(city_type %in% city_vec) |> 
  filter(month %in% c(yearmonth("2020-01"),
                      yearmonth("2021-06"))) |> 
  summarize(first = active[1], last = active[2], change = 1 - last/first)

# Recent growth
scraped_type |> 
  filter(city_type %in% city_vec) |> 
  filter(month %in% c(yearmonth("2022-01"),
                      yearmonth("2023-12"))) |> 
  summarize(first = scraped[1], last = scraped[2], change = last/first - 1)

active_type |> 
  filter(city_type %in% city_vec) |> 
  filter(month %in% c(yearmonth("2022-01"),
                      yearmonth("2023-12"))) |> 
  summarize(first = active[1], last = active[2], change = last/first - 1)


# Figure K1 ---------------------------------------------------------------

fig_K_1 <- 
  monthly_ia |> 
  filter(city == "Kingston") |> 
  filter(month == max(month)) |> 
  strr_as_sf(3347) |> 
  st_join(CT, y = _, left = FALSE) |> 
  summarize(n = n(), across(geometry, st_union), 
            .by = c(city_type, CTUID, dwellings)) |> 
  inner_join(coefs_type, by = "city_type") |> 
  mutate(active = active * n) |> 
  mutate(active_pct = active / dwellings) |> 
  ggplot() +
  geom_sf(data = DA, fill = "grey90", colour = "transparent") +
  geom_sf(data = filter(CSD, city == "Kingston"), fill = "grey",
          colour = "transparent") +
  geom_sf(aes(fill = active_pct), colour = "white", lwd = 0.05) +
  geom_sf(data = water, fill = "white", colour = "transparent") +
  scale_fill_stepsn(name = "STRs / dwelling", 
                    colours = col_palette[c(4, 6)],
                    breaks = c(0, 0.002, 0.004, 0.006, 0.008, 0.01), 
                    limits = c(0, 0.01), oob = scales::squish,
                    labels = scales::percent) +
  coord_sf(xlim = st_bbox(filter(CSD, city == "Kingston"))[c("xmin", "xmax")],
           ylim = st_bbox(filter(CSD, city == "Kingston"))[c("ymin", "ymax")]) +
  theme_void() +
  theme(plot.margin = unit(c(0, 10, 10, 0), "pt"), 
        legend.position = "bottom", legend.key.width = unit(30, "pt"), 
        text = element_text(family = "Futura"))

ggsave("output/figure_K_1.png", fig_K_1, width = 4, height = 4, units = "in")


# Figure K2 ---------------------------------------------------------------

fig_K_2 <-
  scraped_type |> 
  mutate(type = "Displayed") |> 
  rename(value = scraped) |> 
  bind_rows(
    active_type |> 
      mutate(type = "Active (est.)") |> 
      rename(value = active)) |> 
  filter(city_type %in% city_vec) |> 
  arrange(month, type) |> 
  filter(month >= yearmonth("2017-07"), month <= yearmonth("2023-12")) |>
  mutate(label = if_else(month == yearmonth("2019-05"),
                         type, NA)) |>
  mutate(value = slide_dbl(value, \(x) mean(x), .before = 1), 
         .by = c(type)) |> 
  ggplot() +
  geom_line(aes(month, value, colour = type, linetype = type), 
            linewidth = 1) +
  geom_label(aes(month, value, colour = type, label = label), 
             family = "Futura", size = 3) +
  scale_colour_manual(guide = "none", values = c(
    col_palette, col_palette[1:2])) +
  scale_y_continuous(name = NULL, labels = scales::comma) +
  scale_x_yearmonth(name = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"), legend.position = "none",
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figure_K_2.png", fig_K_2, width = 4, height = 4)


# Home sharers and commercial operators -----------------------------------

# EH %
(EH_pct_2023 <-
   monthly_ia |> 
   filter(month == yearmonth("2023-12")) |> 
   filter(city_type %in% city_vec) |> 
   summarize(EH = mean(listing_type == "Entire home/apt")) |> 
   pull(EH) |> 
   scales::percent(0.1))

# Multilistings
monthly_ia |> 
  filter(month == yearmonth("2023-12")) |> 
  filter(city_type %in% city_vec) |> 
  summarize(n = n(), pct = mean(multi))

monthly_for_rev <- 
  monthly |>  
  filter(!is.na(host_ID), month == yearmonth("2022-09"), R > 0) |> 
  summarize(rev = sum(rev), .by = c(city_type, host_ID)) |> 
  filter(city_type %in% city_vec)

(host_rev_data <- 
  monthly_for_rev |> 
  summarize(rev = sum(rev), .by = host_ID) |> 
  arrange(-rev) |> 
  summarize(
    pct_1 = quantile(rev, 0.99),
    pct_10 = quantile(rev, 0.9),
    pct_50 = quantile(rev, 0.5),
    n_1 = round(n() / 10),
    rev_1 = sum(rev[rev >= pct_1]) / sum(rev),
    rev_10 = sum(rev[rev >= pct_10]) / sum(rev)))


# STR-induced housing loss ------------------------------------------------

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
              filter(city_type != "Toronto" | month >= yearmonth("2023-07"))) |> 
  filter(city_type %in% city_vec)

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
              filter(city_type != "Toronto" | month >= yearmonth("2023-07"))) |> 
  filter(city_type %in% city_vec)

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

(housing_loss_2023 <- 
    housing_loss_seasonal |> 
    filter(month == yearmonth("2023-12")) |> 
    filter(city_type %in% city_vec) |> 
    summarize(total = sum(units, na.rm = TRUE)) |> 
    pull() |> 
    sum() |> 
    scales::comma(10))

(housing_loss_2020 <- 
    housing_loss_seasonal |> 
    filter(month == yearmonth("2020-01")) |> 
    filter(city_type %in% city_vec) |> 
    summarize(total = sum(units, na.rm = TRUE)) |> 
    pull() |> 
    scales::comma(10))

(housing_loss_2021 <- 
    housing_loss_seasonal |> 
    filter(month == yearmonth("2021-09")) |> 
    filter(city_type %in% city_vec) |> 
    summarize(total = sum(units, na.rm = TRUE)) |> 
    pull() |> 
    scales::comma(10))


# Figure K3 ---------------------------------------------------------------

fig_K_3 <-
  housing_loss_seasonal |>
  filter(month <= yearmonth("2023-12")) |> 
  filter(city_type %in% city_vec) |> 
  ggplot(aes(month, units, fill = type)) +
  geom_col(lwd = 0) +
  scale_fill_manual(name = "Listing type", values = col_palette[c(1, 3)]) +
  scale_x_yearmonth(name = NULL, limits = c(as.Date("2017-07-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_K_3.png", fig_K_3, width = 4, height = 4)


# The impact of STR activity on residential rents in Kingston -------------

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

rent_cause <- 
  map(2017:2022, \(x) {
    model_change(x, id,
                 change_FREH = change_val("FREH", x, 1),
                 change_non_FREH = change_val("non_FREH", x, 1),
                 change_price = change_val("price", x, 1))}) |> 
  bind_rows() |> 
  semi_join(filter(reg, name_CSD == "Kingston"))
  
rent_cause |> 
  pull(str_share) |> 
  sum() |> 
  (\(x) x * 12)() |> 
  scales::dollar(accuracy = 0.1, scale = 1 / 1000000, suffix = "M")

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
  filter(name_CSD == "Kingston")


# Better STR regulations would lower rents in Kingston --------------------

dyn <- aggte(md$main$rent_log, type = "dynamic")

did_effects <- 
  tibble(treat = md$main$rent_log$group,
         year = md$main$rent_log$t,
         att = md$main$rent_log$att) |> 
  filter(year >= treat)

did_rent_dif <-
  dd$main |> 
  mutate(id = as.character(id)) |> 
  # Add effects to data
  inner_join(did_effects, by = c("year", "treat")) |> 
  # Create counterfactual for rent_log in the absence of treatment
  mutate(rent_log_cf = rent_log - att) |> 
  # Un-standardize rent_log_cf
  mutate(rent_log_cf_raw = rent_log_cf * sd(dr$main$rent_log_raw) + 
           mean(dr$main$rent_log_raw)) |>
  # Exponentiate rent_log_cf_raw to get counterfactual rent
  mutate(rent_cf_raw = exp(rent_log_cf_raw)) |> 
  # Exponentiate rent_log_raw to get actual rent
  mutate(rent_raw = exp(rent_log_raw)) |>
  # Get rent difference
  mutate(rent_dif = rent_cf_raw - rent_raw) |> 
  select(id, year, treat, rent_raw, rent_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

did_rent_non_treated_y0 <-
  dr$main |> 
  anti_join(did_rent_dif, by = c("id")) |> 
  # Remove Georgina ON, because it removed its PR restriction
  filter(id != "2270780") |> 
  filter(year == 2022) |> 
  filter(province == "Ontario") |> 
  mutate(att = dyn$att.egt[dyn$egt == 0]) |> 
  mutate(rent_log_cf = rent_log - att) |> 
  mutate(rent_log_cf_raw = rent_log_cf * sd(dr$main$rent_log_raw) + 
           mean(dr$main$rent_log_raw)) |>
  mutate(rent_cf_raw = exp(rent_log_cf_raw)) |> 
  mutate(rent_raw = exp(rent_log_raw)) |> 
  mutate(rent_dif = rent_cf_raw - rent_raw) |> 
  select(id, year, rent_raw, rent_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

did_rent_non_treated_y1 <-
  dr$main |> 
  anti_join(did_rent_dif, by = c("id")) |> 
  # Remove Georgina ON, because it removed its PR restriction
  filter(id != "2270780") |> 
  filter(year == 2023) |> 
  filter(province == "Ontario") |> 
  mutate(att = dyn$att.egt[dyn$egt == 0]) |> 
  mutate(rent_log_cf = rent_log - att) |> 
  mutate(rent_log_cf_raw = rent_log_cf * sd(dr$main$rent_log_raw) + 
           mean(dr$main$rent_log_raw)) |>
  mutate(rent_cf_raw = exp(rent_log_cf_raw)) |> 
  mutate(rent_raw = exp(rent_log_raw)) |> 
  mutate(rent_dif = rent_cf_raw - rent_raw) |> 
  select(id, year, rent_raw, rent_dif) |> 
  inner_join(tenant_count, by = c("id", "year"))

did_rent_non_treated_y0 |> 
  inner_join(select(reg, id, name_CSD), by = "id") |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif), .by = name_CSD) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif)) |> 
  filter(name_CSD == "Kingston") |> 
  select(name_CSD, mean_2022 = mean_rent_dif, total_2022 = total_rent_dif)

did_rent_non_treated_y1 |> 
  inner_join(select(reg, id, name_CSD), by = "id") |> 
  mutate(total_rent = rent_raw * tenant_count,
         total_rent_dif = rent_dif * tenant_count) |> 
  summarize(
    mean_rent = weighted.mean(rent_raw, tenant_count),
    mean_rent_dif = weighted.mean(rent_dif, tenant_count),
    total_rent = sum(total_rent),
    total_rent_dif = sum(total_rent_dif), .by = name_CSD) |> 
  mutate(pct = total_rent_dif / (total_rent + total_rent_dif)) |> 
  filter(name_CSD == "Kingston") |> 
  select(name_CSD, mean_2025 = mean_rent_dif, total_2025 = total_rent_dif)
