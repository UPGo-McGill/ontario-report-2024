#### Chapter 2: Short-term rentals in Ontario: Market overview #################

source("R/01_startup.R")
monthly <- qread("output/monthly.qs", nthreads = availableCores())
qload("output/ia.qsm", nthreads = availableCores())
GH <- qread("output/GH.qs")
CSD <- qread("output/CSD.qs")
CT <- qread("output/CT.qs")
DA <- qread("output/DA.qs")
province <- qread("output/province.qs")
water <- qread("output/water.qs")


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
    pull(scraped) |> 
    sum() |> 
    scales::comma(100))

(scraped_dec_2023_not_housing <-
  monthly_ia_all |> 
    filter(!housing, month == yearmonth("2023-12")) |> 
    count(city_type) |> 
    inner_join(coefs_type, by = "city_type") |> 
    mutate(scraped = scraped * n) |> 
    pull(scraped) |> 
    sum() |> 
    scales::comma(100))
  
(active_dec_2023 <-
  active_type |> 
  filter(month == yearmonth("2023-12")) |> 
  pull(active) |> 
  sum() |> 
  scales::comma(100))

(active_2023_pct <- 
    active_type |> 
    filter(month == yearmonth("2023-12")) |> 
    pull(active) |> 
    sum()) / (scraped_type |> 
                filter(month == yearmonth("2023-12")) |> 
                pull(scraped) |> 
                sum())

(hosts_active_dec_2023 <-
  monthly_ia |> 
  filter(month == yearmonth("2023-12")) |> 
  count(host_ID) |> 
  nrow() |> 
  (\(x) x *coefs$active)() |> 
  scales::comma(100))

(hosts_scraped_dec_2023 <-
    monthly_ia |> 
    filter(month == yearmonth("2023-12")) |> 
    count(host_ID) |> 
    nrow() |> 
    scales::comma(100))

(rev_july_2023 <-
  monthly_ia |> 
  filter(month == yearmonth("2023-07-01")) |> 
  count(city_type) |> 
  inner_join(coefs_type, by = "city_type") |> 
  mutate(rev = n * rev) |> 
  pull(rev) |> 
  sum() |> 
  scales::dollar(1, scale = 1/1000000, suffix = " million"))

(rev_avg_july_2023 <-
  monthly_ia |> 
  filter(month == yearmonth("2023-07-01")) |> 
  count(city_type) |> 
  inner_join(coefs_type, by = "city_type") |> 
  mutate(active = active * n, rev = rev * n) |> 
  summarize(rev_avg = sum(rev) / sum(active)) |> 
  pull(rev_avg) |> 
  scales::dollar(100))

# ON rent
(rent_2021 <- get_census("CA21", regions = list(PR = "35"), 
                        vectors = c(rent = "v_CA21_4318")) |> 
  pull(rent))

# 2022-2023 comparison
monthly_ia |> 
  count(month, city_type) |> 
  inner_join(coefs_type) |> 
  mutate(active = n * active, rev = n * rev) |> 
  summarize(
    active_2023 = active[month == yearmonth("2023-12")],
    active_2022 = active[month == yearmonth("2022-12")],
    active_yoy = (active_2023 - active_2022) / active_2022,
    rev_2023 = rev[month == yearmonth("2023-07")],
    rev_2022 = rev[month == yearmonth("2022-07")],
    rev_yoy = (rev_2023 - rev_2022) / rev_2022, .by = city_type) |> 
  summarize(
    across(c(active_2023, active_2022, rev_2023, rev_2022), sum),
    active_yoy = (active_2023 - active_2022) / active_2022,
    rev_yoy = (rev_2023 - rev_2022) / rev_2022)

# 2019 comparison
monthly |> 
  filter(month == yearmonth("2019-12")) |> 
  summarize(active = sum(A + R) / 30)


# Figure 1.1 --------------------------------------------------------------

fig_1_1_list_1 <-
  map(c("Toronto", "Ottawa", "Kingston"), \(name) {
    
    monthly_ia |> 
      filter(city == name) |> 
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
      geom_sf(data = filter(CSD, city == name), fill = "grey",
              colour = "transparent") +
      geom_sf(aes(fill = active_pct), colour = "white", lwd = 0.05) +
      geom_sf(data = water, fill = "white", colour = "transparent") +
      scale_fill_stepsn(name = "STRs / dwelling", 
                        colours = col_palette[c(4, 6)],
                           breaks = c(0, 0.004, 0.008, 0.012, 0.016, 0.02), 
                           limits = c(0, 0.02), oob = scales::squish,
                           labels = scales::percent) +
      coord_sf(xlim = st_bbox(filter(CSD, city == name))[c("xmin", "xmax")],
               ylim = st_bbox(filter(CSD, city == name))[c("ymin", "ymax")]) +
      ggtitle(name) +
      theme_void() +
      theme(plot.margin = unit(c(0, 10, 10, 0), "pt"), 
            legend.position = "bottom",
            legend.key.height = unit(20, "pt"), 
            text = element_text(family = "Futura"))
  })

fig_1_1_list_2 <-
  map(c("The Blue Mountains", "Prince Edward County", 
        "Niagara Falls"), \(name) {
    
    monthly_ia |> 
        filter(city == name) |> 
        filter(month == max(month)) |> 
        strr_as_sf(3347) |> 
        st_join(DA, y = _, left = FALSE) |> 
        summarize(n = n(), across(geometry, st_union), 
                  .by = c(city_type, DA, dwellings)) |> 
        inner_join(coefs_type, by = "city_type") |> 
        mutate(active = active * n) |> 
        mutate(active_pct = active / dwellings) |>  
      ggplot() +
      geom_sf(data = DA, fill = "grey90", colour = "transparent") +
      geom_sf(data = filter(CSD, city == name), fill = "grey",
              colour = "transparent") +
      geom_sf(aes(fill = active_pct), colour = "white", lwd = 0.05) +
      geom_sf(data = water, fill = "white", colour = "transparent") +
      scale_fill_stepsn(name = "STRs / dwelling", 
                        colours = col_palette[c(4, 3)],
                        breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 
                                   0.07), limits = c(0, 0.07), 
                        oob = scales::squish,
                        labels = scales::percent) +
      coord_sf(xlim = st_bbox(filter(CSD, city == name))[c("xmin", "xmax")],
               ylim = st_bbox(filter(CSD, city == name))[c("ymin", "ymax")]
      ) +
      ggtitle(name) +
      theme_void() +
      theme(plot.margin = unit(c(0, 10, 10, 0), "pt"), 
            legend.position = "bottom",
            legend.key.height = unit(20, "pt"), 
            text = element_text(family = "Futura"))
  })

fig_1_1_1 <- patchwork::wrap_plots(fig_1_1_list_1) + 
  plot_layout(guides = "collect") & theme(legend.position = "right")

fig_1_1_2 <- patchwork::wrap_plots(fig_1_1_list_2) + 
  plot_layout(guides = "collect") & theme(legend.position = "right")

fig_1_1 <- patchwork::wrap_plots(c(fig_1_1_list_1, fig_1_1_list_2)) + 
  plot_layout(guides = "collect") & theme(legend.position = "right")

ggsave("output/figure_1_1.png", fig_1_1, width = 9, height = 6)


# Figure 1.2 --------------------------------------------------------------

fig_1_2 <-
  scraped_type |> 
  summarize(city_type = "Total", scraped = sum(scraped), .by = month) |> 
  bind_rows(scraped_type) |> 
  mutate(type = "Displayed") |> 
  rename(value = scraped) |> 
  bind_rows(
    active_type |> 
      summarize(city_type = "Total", active = sum(active), .by = month) |> 
      bind_rows(active_type) |> 
      mutate(type = "Active (est.)") |> 
      rename(value = active)) |> 
  mutate_type() |>
  arrange(city_type, month, type) |> 
  filter(month >= yearmonth("2017-07"), month <= yearmonth("2023-12")) |>
  filter(!city_type %in% c("Total", "Toronto") |
           !month %in% c(yearmonth("2023-06"), yearmonth("2023-07"))) |>
  mutate(label = if_else(city_type == "Total" & month == yearmonth("2019-05"),
                         type, NA)) |>
  mutate(value = slide_dbl(value, \(x) mean(x), .before = 1), 
         .by = c(city_type, type)) |> 
  ggplot() +
  geom_line(aes(month, value, colour = city_type, linetype = type), 
            linewidth = 1) +
  geom_label(aes(month, value, label = label), family = "Futura", size = 3) +
  facet_wrap(~city_type, scales = "free_y", nrow = 2) +
  scale_colour_manual(guide = "none", values = c(col_palette, 
                                                 col_palette[1:2])) +
  scale_y_continuous(name = NULL, labels = scales::comma) +
  scale_x_yearmonth(name = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"), legend.position = "none",
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figure_1_2.png", fig_1_2, width = 10, height = 5)


# Table 1 -----------------------------------------------------------------

tab_1 <-
  monthly_ia |> 
  count(city_type, city, month) |> 
  inner_join(coefs_type, by = "city_type") |> 
  mutate(active = active * n, rev = rev * n) |> 
  mutate(active = if_else(city == "Toronto" & month == yearmonth("2022-12"), 
                          active_type |> 
                            filter(city_type == "Toronto", 
                                   month == yearmonth("2022-12")) |> 
                            pull(active), active)) |> 
  summarize(active_2023 = sum(active[month == yearmonth("2023-12")]),
            active_2022 = sum(active[month == yearmonth("2022-12")]),
            active_growth = (active_2023 - active_2022) / active_2022,
            rev_2023 = sum(rev[month == yearmonth("2023-07")]),
            .by = city) |> 
  left_join(monthly |> 
              filter(month == yearmonth("2022-07")) |> 
              summarize(rev_2022 = sum(rev), .by = city), 
            by = "city") |>  
  mutate(rev_growth = (rev_2023 - rev_2022) / rev_2022) |> 
  left_join(select(st_drop_geometry(CSD), city, dwellings = dwellings_CSD), 
            by = "city") |> 
  mutate(active_pct = active_2023 / dwellings, .after = active_growth) |> 
  arrange(-active_2023)

tab_1 |> 
  summarize(across(c(active_2023, active_2022, rev_2023, rev_2022, dwellings), 
            sum)) |>
  mutate(city = "Total", 
         active_growth = (active_2023 - active_2022) / active_2022,
         rev_growth = (rev_2023 - rev_2022) / rev_2022,
         active_pct = active_2023 / dwellings) |> 
  bind_rows(tab_1) |> 
  select(city, active_2023, active_growth, active_pct, rev_2023) |> 
  arrange(-active_2023) |> 
  slice(1:23) |> 
  mutate(active_2023 = scales::comma(active_2023, 10),
         active_growth = scales::percent(active_growth, 0.1),
         active_pct = scales::percent(active_pct, 0.1),
         rev_2023 = scales::dollar(rev_2023, 0.1, scale = 1/1000000, 
                                   suffix = " million")) |> 
  gt::gt()


# Figure 3 ----------------------------------------------------------------

fig_1_3 <-
  active_type |> 
  arrange(city_type, month) |> 
  mutate(active_change = slide_dbl(active, \(x) (x[13] - x[1]) / x[1], 
                                   .before = 12, .complete = TRUE), 
         .by = city_type) |> 
  bind_rows(
    active_type |> 
      summarize(active = sum(active), .by = month) |> 
      arrange(month) |> 
      mutate(city_type = "Total",
             active_change = slide_dbl(active, \(x) (x[13] - x[1]) / x[1], 
                                       .before = 12, .complete = TRUE))) |> 
  filter(month >= yearmonth("2018-06")) |> 
  mutate_type() |> 
  ggplot(aes(month, active_change, colour = city_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(lwd = 1, na.rm = TRUE) +
  scale_x_yearmonth(name = NULL) +
  scale_y_continuous(name = NULL, limits = c(-0.6, 1.2), oob = scales::squish,
                     labels = scales::percent) +
  scale_colour_manual(guide = "none", values = col_palette[c(1:6, 1:2)]) +
  facet_wrap(~city_type, nrow = 2) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_1_3.png", fig_1_3, width = 10, height = 5)


# Home sharers and commercial operators -----------------------------------

# EH %
(EH_pct_2023 <-
  monthly_ia |> 
  filter(month == yearmonth("2023-12")) |> 
  summarize(EH = mean(listing_type == "Entire home/apt")) |> 
  pull(EH) |> 
  scales::percent(0.1))

(EH_pct_2019 <- 
  monthly |> 
  filter(month == yearmonth("2019-12")) |> 
  summarize(EH = mean(listing_type == "Entire home/apt")) |> 
  pull(EH) |> 
  scales::percent(0.1))

revenue_colour <- rev(col_palette[c(6, 1, 4, 5, 3, 2)])

monthly_for_rev <- 
  monthly |>  
  filter(!is.na(host_ID), month == yearmonth("2022-09"), R > 0) |> 
  summarize(rev = sum(rev), .by = c(city_type, host_ID))

monthly_for_rev <- 
  monthly_for_rev |> 
  summarize(rev = sum(rev), .by = host_ID) |> 
  mutate(city_type = "Total") |> 
  bind_rows(monthly_for_rev)

host_deciles <-
  monthly_for_rev |> 
  group_by(city_type) |> 
  summarize(all = sum(rev),
            top_10 = sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_20 = sum(rev[rev > quantile(rev, c(0.80))] / all) - 
              sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_30 = sum(rev[rev > quantile(rev, c(0.70))] / all) - 
              sum(rev[rev > quantile(rev, c(0.80))] / all),
            top_40 = sum(rev[rev > quantile(rev, c(0.60))] / all) - 
              sum(rev[rev > quantile(rev, c(0.70))] / all),
            top_50 = sum(rev[rev > quantile(rev, c(0.50))] / all) - 
              sum(rev[rev > quantile(rev, c(0.60))] / all),
            top_60 = sum(rev[rev > quantile(rev, c(0.40))] / all) - 
              sum(rev[rev > quantile(rev, c(0.50))] / all),
            top_70 = sum(rev[rev > quantile(rev, c(0.30))] / all) - 
              sum(rev[rev > quantile(rev, c(0.40))] / all),
            top_80 = sum(rev[rev > quantile(rev, c(0.20))] / all) - 
              sum(rev[rev > quantile(rev, c(0.30))] / all),
            top_90 = sum(rev[rev > quantile(rev, c(0.10))] / all) - 
              sum(rev[rev > quantile(rev, c(0.20))] / all),
            top_100 = sum(rev[rev > quantile(rev, c(0.00))] / all) - 
              sum(rev[rev > quantile(rev, c(0.10))] / all)) |> 
  select(-all) |> 
  pivot_longer(-city_type, names_to = "percentile", values_to = "value") |> 
  mutate(percentile = factor(percentile, levels = paste0("top_", 1:10 * 10))) |> 
  mutate(perfect_distribution = 0.1,
         decile = rep(1:10, 8),
         dummy_1 = perfect_distribution,
         dummy_2 = value) |> 
  rename("0" = perfect_distribution, "1" = value, "0.25" = dummy_1, 
         "0.75" = dummy_2) |> 
  pivot_longer(c("0","0.25", "0.75", "1"), names_to = "position") |> 
  mutate(position = as.numeric(position), 
         display_val = scales::percent(value, .1)) |> 
  group_by(city_type, position) |> 
  mutate(absolute_val = slider::slide_dbl(value, ~{.x[1] / 2 + sum(.x[-1])}, 
                                          .after = 9)) |> 
  ungroup() |> 
  mutate(
    display_val = paste0("earned ", display_val, "\nof revenue"),
    display_percentile = case_when(
      percentile == "top_10" ~ "Top 10% of hosts...",
      percentile == "top_20" ~ "Next 10% of hosts...",
      TRUE ~ NA_character_))

host_rev_data <- 
  monthly_for_rev |> 
  summarize(rev = sum(rev), .by = host_ID) |> 
  arrange(-rev) |> 
  summarize(
    pct_1 = quantile(rev, 0.99),
    pct_10 = quantile(rev, 0.9),
    pct_50 = quantile(rev, 0.5),
    n_1 = round(n() / 10),
    rev_1 = sum(rev[rev >= pct_1]) / sum(rev),
    rev_10 = sum(rev[rev >= pct_10]) / sum(rev),
  )

(host_top_10_pct <- scales::percent(host_rev_data$rev_10, 0.1))
(host_top_1_pct <- scales::percent(host_rev_data$rev_1, 0.1))
(host_top_1_n <- scales::comma(host_rev_data$n_1, 10))
(host_median <- scales::dollar(host_rev_data$pct_50, 100))


# Table 2 -----------------------------------------------------------------

monthly_ia |> 
  filter(month == yearmonth("2023-12")) |> 
  summarize(n = n(), pct = mean(multi), .by = city_type) |> 
  bind_rows(
    monthly_ia |> 
      filter(month == yearmonth("2023-12")) |> 
      summarize(n = n(), city_type = "Total", pct = mean(multi))) |> 
  arrange(-n) |> 
  mutate(pct = scales::percent(pct, 0.1)) |> 
  select(-n) |> 
  gt::gt()
  

# Figure 1_4 --------------------------------------------------------------

fig_1_4 <-
  host_deciles |> 
  select(-display_val, -display_percentile) |> 
  mutate_type() |> 
  ggplot(aes(position, value, group = decile, fill = decile)) +
  geom_area(colour = "white", lwd = 1.2) +
  facet_wrap(~ city_type, nrow = 2) +
  scale_y_continuous(name = "Host decile", label = scales::label_percent(1),
                     breaks = seq(0, 1, by = 0.1), #limits = c(0, 1),
                     sec.axis = sec_axis(~., 
                                         name = "% of total revenue",
                                         labels = derive(), 
                                         breaks = derive())) +
  scale_fill_gradientn(colours = revenue_colour) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Futura"),
        axis.text.y = element_text(hjust = 1),
        axis.title.y.left = element_text(
          angle = 90, margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(
          angle = 270, margin = margin(0, 0, 0, 10)),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figure_1_4.png", fig_1_4, width = 10, height = 5)


# Short-term and medium-term rentals --------------------------------------

# Figure 1_5 --------------------------------------------------------------

fig_1_5 <- 
  monthly_ia |> 
  mutate(mtr = coalesce(min_stay >= 28, FALSE)) |> 
  summarize(mtr = mean(mtr), .by = c(city_type, month)) |>
  bind_rows(
    monthly_ia |> 
      mutate(mtr = coalesce(min_stay >= 28, FALSE)) |> 
      summarize(city_type = "Total", mtr = mean(mtr), .by = c(month))) |> 
  mutate_type() |> 
  mutate(label = if_else(month == yearmonth("2023-01"), city_type, NA)) |> 
  ggplot(aes(month, mtr, colour = city_type)) +
  geom_line(lwd = 1, na.rm = TRUE) +
  ggrepel::geom_label_repel(aes(label = label), family = "Futura") +
  scale_x_yearmonth(name = NULL) +
  scale_y_continuous(name = NULL, labels = scales::percent) +
  scale_colour_manual(guide = "none", values = col_palette[c(1:6, 1:2)]) +
  # facet_wrap(~city_type) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_1_5.png", fig_1_5, width = 8, height = 4)
