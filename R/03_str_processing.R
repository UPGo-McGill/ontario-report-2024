#### STR PROCESSING ############################################################

source("R/01_startup.R")
qload("output/cmhc.qsm", nthreads = availableCores())
province <- qread("output/province.qs", nthreads = availableCores())
CSD <- qread("output/CSD.qs", nthreads = availableCores())
CD <- qread("output/CD.qs", nthreads = availableCores())


# Import raw monthly files ------------------------------------------------

monthly <- read_csv("data/monthly_raw.csv")
monthly_TO <- read_csv("data/monthly_raw_TO.csv")

monthly <-
  monthly |>
  mutate(property_ID = `Property ID`,
         month = `Reporting Month`,
         host_ID = coalesce(as.character(`Airbnb Host ID`),
                            `HomeAway Property Manager`),
         listing_type = `Listing Type`,
         property_type = `Property Type`,
         country = Country,
         region = State,
         R = `Reservation Days`,
         A = `Available Days`,
         B = `Blocked Days`,
         rev = `Revenue (USD)`,
         latitude = `Latitude`,
         longitude = `Longitude`,
         scraped = `Scraped During Month`,
         .keep = "none") |>
  mutate(month = yearmonth(month)) |>
  filter(!region %in% c("Idaho", "Vermont"))

monthly_TO <-
  monthly_TO |>
  mutate(property_ID = `Property ID`,
         month = `Reporting Month`,
         host_ID = coalesce(as.character(`Airbnb Host ID`),
                            `HomeAway Property Manager`),
         listing_type = `Listing Type`,
         property_type = `Property Type`,
         country = Country,
         region = State,
         R = `Reservation Days`,
         A = `Available Days`,
         B = `Blocked Days`,
         rev = `Revenue (USD)`,
         latitude = `Latitude`,
         longitude = `Longitude`,
         scraped = `Scraped During Month`,
         .keep = "none") |>
  mutate(month = yearmonth(month)) |>
  filter(!region %in% c("Idaho", "Vermont"))


# Combine files -----------------------------------------------------------

monthly <- 
  monthly_TO |> 
  anti_join(monthly, by = c("property_ID", "month")) |> 
  bind_rows(monthly) |> 
  arrange(property_ID, month) |> 
  mutate(across(c(host_ID, listing_type, latitude, longitude), \(x) first(x)), 
         .by = property_ID)

rm(monthly_TO)


# Fill in missing provinces -----------------------------------------------

missing_provinces <-
  monthly |>
  filter(is.na(region)) |>
  slice(1, .by = property_ID) |>
  strr_as_sf(3347) |>
  mutate(prov = unlist(nngeo::st_nn(
    geometry, st_transform(province, 3347)))) |>
  mutate(prov = province$province[prov]) |>
  st_drop_geometry() |>
  select(property_ID, prov) |>
  mutate(prov = if_else(prov == "Quebec", "Québec", prov))

monthly <-
  monthly |>
  left_join(missing_provinces, by = "property_ID") |>
  mutate(region = coalesce(prov, region)) |>
  select(-prov)

rm(missing_provinces)


# Filter to province ------------------------------------------------------

monthly <-
  monthly |>
  filter(region == "Ontario") |>
  select(-country, -region)


# Add city ----------------------------------------------------------------

CSD_to_join <-
  monthly |>
  slice(1, .by = property_ID) |>
  strr_as_sf(3347) |>
  select(property_ID) |>
  mutate(CSDUID = CSD$CSDUID[st_nearest_feature(geometry, CSD)]) |>
  st_drop_geometry()

CSD_to_join <-
  CSD_to_join |>
  inner_join(CSD, by = "CSDUID") |>
  select(property_ID, CSDUID, city, CMAUID)

CD_to_join <-
  monthly |>
  slice(1, .by = property_ID) |>
  strr_as_sf(3347) |>
  select(property_ID) |>
  mutate(CDUID = CD$CDUID[st_nearest_feature(geometry, CD)]) |>
  st_drop_geometry()

CD_to_join <-
  CD_to_join |>
  inner_join(CD, by = "CDUID") |>
  select(property_ID, CDUID)

monthly <-
  monthly |>
  inner_join(CSD_to_join, by = "property_ID") |>
  inner_join(CD_to_join, by = "property_ID") |> 
  relocate(city, CSDUID, CDUID, CMAUID, .after = property_type)

rm(CSD_to_join)
rm(CD_to_join)


# Trim file by first and last scraped date --------------------------------

monthly <-
  monthly |>
  filter(month <= max(month[scraped]),
         month >= min(month[scraped]),
         .by = property_ID)

 
# Fill in missing months, and clean up ------------------------------------

monthly <-
  monthly |>
  filter(!is.na(listing_type)) |>
  as_tsibble(key = property_ID, index = month) |>
  fill_gaps(scraped = FALSE) |>
  as_tibble() |> 
  mutate(across(c(host_ID, listing_type, property_type, city, CSDUID, CDUID, 
                  CMAUID, latitude, longitude), \(x) coalesce(x, first(x))), 
         .by = property_ID) |> 
  mutate(across(c(R, A, B, rev), \(x) coalesce(x, 0))) |>
  arrange(property_ID, month)


# Convert currency --------------------------------------------------------

exchange_rates <- qread("data/exchange_rates.qs")

monthly <-
  monthly |>
  inner_join(mutate(exchange_rates, month = yearmonth(year_month)),
            by = "month") |>
  mutate(rev = rev * exchange_rate) |>
  select(-year_month, -exchange_rate)

rm(exchange_rates)


# Find housing ------------------------------------------------------------

monthly <-
  monthly |>
  strr_housing() |>
  mutate(housing = if_else(property_type %in% c(
    "Home", "condo", "apartment", "house", "townhome", "cottage", "bungalow",
    "studio", "Vacation home"), TRUE, housing)) |>
  relocate(housing, .after = property_type)


# Calculate FREH ----------------------------------------------------------

# Add FREH
monthly <-
  monthly |>
  arrange(property_ID, month) |>
  mutate(FREH = listing_type == "Entire home/apt" & slide2_lgl(
    A, R, \(A, R) sum(A) + sum(R) >= 183 & sum(R) > 90, .before = 11),
    .by = property_ID, .after = B)

# Add FREH_3
monthly <-
  monthly |>
  arrange(property_ID, month) |>
  mutate(FREH_3 = listing_type == "Entire home/apt" & slide2_lgl(
    A, R, \(A, R) sum(A) + sum(R) >= 46 & sum(R) >= 23, .before = 2),
    .by = property_ID, .after = FREH)


# Calculate multilistings -------------------------------------------------

monthly <-
  monthly |>
  mutate(multi = (n() > 1 & sum(listing_type == "Entire home/apt") > 1) |
           n() > 2, .by = c(host_ID, month), .after = FREH_3) |>
  mutate(multi = if_else(is.na(host_ID), FALSE, multi))


# Add city_type -----------------------------------------------------------

monthly <-
  monthly |> 
  mutate_city_type()


# Save intermediate output ------------------------------------------------

qsave(monthly, file = "output/monthly.qs", nthreads = availableCores())
# monthly <- qread("output/monthly.qs", nthreads = availableCores())


# Filter to only Airbnb and housing ---------------------------------------

monthly_all <- monthly

monthly <- 
  monthly |> 
  filter(str_starts(property_ID, "ab-")) |> 
  filter(housing) |> 
  select(-housing)


# Calculate GH ------------------------------------------------------------

GH <- 
  monthly |> 
  summarize(
    created = as.Date(min(month)),
    scraped = as.Date(max(month) + 1) - 1,
    .by = c(property_ID, host_ID, listing_type, latitude, longitude)) |> 
  strr_as_sf(3347) |> 
  strr_ghost()

GH <-
  GH |> 
  select(-data) |> 
  mutate(month = yearmonth(date)) |> 
  slice(1, .by = c(ghost_ID, month)) |> 
  relocate(month, .after = ghost_ID) |> 
  select(-date)

GH <-
  GH |> 
  unnest(property_IDs) |> 
  rename(property_ID = property_IDs) |> 
  inner_join(select(monthly, property_ID, month, R, A, B), 
             by = c("property_ID", "month")) |> 
  summarize(property_IDs = list(property_ID),
            active_pct = sum(R + A) / sum(R + A + B),
            .by = c(ghost_ID, month, host_ID, listing_count, housing_units, 
                    geometry)) |> 
  relocate(property_IDs, active_pct, .before = geometry)

GH <- 
  GH |> 
  st_set_crs(3347) |> 
  st_join(CSD) |> 
  st_join(CD) |> 
  mutate_city_type() |> 
  select(-pop_CSD, -dwellings_CSD) |> 
  relocate(geometry, .after = last_col())


# Force post-September-2022 listings to Toronto ---------------------------

monthly <- 
  monthly |> 
  mutate(city = if_else(month >= yearmonth("2022-10"), "Toronto", city),
         city_type = if_else(month >= yearmonth("2022-10"), "Toronto", 
                             city_type))
  

# Save output -------------------------------------------------------------

qsave(monthly, file = "output/monthly.qs", nthreads = availableCores())
qsave(monthly_all, file = "output/monthly_all.qs", 
      nthreads = availableCores())
qsave(GH, file = "output/GH.qs", nthreads = availableCores())


# Get monthly/CMHC correspondence -----------------------------------------

# Start with only points within 250m of CMHC boundary
monthly_close <-
  monthly_all |>
  distinct(property_ID, .keep_all = TRUE) |>
  strr_as_sf(3347) |>
  st_filter(st_buffer(st_union(cmhc_nbhd), 250))

# Get intersections with st_join
prop_cmhc <-
  monthly_close |>
  select(property_ID, geometry) |>
  st_join(cmhc_nbhd) |>
  arrange(property_ID, id) |>
  slice(1, .by = property_ID) |>
  select(property_ID, id, province, CMA)

# Use st_nn for non-intersecting points
prop_cmhc_2 <-
  prop_cmhc |>
  filter(is.na(id)) |>
  select(property_ID, geometry) |>
  mutate(id = cmhc_nbhd$id[unlist(nngeo::st_nn(geometry, cmhc_nbhd))]) |>
  arrange(property_ID) |>
  st_drop_geometry() |>
  inner_join(st_drop_geometry(select(cmhc_nbhd, id, province, CMA)), by = "id")

prop_cmhc <-
  prop_cmhc |>
  st_drop_geometry() |>
  filter(!is.na(id)) |>
  bind_rows(prop_cmhc_2) |>
  arrange(property_ID)

qsave(prop_cmhc, "output/prop_cmhc.qs", nthreads = availableCores())
rm(monthly_close, prop_cmhc, prop_cmhc_2)
prop_cmhc <- qread("output/prop_cmhc.qs", nthreads = availableCores())


# Join to monthly_all -----------------------------------------------------

monthly_cmhc <-
  monthly_all |>
  mutate(year = year(month)) |>
  inner_join(prop_cmhc, by = "property_ID") |>
  inner_join(cmhc, by = c("id", "year"))


# Make version for September data -----------------------------------------

monthly_sept <-
  monthly_cmhc |> 
  filter(month(month) == 9) |> 
  summarize(
    active_count = sum(A + R) / 30,
    rev_count = sum(rev),
    FREH_count = sum(FREH),
    FREH_3_count = sum(FREH_3),
    EH = mean(listing_type == "Entire home/apt"),
    .by = c(id, year, rent, rent_rel, vacancy, vacancy_rel, universe)) |> 
  arrange(id, year) |> 
  full_join(select(cmhc, id, year, u2 = universe, r2 = rent, rr2 = rent_rel,
                   v2 = vacancy, vr2 = vacancy_rel), by = c("id", "year")) |> 
  mutate(rent = coalesce(rent, r2),
         rent_rel = coalesce(rent_rel, rr2),
         vacancy = coalesce(vacancy, v2),
         vacancy_rel = coalesce(vacancy_rel, vr2),
         universe = coalesce(universe, u2)) |> 
  select(-c(u2:vr2)) |> 
  # Add CMHC geometries, using full join to get all id/years even without data
  full_join((cmhc_nbhd |> 
               rowwise() |> 
               mutate(year = list(2016:2022)) |> 
               ungroup() |> 
               unnest(year)), by = c("id", "year")) |> 
  mutate(across(c(active_count, rev_count, FREH_count, FREH_3_count), 
                \(x) coalesce(x, 0))) |> 
  mutate(
    active = active_count / dwellings,
    rev = rev_count / (rent_DA * dwellings * tenant),
    FREH = FREH_count / dwellings,
    FREH_3 = FREH_3_count / dwellings,
    active_u = active_count / universe,
    rev_u = rev_count / (rent * universe),
    FREH_u = FREH_count / universe,
    FREH_3_u = FREH_3_count / universe,
    .after = EH) |> 
  relocate(rent_rel, vacancy_rel, .after = last_col()) |> 
  mutate(
    across(c(rent:FREH_3_u), 
           list(change = \(x) slide_dbl(x, \(y) y[2] - y[1], .before = 1))),
    .by = id) |> 
  st_as_sf()


# Save output -------------------------------------------------------------

qsave(monthly_cmhc, file = "output/monthly_cmhc.qs", 
      nthreads = availableCores())
qsave(monthly_sept, "output/monthly_sept.qs", nthreads = availableCores())
