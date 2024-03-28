#### CMHC AND CENSUS PROCESSING ################################################

source("R/01_startup.R")


# Import census data ------------------------------------------------------

province <-
  get_census("CA21", regions = list(PR = 35), level = "PR",
             geo_format = "sf") |>
  st_transform(4326) |>
  select(province = name, province_ID = GeoUID, geometry) |>
  mutate(province = str_remove(province, " \\(.*\\)"))

CMA <-
  get_census("CA21", regions = list(PR = 35), level = "CMA",
             geo_format = "sf") |>
  st_transform(3347) |>
  select(CMA = GeoUID, name_CMA = name, pop_CMA = Population,
         dwellings_CMA = Dwellings, geometry) |>
  mutate(name_CMA = str_remove(name_CMA, " \\(.*\\)"))

CSD <-
  get_census("CA21", regions = list(PR = 35), level = "CSD",
             geo_format = "sf") |>
  st_transform(3347) |>
  select(CSDUID = GeoUID, city = name, CMAUID = CMA_UID, pop_CSD = Population,
         dwellings_CSD = Dwellings, geometry) |>
  as_tibble() |>
  st_as_sf()

CSD <-
  CSD |>
  # This may change for each different province
  filter(!str_detect(city, "\\((IRI)|(TWL)|(TAL)|(IGD)|(S-É)\\)")) |>
  # mutate(type = str_remove(city, "^.*\\("), .after = city) |>
  # mutate(type = str_remove(type, "\\)$")) |>
  mutate(city = str_remove(city, " \\(..\\)$")) |>
  mutate(city = str_remove(city, " \\(.\\)$")) |>
  # Fix for Hamilton TP
  mutate(city = if_else(
    city == "Hamilton" & is.na(CMAUID), "Hamilton TP", city))

DA <-
  get_census("CA21", regions = list(PR = 35), level = "DA",
             vectors = c(rent = "v_CA21_4318",
                         tenants = "v_CA21_4313",
                         owners = "v_CA21_4305",
                         entertainment = "v_CA21_6657",
                         accommodation = "v_CA21_6660",
                         tourism_parent = "v_CA21_6606"),
             geo_format = "sf") |>
  as_tibble() |>
  st_as_sf() |>
  st_transform(3347) |>
  mutate(tourism = entertainment + accommodation) |>
  select(DA = GeoUID, CMA = CMA_UID, pop = Population, dwellings = Dwellings,
         tourism, tourism_parent, rent, tenants, owners) |>
  mutate(area_DA = units::drop_units(st_area(geometry)), .before = geometry) |>
  st_set_agr("constant")

DA <-
  DA |>
  mutate(province_ID = substr(DA, 1, 2)) |>
  inner_join(st_drop_geometry(province), by = "province_ID") |>
  relocate(province, province_ID, .after = CMA)

# Add QC side of Ottawa region
DA_ott <-
  get_census("CA21", regions = list(CMA = 505), level = "DA",
             vectors = c(rent = "v_CA21_4318",
                         tenants = "v_CA21_4313",
                         owners = "v_CA21_4305",
                         entertainment = "v_CA21_6657",
                         accommodation = "v_CA21_6660",
                         tourism_parent = "v_CA21_6606"),
             geo_format = "sf") |>
  as_tibble() |>
  st_as_sf() |>
  st_transform(3347) |>
  mutate(tourism = entertainment + accommodation) |>
  select(DA = GeoUID, CMA = CMA_UID, pop = Population, dwellings = Dwellings,
         tourism, tourism_parent, rent, tenants, owners) |>
  mutate(area_DA = units::drop_units(st_area(geometry)), .before = geometry) |>
  st_set_agr("constant")

DA_ott <-
  DA_ott |>
  mutate(province_ID = substr(DA, 1, 2)) |>
  filter(province_ID == "24") |> 
  relocate(province_ID, .after = CMA) |> 
  mutate(province = "Quebec", .after = CMA)

DA <- bind_rows(DA, DA_ott)

province <-
  province |>
  select(-province_ID)

DA_union <-
  DA |>
  summarize()

CT <-
  get_census("CA21", regions = list(PR = 35), level = "CT",
             vectors = c(rent = "v_CA21_4318",
                         tenants = "v_CA21_4313",
                         owners = "v_CA21_4305"),
             geo_format = "sf") |>
  as_tibble() |>
  st_as_sf() |>
  st_transform(3347) |>
  select(CTUID = GeoUID, dwellings = Dwellings, rent, tenants, owners) |>
  st_set_agr("constant")


# Water -------------------------------------------------------------------

water <- read_sf("data/lhy_000c16a_e/lhy_000c16a_e.shp") |> 
  filter(PRUID == "35")


# Save census geometries --------------------------------------------------

qsave(DA, file = "output/DA.qs", nthreads = availableCores())
qsave(DA_union, file = "output/DA_union.qs", nthreads = availableCores())
qsave(CT, file = "output/CT.qs", nthreads = availableCores())
qsave(CSD, file = "output/CSD.qs", nthreads = availableCores())
qsave(CMA, file = "output/CMA.qs", nthreads = availableCores())
qsave(province, file = "output/province.qs", nthreads = availableCores())
qsave(water, "output/water.qs", nthreads = availableCores())


# Process neighbourhoods --------------------------------------------------

qload("data/cmhc_shp.qsm", nthreads = 10)

# 2017-2022 neighbourhoods are basically the same, with one exception which can
# be handled with imputation. So use 2022 geometries for all neighbourhoods to 
# facilitate spatial panel models
cmhc_nbhd <- 
  cmhc_nbhd_2022 |> 
  st_transform(3347) |> 
  mutate(id = paste0(METCODE, NBHDCODE)) |> 
  select(id, name = NBHDNAME_E, CMA = METCODE) |> 
  mutate(geometry = st_make_valid(geometry)) |> 
  st_set_agr("constant")

# Match 2016 geometries to the rest
cmhc_nbhd_2016 <- 
  cmhc_nbhd_2016 |> 
  st_transform(3347) |> 
  mutate(id = paste0(METCODE, NBHDCODE)) |> 
  select(id, name = NBHDNAME_E, CMA = METCODE) |> 
  mutate(geometry = st_make_valid(geometry))

cmhc_int <-
  cmhc_nbhd_2016 |> 
  mutate(area_2016 = as.numeric(st_area(geometry))) |> 
  st_set_agr("constant") |> 
  st_intersection(mutate(cmhc_nbhd, area_2022 = as.numeric(
    st_area(geometry)))) |> 
  mutate(area = as.numeric(st_area(geometry))) |> 
  filter(area == max(area), .by = id.1)
  
cmhc_join <- 
  cmhc_int |> 
  st_drop_geometry() |> 
  select(id_2016 = id, id_2022 = id.1)

cmhc_PR <- 
  cmhc_nbhd |> 
  st_set_agr("constant") |> 
  st_centroid() |> 
  nngeo::st_nn(st_transform(province, 3347))

cmhc_nbhd <- 
  cmhc_nbhd |> 
  mutate(province = !!province$province[as.numeric(cmhc_PR)], .after = name)

rm(cmhc_PR)

rm(cmhc_nbhd_2016, cmhc_nbhd_2017, cmhc_nbhd_2018, cmhc_nbhd_2019, 
   cmhc_nbhd_2020, cmhc_nbhd_2021, cmhc_nbhd_2022)


# Add nbhd- and CMA-level data --------------------------------------------

cmhc_nbhd <- 
  cmhc_nbhd |> 
  mutate(area_CMHC = units::drop_units(st_area(geometry)), 
         .before = geometry) |> 
  st_set_agr("constant")

cmhc_DA <-
  cmhc_nbhd |> 
  st_set_agr("constant") |> 
  st_intersection(st_set_agr(DA, "constant")) |> 
  mutate(area_int = units::drop_units(st_area(geometry)), .before = geometry)

cmhc_DA <- 
  cmhc_DA |> 
  st_drop_geometry() |> 
  summarize(
    pop = sum(pop * area_int / area_DA, na.rm = TRUE),
    dwellings = sum(dwellings * area_int / area_DA, na.rm = TRUE),
    tourism = sum(tourism * area_int / area_DA, na.rm = TRUE),
    tourism_parent = sum(tourism_parent * area_int / area_DA, na.rm = TRUE),
    rent_DA = sum(rent * tenants * area_int / area_DA, na.rm = TRUE) / 
      sum(tenants + area_int / area_DA, na.rm = TRUE),
    tenants_DA = sum(tenants + area_int / area_DA, na.rm = TRUE),
    owners_DA = sum(owners + area_int / area_DA, na.rm = TRUE),
    .by = c(id, name, province, CMA)
  )

cmhc_DA <- 
  cmhc_DA |> 
  mutate(
    tourism = tourism / tourism_parent,
    tenant = tenants_DA / (tenants_DA + owners_DA)) |>
  rename(tenant_count = tenants_DA) |> 
  select(-tourism_parent, -owners_DA)

cmhc_nbhd <- 
  cmhc_nbhd |> 
  select(-area_CMHC) |> 
  inner_join(cmhc_DA, by = join_by(id, name, province, CMA)) |> 
  relocate(geometry, .after = last_col())

cmhc_CMA <- 
  cmhc_nbhd |> 
  summarize(geometry = st_union(geometry), .by = CMA) |> 
  rename(CMA_old = CMA) |> 
  st_set_agr("constant") |> 
  st_centroid() |> 
  mutate(CMA_id = unlist(nngeo::st_nn(geometry, CMA)),
         CMA_id = CMA$CMA[CMA_id]) |> 
  st_drop_geometry()

cmhc_nbhd <- 
  cmhc_nbhd |> 
  inner_join(cmhc_CMA, by = c("CMA" = "CMA_old")) |> 
  select(-CMA) |> 
  rename(CMA = CMA_id) |> 
  relocate(CMA, .after = province)

cmhc_nbhd <- 
  cmhc_nbhd |> 
  inner_join(st_drop_geometry(CMA), by = "CMA") |> 
  relocate(name_CMA, .after = CMA) |> 
  relocate(geometry, .after = last_col())

rm(cmhc_CMA)


# Process data ------------------------------------------------------------

qload("data/cmhc_data.qsm", nthreads = 10)

# Fix 2016 IDs
cmhc_2016 <- 
  cmhc_2016 |> 
  mutate(id = paste0(met_code, nbrhood_code)) |> 
  select(id, year = Year, universe, rent = average_rent, 
         rent_rel = average_rent_reliability, vacancy = vacancy_rate_percent,
         vacancy_rel = vacancy_rate_reliability)

cmhc <- bind_rows(cmhc_2017, cmhc_2018, cmhc_2019, cmhc_2020, cmhc_2021, 
                  cmhc_2022) |> 
  mutate(id = paste0(met_code, nbrhood_code)) |> 
  select(id, year = Year, universe, rent = average_rent, 
         rent_rel = average_rent_reliability, vacancy = vacancy_rate_percent,
         vacancy_rel = vacancy_rate_reliability)

# Join 2016 and 2017-2022 data
cmhc <- 
  cmhc_2016 |> 
  left_join(cmhc_join, by = c(id = "id_2016")) |> 
  select(id = id_2022, year, universe, rent, rent_rel, vacancy, vacancy_rel) |> 
  bind_rows(cmhc)

# Plug gaps with cmhc_nbhd
cmhc <- 
  cmhc |> 
  full_join((cmhc_nbhd |> 
               st_drop_geometry() |> 
               select(id) |> 
               rowwise() |> 
               mutate(year = list(2016:2022)) |> 
               ungroup() |> 
               unnest(year)), by = c("id", "year"))

rm(cmhc_DA, cmhc_int, cmhc_join, cmhc_2016, cmhc_2017, cmhc_2018, cmhc_2019, 
   cmhc_2020, cmhc_2021, cmhc_2022)


# Harmonize cmhc and cmhc_nbhd re: missing data ---------------------------

cmhc <-
  cmhc |> 
  filter(sum(is.na(universe)) < 2, .by = id) |> 
  filter(sum(is.na(rent)) < 7, .by = id) |> 
  filter(!is.na(id)) |> 
  arrange(id, year)
  
cmhc_nbhd <- 
  cmhc_nbhd |> 
  filter(id %in% cmhc$id)


# Filter to province ------------------------------------------------------

cmhc <- 
  cmhc |> 
  filter(id %in% cmhc_nbhd$id)
  

# Save output and clean up ------------------------------------------------

qsavem(cmhc, cmhc_nbhd, file = "output/cmhc.qsm", nthreads = availableCores())

