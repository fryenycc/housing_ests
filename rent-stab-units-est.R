library(tidyverse)
library(fs)
library(dplyr)

data_dir <- "Desktop/housing_ests/rent_stab"
dir_create(data_dir)

# Rent Stabilization ------------------------------------------------------

rentstab_url <- "https://s3.amazonaws.com/justfix-data/rentstab_counts_from_doffer_2019.csv"
rentstab_file <- path(data_dir, path_file(rentstab_url))
download.file(rentstab_url, rentstab_file)

rentstab_bbls <- rentstab_file %>% 
  read_csv(col_types = "cdcdc") %>% 
  transmute(
    bbl = ucbbl,
    rent_stab_2019_units = uc2019
  )


# PLUTO -------------------------------------------------------------------

pluto_url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_20v6_csv.zip"
pluto_zip <- path(data_dir, path_file(pluto_url))
download.file(pluto_url, pluto_zip)
unzip(pluto_zip, exdir = data_dir)
pluto_file <- path(data_dir, "pluto_file.csv")

pluto_res_bbls <- pluto_file %>% 
  read_csv(
    col_types = cols_only(
      bbl = "c", 
      unitsres = "d", 
      bldgarea = "d",
      bldgclass = "c"
    )
  ) %>% 
  filter(unitsres > 0) %>% 
  transmute(
    bbl = str_sub(bbl, 1, 10),
    res_units = unitsres,
    bldg_area = bldgarea,
    bldg_class = bldgclass
  )


# Subsidized Housing Database ---------------------------------------------

shd_url <- "https://furmancenter.org/files/CoreData/FC_SHD_bbl_analysis_2020-06-30.csv"

shd_file <- path(data_dir, path_file(shd_url))
download.file(shd_url, shd_file)

non_j51_421a_subsidized <- shd_file %>% 
  read_csv(col_types = cols(.default = "c")) %>% 
  mutate(dplyr::across(starts_with("prog_"), as.numeric)) %>% 
  rowwise() %>%
  mutate(
    j51_421a_sub_num = sum(c_across(matches("prog_((j51)|(421a))"))),
    all_sub_num = sum(c_across(matches("prog_"))),
    non_j51_421a_sub_num = all_sub_num - j51_421a_sub_num,
    non_j51_421a_sub = (non_j51_421a_sub_num > 0)
  ) %>% 
  filter(non_j51_421a_sub) %>% 
  select(bbl, non_j51_421a_sub)


# Final dataset -----------------------------------------------------------

pluto_res_bbls %>% 
  left_join(rentstab_bbls, by = "bbl") %>% 
  left_join(non_j51_421a_subsidized, by = "bbl") %>% 
  mutate(
    rent_stab_2019_units = coalesce(rent_stab_2019_units, 0),
    rent_stab_2019_pct = rent_stab_2019_units / res_units,
    non_j51_421a_sub = coalesce(rent_stab_2019_units, FALSE)
  ) %>% 
  write_csv(path(data_dir, "bbls_rent-stab-analysis_2020-10-15.csv"))
