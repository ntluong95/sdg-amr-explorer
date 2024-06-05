#
map_info <- import(here("../data/0. Mapping countries.xlsx"), 
                   which = "country mapping") %>% 
  clean_names() %>% 
  
  # Create columns: LIC, MIC, HIC
  mutate(income_group = case_when(
    wb_income_group_2022 == "LIC"                             ~ "LIC",
    wb_income_group_2022 %in% c("MIC (lower)", "MIC (upper)") ~ "MIC",
    TRUE                                                 ~ "HIC"),
    LIC = ifelse(wb_income_group_2022 == "LIC", 1, 0),
    MIC = ifelse(wb_income_group_2022 %in% c("MIC (lower)", "MIC (upper)"), 1, 0),
    HIC = ifelse(wb_income_group_2022 == "HIC", 1, 0),
    sdg = case_when(
      !is.na(sdg1) | !is.na(sdg2) | !is.na(sdg3) ~ "Yes",
      TRUE ~ "no"),
    country = countrycode::countrycode(
      iso3, "iso3c", "country.name",
      # Country code couldn't recognize Kosovo and Tuvalu
      custom_match = c("USA" = "USA",
                       "SWZ" = "Swaziland",
                       "CIV" = "Ivory Coast",
                       "COD" = "Democratic Republic of the Congo",
                       "PSE" = "Palestine",
                       "CZE" = "Czech Republic",
                       "MKD" = "Macedonia",
                       "GBR" = "UK",
                       "MMR" = "Myanmar",
                       "FSM" = "Micronesia",
                       "XKX" = "Kosovo"))) %>% 
  # select only columns need for the analysis
  select(country, iso3, geographic_region, income_group, selection, amr_language, sdg_availabe = sdg_nap_23) %>% 
  # rename abbreviation income group
  mutate(income_group = case_when(
    income_group == "LIC" ~ "Low income",
    income_group == "MIC" ~ "Middle income",
    income_group == "HIC" ~ "High income")) %>% 
  mutate(amr_available = "Yes") %>% 
  # AMR document is in English and accessible
  mutate(amr_language = case_when(
    amr_language == "English" & !country %in% c("Vietnam", "Macedonia") ~ "Yes",
    TRUE                      ~ "No")) %>% 
  # SDG document is in English and accessible
  mutate(sdg_available = case_when(
    sdg_availabe == "Yes" ~ "Yes",
    TRUE                  ~ "No"))



# 
# 
world_map <- map_data("world")
