#
map_info <- import(
  here("data", "0. Mapping countries.xlsx"),
  which = "country mapping"
) %>%
  clean_names() %>%

  # Create columns: LIC, MIC, HIC
  mutate(
    income_group = case_when(
      wb_income_group_2022 == "LIC" ~ "LIC",
      wb_income_group_2022 %in% c("MIC (lower)", "MIC (upper)") ~ "MIC",
      TRUE ~ "HIC"
    ),
    LIC = ifelse(wb_income_group_2022 == "LIC", 1, 0),
    MIC = ifelse(
      wb_income_group_2022 %in% c("MIC (lower)", "MIC (upper)"),
      1,
      0
    ),
    HIC = ifelse(wb_income_group_2022 == "HIC", 1, 0),
    sdg = case_when(
      !is.na(sdg1) | !is.na(sdg2) | !is.na(sdg3) ~ "Yes",
      TRUE ~ "no"
    ),
    country = countrycode::countrycode(
      iso3,
      "iso3c",
      "country.name",
      # Country code couldn't recognize Kosovo and Tuvalu
      custom_match = c(
        "USA" = "USA",
        "SWZ" = "Swaziland",
        "CIV" = "Ivory Coast",
        "COD" = "Democratic Republic of the Congo",
        "PSE" = "Palestine",
        "CZE" = "Czech Republic",
        "MKD" = "Macedonia",
        "GBR" = "UK",
        "MMR" = "Myanmar",
        "FSM" = "Micronesia",
        "XKX" = "Kosovo"
      )
    )
  ) %>%
  # select only columns need for the analysis
  select(
    country,
    iso3,
    geographic_region,
    income_group,
    selection,
    amr_language,
    sdg_availabe = sdg_nap_23
  ) %>%
  # rename abbreviation income group
  mutate(
    income_group = case_when(
      income_group == "LIC" ~ "Low income",
      income_group == "MIC" ~ "Middle income",
      income_group == "HIC" ~ "High income"
    )
  ) %>%
  mutate(
    income_group = factor(
      income_group,
      levels = c("Low income", "Middle income", "High income")
    )
  ) %>%
  mutate(amr_available = "Yes") %>%
  # AMR document is in English and accessible
  mutate(
    amr_language = case_when(
      amr_language == "English" & !country %in% c("Vietnam", "Macedonia") ~
        "Yes",
      TRUE ~ "No"
    )
  ) %>%
  # SDG document is in English and accessible
  mutate(
    sdg_available = case_when(
      sdg_availabe == "Yes" ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  arrange(geographic_region, income_group)

#
#
world_map <- map_data("world")

table_s1 <- data.frame(
  Country = c(
    "Ethiopia",
    "Ethiopia",
    "Rwanda",
    "Rwanda",
    "Uganda",
    "Uganda",
    "Kenya",
    "Kenya",
    "Zambia",
    "Zambia",
    "Thailand",
    "Thailand",
    "China",
    "China",
    "Philippines",
    "Philippines",
    "Saudi Arabia",
    "Saudi Arabia",
    "Japan",
    "Japan"
  ),
  DocumentType = c(
    "AMR",
    "SDGs",
    "AMR",
    "SDGs",
    "AMR",
    "SDGs",
    "AMR",
    "SDGs",
    "AMR",
    "SDGs",
    "AMR",
    "SDGs",
    "AMR",
    "SDGs",
    "AMR",
    "SDGs",
    "AMR",
    "SDGs",
    "AMR",
    "SDGs"
  ),
  DocumentName = c(
    # Ethiopia
    "Antimicrobial resistance prevention and containment strategic plan: The One Health approach (3rd edition 2021-2025)",
    "Ten years development plan: A pathway to prosperity 2021-2030",

    # Rwanda
    "National Action Plan on Antimicrobial Resistance 2020-2024",
    "7 Years government program: National strategy for transformation (2017-2024)",

    # Uganda
    "Antimicrobial Resistance National Action Plan 2018-2023",
    "Third national development plan 2020/21–2024/25",

    # Kenya
    "National Action Plan on prevention and containment of Antimicrobial Resistance (2017-2022)",
    "Kenya Third Medium Term Plan 2018 – 2022",

    # Zambia
    "Multi-sectoral National Action Plan on Antimicrobial Resistance (2007-2027)",
    "Eight National Development Plan 2022-2026",

    # Thailand
    "Thailand's National Strategic Plan on Antimicrobial Resistance 2017-2021",
    "National strategy 2018-2037",

    # China
    "National Action Plan to Contain Antimicrobial Resistance (2022-2025)",
    "The 14th Five-Year Plan (2021-2025) for National Economic and Social Development and Vision 2035 of the People’s Republic of China",

    # Philippines
    "The Philippine Action Plan to combat Antimicrobial Resistance 2019-2023",
    "Philippine Development Plan 2023-2028",

    # Saudi Arabia
    "Kingdom Saudi Arabia National Action Plan on combating Antimicrobial Resistance",
    "National Transformation Program Delivery Plan (2021 – 2025)",

    # Japan
    "National Action Plan on Antimicrobial Resistance (AMR) 2023-2027",
    "The SDGs Implementation Guiding Principles"
  ),
  stringsAsFactors = FALSE
)
