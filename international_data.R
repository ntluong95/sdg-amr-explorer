df_combined <- import(here("data", "sdg-amr.xlsx")) %>%
  mutate(
    drivers = case_when(
      drivers == "patient health status" ~ "human susceptibility to infection",
      drivers == "environmental contamination and degradation" ~
        "environment, climate and ecosystem changes",
      drivers ==
        "close human-animal interface and wildlife-associated transmission" ~
        "human wildlife interaction",
      TRUE ~ drivers
    )
  )

#                     discharge of waste from anthropogenic activities  8 0.025236593
#                                        doctor knowledge and attitude  7 0.022082019
#                             drug promotion by pharmaceutical company  5 0.015772871
#                                  farm model and financial conditions  2 0.006309148
#                                           farmer awareness about AMR  7 0.022082019
#                                       food-animal trading activities  5 0.015772871
#  gaps in AMR-related policy adoption, implementation and enforcement 13 0.041009464
#                                                  governance capacity 23 0.072555205
#                                                  humanitarian crisis  6 0.018927445
#                    inadequate infection control measures in hospital  3 0.009463722
#                                  inappropriate prescribing by doctor  6 0.018927445
#                            inappropriate prescribing by veterinarian  8 0.025236593
#         increasing meat consumption due to socioeconomic transitions  2 0.006309148
#                                 individual socio-demographic factors 36 0.113564669
#                           infection by resistant bacteria in animals  5 0.015772871
#                             infection by resistant bacteria in human  9 0.028391167
#                                  insufficient public health services  5 0.015772871
#                                         limited access to healthcare  9 0.028391167
#                                  limited investment for AMR research 13 0.041009464
#                        limited laboratory capacity in human medicine  4 0.012618297
#                   limited laboratory capacity in veterinary medicine  5 0.015772871
#                       poor AMR surveillance system in human medicine 13 0.041009464
#                  poor AMR surveillance system in veterinary medicine 13 0.041009464
#                         poor access to WASH and other infrastructure  8 0.025236593
#               poor animal husbandry practice and bio-risk management  8 0.025236593
#                                           public awareness about AMR  6 0.018927445
#                        shortage of quality antibiotics in the market  4 0.012618297
#                                              social-cultural factors  9 0.028391167
#                                     substandard healthcare provision  1 0.003154574
#                                        substandard veterinary system  4 0.012618297
#                  the decline in antibiotics research and development 10 0.031545741
#                         the expansion of intensive husbandry farming  2 0.006309148
#                                   uncontrolled access to antibiotics  2 0.006309148
#                                  veterinarian knowledge and attitude 10 0.031545741

interactive_table <- import(here("data", "interactive_table.xlsx"))

amr_selection <- c("AMR-01", "AMR-02", "AMR-03", "AMR-04", "AMR-05")
sdg_selection <- c(
  "SDG-01",
  "SDG-02",
  "SDG-03",
  "SDG-04",
  "SDG-05",
  "SDG-06",
  "SDG-07",
  "SDG-08",
  "SDG-09",
  "SDG-10",
  "SDG-11",
  "SDG-12",
  "SDG-13",
  "SDG-14",
  "SDG-15",
  "SDG-16",
  "SDG-17"
)

target_by_country <- import(here("data", "target_by_country.xlsx")) %>%
  mutate(
    policy_agenda = case_when(
      policy_agenda == "GAP-AMR" ~ "Only targeted by AMR-NAPs",
      policy_agenda == "UN-SDG" ~ "Only targeted by SDG-NAPs",
      policy_agenda == "Both" ~ "Targeted by both AMR-NAPs and SDG-NAPs",
      policy_agenda == "No targeted" ~ "Untargeted by any policy agenda"
    )
  )

summarized_data <- import(here("data", "nap-coverage.xlsx")) %>%
  select(-Both, -`GAP-AMR`, -`UN-SDG`) %>%
  pivot_longer(
    cols = c(`AMR National Action Plans`, `SDG National Action Plans`),
    names_to = "policy_agenda",
    values_to = "n"
  ) %>%
  mutate(
    coverage = case_when(
      type == "Proximal drivers" ~ n / 18 * 100,
      type == "Distal drivers" ~ n / 20 * 100
    ),
    type = factor(type, levels = c("Proximal drivers", "Distal drivers")),
    country = factor(
      country,
      levels = c(
        "Ethiopia",
        "Uganda",
        "Rwanda",
        "Zambia",
        "Kenya",
        "Thailand",
        "Philippines",
        "China",
        "Japan",
        "Saudi Arabia"
      )
    )
  )
