library(tidyverse)
library(tidycensus)
library(sf)
library(censusapi)

# 2020 Redistricting Data

# Spatial Data

wards <- st_read("../Shapefiles/Milwaukee/wards/ward.shp")
sbd <- st_read("../Shapefiles/Milwaukee/MPSSchoolBoardDistricts/MPS_School_Board_Districts.shp")

ward_ald_sbd <- read_csv("data/ward_ald_sbd.csv")

dec_vars <- load_variables(year = 2020, dataset = "pl")

black_nh <- dec_vars %>%
  filter(str_detect(name, "^P1_") & str_detect(label, "Black or African American")) %>%
  .[["name"]]


dem_vars <- c(Total = "P1_001N",
              `Total 18+` = "P3_001N",
              White = "P1_003N",
              `White 18+` = "P3_003N",
              Black = "P1_004N",
              `Black 18+` = "P3_004N",
              Asian = "P1_006N",
              `Asian 18+` = "P3_006N",
              Hispanic = "P2_002N",
              `Hispanic 18+` = "P4_002N")

v <- get_decennial(geograph = "voting district",
                   variables = dem_vars,
                   state = "WI",
                   county = "Milwaukee",
                   year = 2020,
                   geometry = FALSE, show_call = TRUE)

v_black <- get_decennial(geograph = "voting district",
                         variables = black_nh,
                         state = "WI",
                         county = "Milwaukee",
                         year = 2020,
                         geometry = FALSE, show_call = TRUE)


vv <- v_black %>%
  filter(str_detect(NAME, "^Milwaukee - C")) %>%
  mutate(WARD = str_extract(NAME, "\\d{4}"),
         WARD = str_remove_all(WARD, "^0*"))

v_wards <- left_join(wards, vv) %>%
  select(WARD, variable:geometry) %>%
  left_join(., ward_ald_sbd %>% mutate(Ward = as.character(Ward)), by = c("WARD" = "Ward")) 

names(v_wards) <- str_to_lower(names(v_wards)) %>%
  str_replace_all(., " ", "_")

v_wards %>%
  filter(str_detect(variable, "18\\+$") & !str_detect(variable, "Total")) %>%
  group_by(ward) %>%
  filter(value == max(value)) %>%
  ggplot() +
  geom_sf(aes(fill = variable)) +
  facet_wrap(~ school_board_district)


walk(1:8, function(x) {
  plot(v_wards %>%
      filter(!str_detect(variable, "Total") & school_board_district == x) %>%
      group_by(ward, "18+" = str_detect(variable, "18\\+$")) %>%
        mutate(variable = str_remove_all(variable, " 18\\+$")) %>%
      filter(value == max(value)) %>%
      ggplot() +
      geom_sf(aes(fill = variable)) +
      labs(title = paste0("SBD", x)) +
        facet_wrap(~ `18+`) +
      theme_void()
  )
})

v %>%
  st_intersection(., citylimits)
  ggplot() +
  geom_sf()


v_wards %>%
  group_by(ward) %>%
  summarise(total = sum(value)) %>%
  ungroup() %>%
  mutate(c = case_when(total ))
  ggplot() +
  geom_sf(aes(fill = total)) +
  scale_fill_steps2(high = 1400)
