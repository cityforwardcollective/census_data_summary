library(tidyverse)
library(tidycensus)
library(sf)

# 2020 Redistricting Data

# Spatial Data

wards <- st_read("../Shapefiles/Milwaukee/wards/ward.shp")
sbd <- st_read("../Shapefiles/Milwaukee/MPSSchoolBoardDistricts/MPS_School_Board_Districts.shp")

ward_ald_sbd <- read_csv("data/ward_ald_sbd.csv")

census_vars_2020 <- load_variables(year = 2020, dataset = "pl")

race_filters <- c("White",
                  "Black",
                  "Asian",
                  "American Indian",
                  "Native Hawaiian",
                  "Some Other")

race_vars_2020 <- map(race_filters, function(x) {
  t <- census_vars_2020 %>%
    filter(str_detect(name, "^P1_") & str_detect(label, x)) %>%
    .[[1]]
  a <- census_vars_2020 %>%
    filter(str_detect(name, "^P3_") & str_detect(label, x)) %>%
    .[[1]]
  
  c(t, a)
}) %>%
  set_names(., race_filters) 

any_race_2020 <-  map_df(race_filters, function(x) {
  t <- census_vars_2020 %>%
    filter(str_detect(name, "^P1_") & str_detect(label, x))%>%
    mutate(race = paste0(x)) %>%
    select(name, race)
  
  a <- census_vars_2020 %>%
    filter(str_detect(name, "^P3_") & str_detect(label, x))%>%
    mutate(race = paste0(x, " 18+")) %>%
    select(name, race)
  
  bind_rows(t, a)
}) %>%
  bind_rows(., 
            tibble(
              race = c("Hispanic 18+",
                       "Hispanic",
                       "Total",
                       "Total 18+",
                       "Two or More",
                       "Two or More 18+"),
              name = c("P4_002N",
                       "P2_002N",
                       "P1_001N",
                       "P3_001N",
                       "P1_009N",
                       "P3_009N")
            ))


dem_vars_2020 <- c(race_vars_2020$White,
              race_vars_2020$Black,
              race_vars_2020$Asian,
              race_vars_2020$`American Indian`,
              race_vars_2020$`Native Hawaiian`,
              race_vars_2020$Other,
              "P4_002N", # Hispanic 18+
              "P2_002N", # Hispanic Total 
              "P1_001N", # Total
              "P3_001N", # Total 18+
              "P1_009N", # Two or More
              "P3_009N") %>% # Two or More 18+
  unique(.)

races <- c("Total",
           "Hispanic",
           "White",
           "Black",
           "American Indian",
           "Asian",
           "Native Hawaiian",
           "Other",
           "Two or More")

dem_vars_dpi_2020 <- tibble(
  variable = c("P2_001N", # Total
               "P2_002N", # Hispanic
    # Following are not hispanic:
    sprintf("P2_00%iN", 5:9),
    sprintf("P2_0%iN", 10:11),
    # Following are same but 18+
    "P4_001N",
    "P4_002N",
    sprintf("P4_00%iN", 5:9),
    sprintf("P4_0%iN", 10:11)),
  race = c(races,
           paste0(races, " 18+"))
)
  

v_dpi <- get_decennial(geograph = "voting district",
                       variables = dem_vars_dpi_2020[["variable"]],
                       state = "WI",
                       county = "Milwaukee",
                       year = 2020,
                       geometry = FALSE, show_call = TRUE)

v <- get_decennial(geograph = "voting district",
                variables = dem_vars_2020,
                state = "WI",
                county = "Milwaukee",
                year = 2020,
                geometry = FALSE, show_call = TRUE)

vv_dpi <- v_dpi %>%
  filter(str_detect(NAME, "^Milwaukee - C")) %>%
  mutate(WARD = str_extract(NAME, "\\d{4}"),
         WARD = str_remove_all(WARD, "^0*"))
  
  
vv <- v %>%
  filter(str_detect(NAME, "^Milwaukee - C")) %>%
  mutate(WARD = str_extract(NAME, "\\d{4}"),
         WARD = str_remove_all(WARD, "^0*"))

v_wards_dpi <- left_join(wards, vv_dpi) %>%
  select(WARD, variable:geometry) %>%
  left_join(., ward_ald_sbd %>% mutate(Ward = as.character(Ward)), by = c("WARD" = "Ward")) %>%
  left_join(., dem_vars_dpi_2020, by = c("variable"))

v_wards <- left_join(wards, vv) %>%
  select(WARD, variable:geometry) %>%
  left_join(., ward_ald_sbd %>% mutate(Ward = as.character(Ward)), by = c("WARD" = "Ward")) %>%
  left_join(., any_race_2020, by = c("variable" = "name"))

names(v_wards) <- str_to_lower(names(v_wards)) %>%
  str_replace_all(., " ", "_")

names(v_wards_dpi) <- str_to_lower(names(v_wards_dpi)) %>%
  str_replace_all(., " ", "_")

  
ward_pop_2020 <- v_wards_dpi %>%
  group_by(ward,
           race,
           school_board_district,
           aldermanic_district) %>%
  summarise(pop = sum(value)) %>%
  ungroup() %>%
  mutate(ward = str_pad(ward, width = 4, side = "left", pad = 0)) 
  
saveRDS(ward_pop_2020, "data/ward_pop_2020.rds")

ward_pop_comparison <- left_join(ward_pop_2020, ward_pop_2010, suffix = c("_2020", "_2010"), by = c("ward", "race")) %>%
  mutate(change = pop_2020 - pop_2010,
         change_group = case_when(change > 400 ~ "Added 400+",
                                  change > 300 ~ "Added 300-400",
                                  change > 200 ~ "Added 200-300",
                                  change > 100 ~ "Added 100-200",
                                  change > 0 ~ "Added 0-100",
                                  change > -100 ~ "Lost 0-100",
                                  change > -200 ~ "Lost 100-200",
                                  change > -300 ~ "Lost 200-300",
                                  change > -400 ~ "Lost 300-400",
                                  change < -400 ~ "Lost 400+",
                                  TRUE ~ "ERROR"),
         change_group = factor(change_group, levels = c("Added 400+",
                                                        "Added 300-400",
                                                        "Added 200-300",
                                                        "Added 100-200",
                                                        "Added 0-100",
                                                        "Lost 0-100",
                                                        "Lost 100-200",
                                                        "Lost 200-300",
                                                        "Lost 300-400",
                                                        "Lost 400+")))

saveRDS(ward_pop_comparison, "data/ward_pop_comparison.rds")

t <- ward_pop_comparison %>%
  as_tibble() %>%
  filter(!str_detect(race, "Total")) %>%
  group_by(school_board_district, race) %>%
  summarise(sbd_pop_2020 = sum(pop_2020),
            sbd_pop_2010 = sum(pop_2010)) %>%
  ungroup() %>%
  group_by(school_board_district, over_18 = str_detect(race, "18")) %>%
  mutate(per_of_sbd_2020 = sbd_pop_2020 / sum(sbd_pop_2020),
         per_of_sbd_2010 = sbd_pop_2010 / sum(sbd_pop_2010))

write_csv(t, "data/sbd_demo_pop.csv")

ward_pop_comparison %>%
  filter(str_detect(race, "Total")) %>%
  ggplot(aes(fill = change_group)) +
  geom_sf(color = "white") +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  theme_void(base_family = "serif") +
  facet_wrap(~ race)



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


