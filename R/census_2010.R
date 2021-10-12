# Thanks to John Johnson for sharing his code and
# crosswalk data that makes this analysis possible.

library(tidyverse)
library(tidycensus)


# handy reference table to look up variable names
census_vars_2010 <- load_variables(2010, "sf1", T)

# race variables 

race_filters <- c("White",
                  "Black",
                  "Asian")

races_18_plus <- map(race_filters, function(x) {
  t <- census_vars_2010 %>%
    filter(str_detect(name, "^P010") & str_detect(label, x)) %>%
    .[[1]]
}) %>%
  set_names(., race_filters)

any_race <-  map_df(race_filters, function(x) {
  census_vars_2010 %>%
    filter(str_detect(name, "^P010") & str_detect(label, x))%>%
    mutate(race = paste0(x, " 18+")) %>%
    select(name, race)
}) %>%
  bind_rows(
    tibble(
      race = c("Total",
               "Total 18+",
               "White",
               "Black",
               "Asian",
               "Hispanic",
               "Hispanic 18+"),
      name = c("P005001",
               "P010001",
               "P006002",
               "P006003",
               "P006005",
               "P007009",
               "P011002")
    )
  )


races <- c(Total = "P005001",
           `Total 18+` = "P010001",
           # Total Races Tallied
           White = "P006002",
           Black = "P006003",
           Asian = "P006005",
           Hispanic = "P007009",
           `Hispanic 18+` = "P011002",
           # 18+ Doesn't have same Total Races Tallied
           # need to pull each variable
           races_18_plus$White,
           races_18_plus$Black,
           races_18_plus$Asian) %>%
  unique

races <- c("Total",
           "Hispanic",
            "White",
            "Black",
            "American Indian",
            "Asian",
            "Native Hawaiian",
            "Other",
            "Two or More")

dem_vars_2010 <- tibble(
  variable = c("P009001",
               "P009002",
               sprintf("P00900%i", 5:9),
               sprintf("P0090%i", 10:11),
               "P011001",
               "P011002",
               sprintf("P01100%i", 5:9),
               sprintf("P0110%i", 10:11)),
  race = c(races,
           paste0(races, " 18+"))
)


# this is just the total population of the block
block_pop_2010 <- get_decennial("block", variable = dem_vars_2010[["variable"]], state = "WI", county = "079", year = 2010)

bp_2010 <- left_join(block_pop_2010, dem_vars_2010, by = "variable")

# change the path as needed
block_to_ward_2010 <- read_rds("data/BlocksToWards_CombinedMethod.rds") %>%
  ungroup() %>%
  mutate(ward = str_pad(ward, 4, "left", "0"))

# join with the crosswalk, multiply pop by the allocation factor, and summarize
ward_pop_2010 <- bp_2010 %>%
  select(block = GEOID, value, variable, race) %>%
  group_by(block, race) %>%
  summarise(pop = sum(value)) %>%
  ungroup() %>%
  inner_join(., block_to_ward_2010) %>%
  mutate(adj_pop = pop * prop_of_block) %>%
  group_by(ward, race) %>%
  summarise(pop = sum(adj_pop)) %>%
  ungroup() %>%
  mutate(year = 2010)

saveRDS(ward_pop_2010, "data/ward_pop_2010.rds")

