# 2010 Census Data

Sys.setenv(CENSUS_KEY = Sys.getenv("CENSUS_API_KEY"))

apis <- listCensusApis()

meta_2010 <- listCensusMetadata(name = "dec/pl", vintage = 2010)

# Variables for 2010 aren't the same as 2020

vars_2010 <- c(Total = "P001001",
               `Total 18+` = "P003001",
               White = "P001003",
               `White 18+` = "P003003",
               Black = "P001004",
               `Black 18+` = "P003004",
               Asian = "P001006",
               `Asian 18+` = "P003006",
               Hispanic = "P002002",
               `Hispanic 18+` = "P004002")

mke_pl_2010 <- getCensus(name = "dec/pl",
          vintage = 2010,
          region = "voting district:*", # Milwaukee FIPS
          regionin = "state:55+county:079", # WI FIPS
          vars = c("NAME", vars_2010)) %>%
  filter(str_detect(NAME, "^Milwaukee - C")) %>%
  mutate(ward = str_extract(NAME, "(?<=- C )\\d*")) %>%
  pivot_longer(cols = -c(1:5, ward), names_to = "variable", values_to = "value") %>%
  mutate(variable = if_else(variable %in% vars_2010, names(vars_2010)[match(variable, vars_2010)], "ERROR"))


clean_2010 <- mke_pl_2010 %>%
  select(ward,
         variable,
         "value_2010" = value)

both_decs <- left_join(v_wards, clean_2010)

both_decs %>%
  filter(str_detect(variable, "Black")) %>%
  mutate(change = value - value_2010,
         c = case_when(change < -200 ~ "Less Than -200",
                       change < -149 ~ "-150 to -200",
                       change < -99 ~ "-100 to -150",
                       change < -49 ~ "-50 to -100")) %>%
  ggplot() +
  geom_sf(aes(fill = value), size = .01, color = "white") +
  facet_wrap(~ variable) + 
  theme_void() +
  scale_fill_steps2()

both_decs %>%
  as_tibble() %>%
  filter(variable == "Total") %>%
  mutate(ward = as.numeric(ward)) %>%
  group_by(ward) %>%
  summarise(total = sum(value),
            total_2010 = sum(value_2010)) %>%
  ungroup() %>%
  summarise(total = sum(total),
            total_2010 = sum(total_2010, na.rm = TRUE))

mke_pl_2010 %>%
  filter(variable == "Total") %>%
  summarise(total = sum(value))
