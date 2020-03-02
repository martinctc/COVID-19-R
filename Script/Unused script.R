## Packed Circle Map

library(circlepackeR) # devtools::install_github("jeromefroe/circlepackeR")
library(data.tree)
library(treemap)

data("GNI2014") # from {treemap}

df_confirmed %>%
  select(-`Province/State`, -Lat, -Long) %>%
  group_by(`Country/Region`) %>%
  summarise_all(~sum(., na.rm = TRUE)) %>%
  gather(Date, Confirmed, -`Country/Region`) %>%
  group_by(`Country/Region`) %>%
  summarise(Confirmed = max(Confirmed, na.rm = TRUE)) %>%
  left_join(select(GNI2014, country, continent),
            by = c("Country/Region" = "country")) %>%
  mutate(pathString = paste(continent, `Country/Region`, sep = "/")) %>%
  as.Node() -> node_confirmed

circlepackeR(node_confirmed,
             size = "Confirmed",
             color_min = "hsl(56,80%,80%)",
             color_max = "hsl(341,30%,40%)") %>%
  print()