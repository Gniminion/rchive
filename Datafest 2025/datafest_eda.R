library(dplyr)
library(ggplot2)

dfl <- read.csv('Leases.csv', header=FALSE, skip=1)
dfm <- read.csv('Major Market Occupancy Data-revised.csv', header=FALSE)
dfu <- read.csv('Unemployment.csv', header=FALSE)

head(dfl)
colnames(dfl) <- c("year", "quarter", "monthsigned", "market", "building_name", 
                   "building_id", "address", "region", "city", "state", "zip", 
                   "internal_submarket", "internal_class", "leasedSF", "company_name", 
                   "internal_industry", "transaction_type", "internal_market_cluster", 
                   "costarID", "space_type", "CBD_suburban", "RBA", "available_space", 
                   "availability_proportion", "internal_class_rent", "overall_rent", 
                   "direct_available_space", "direct_availability_proportion", 
                   "direct_internal_class_rent", "direct_overall_rent", 
                   "sublet_available_space", "sublet_availability_proportion", 
                   "sublet_internal_class_rent", "sublet_overall_rent", "leasing")
unique(dfl$internal_industry)

filter_dfl <- dfl[
  dfl$internal_industry %in% c("Technology, Advertising, Media, and Information", "Legal Services", "Financial Services and Insurance"),
]

hm_df <- filter_dfl %>%
  group_by(state, internal_industry) %>%
  summarise(totalSF = sum(leasedSF))

ggplot(hm_df, aes(x = state, y = internal_industry, fill = totalSF)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Total SF") +
  labs(
    title = "Total Leased SF by State and Industry",
    x = "State",
    y = "Industry"
  )


filtered_dfl <- filter_dfl[
  dfl$state %in% c("CA")
]

write.csv(filtered_dfl, 'Filtered_Leases.csv', row.names = FALSE)
unique(filtered_dfl$market)
head(filtered_dfl)
nrow(filtered_dfl)

tech_ts <- filtered_dfl %>%
  filter(filtered_dfl$internal_industry %in% "Technology, Advertising, Media, and Information") %>%
  mutate(
    year_quarter = paste(year, quarter)
  ) %>%
  group_by(year, quarter, year_quarter) %>%
  summarise(totalSF = sum(leasedSF)) %>%
  arrange(year, quarter)

label_breaks <- tech_ts %>%
  filter(quarter %in% c("Q1", "Q3")) %>%
  pull(year_quarter)

ggplot(tech_ts, aes(x = year_quarter, y = totalSF, group = 1)) +
  geom_line(color = "darkblue") +
  geom_point(color = "blue", size = 2) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_discrete(breaks = label_breaks) +
  labs(
    title = "Tech Leasing Activity in California by Year and Quarter",
    x = "Year Quarter",
    y = "Total Leased SF"
  ) 

# ------

library(maps)

tech_sublet <- filtered_dfl %>%
  filter(
    internal_industry == "Technology, Advertising, Media, and Information",
    year == 2019
  ) %>%
  filter(!is.na(space_type)) %>%
  mutate(
    lease_type = ifelse(space_type == "Sublet", "Sublet", "Direct")
  )

head(tech_sublet)
tech_sublet <- tech_sublet %>%
  group_by(market, lease_type) %>%
  summarise(sf = sum(leasedSF)) %>%
  ungroup() %>%
  group_by(market) %>%
  mutate(
    direct = ifelse(lease_type == "Direct", sf, 0),
    sublet = ifelse(lease_type == "Sublet", sf, 0)
  ) %>%
  summarise(
    direct = sum(direct),
    sublet = sum(sublet),
  ) %>%
  mutate(
    totalSF = direct + sublet,
    sublet_share = ifelse(totalSF > 0, sublet / totalSF * 100, NA)
  )

tech_sublet

market_coords <- data.frame(
  market = c("Los Angeles", "Orange County", "San Diego", "San Francisco", "South Bay/San Jose"),
  lat = c(34.05, 33.71, 32.72, 37.77, 37.34),
  lon = c(-118.24, -117.83, -117.16, -122.42, -121.89),
  sublet_total = c(8.33, 4.23, 12.2, 28.7, 28.5),
  sublet_2019 = c(3.6, 1.89, 1.2, 24.4, 21.2),
  sublet_2024 = c(5.26, 7.85, 5.65, 31.4, 51.7)
)

california_map <- map_data("state") %>% filter(region == "california")

ggplot() +
  geom_polygon(data = california_map, aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white") +
  geom_point(data = market_coords, aes(x = lon, y = lat, color = sublet_2019),
             size = 10, alpha = 1) +
  geom_text(data = market_coords, aes(x = lon, y = lat, label = market),
            hjust = -0.2, size = 6, type = "bold") +
  scale_color_gradientn(colours = c("lightblue", high = "darkred"),
                       limit = c(0, 60), name = "% Sublet") +
  coord_fixed(1.3) +
  theme_void() +
  labs(title = "Tech Sublet Proportions by Market in California") 

