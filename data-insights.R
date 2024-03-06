# COMM_ST 395: Exploring potential data insights

# load package(s)
library(tidyverse)
library(ggcorrplot)
library(scales)
library(ggtext)
library(forcats)

# read in dataset
superbowl_ads <- read_csv("data/superbowl-ads.csv")

# stacked bar chart
superbowl_pivot_longer <- superbowl_ads |> 
  pivot_longer(cols = funny:use_sex,
               names_to = "ad_characteristic",
               values_to = "count") |> 
  filter(count == "TRUE")

# # overall stat percentages
# superbowl_pivot_longer |> 
#   group_by(ad_characteristic) |> 
#   summarize

superbowl_stacked_bar <- superbowl_pivot_longer |> 
  ggplot(aes(x = brand, fill = ad_characteristic)) +
  geom_bar(stat = "count", position = "fill") +
  theme_minimal() +
  labs(
    title = "Ad Strategies used by the Top 10 Brands in the Super Bowl",
    x = "Brand",
    y = "Characteristic Usage",
    fill = "Advertisement\nCharacteristic"
  ) + 
  scale_fill_manual(values = c("#66c2a5", "gray91", "gray81",
                               "#e78ac3", "gray71",
                               "#ffd92f", "gray66"),
                    labels = c("Animals", "Celebrity", "Danger",
                               "Funny", "Patriotic",
                               "Shows Product Quickly", "Uses Sex")) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    plot.title = element_textbox_simple(face = "bold", size = 15, margin = margin(10, 0, 10, 0)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, vjust = 1.25),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())

ggsave("figures/insights/sb_stacked_barchart.png", plot = superbowl_stacked_bar,
       units = "in",
       width = 9,
       height = 9)

# correlation matrix
superbowl_binary_var <- superbowl_ads |> 
  mutate(funny = as.numeric(funny),
         show_product_quickly = as.numeric(show_product_quickly),
         patriotic = as.numeric(patriotic),
         celebrity = as.numeric(celebrity),
         danger = as.numeric(danger),
         animals = as.numeric(animals),
         use_sex = as.numeric(use_sex)
  )

superbowl_binary_only <- superbowl_dummy_var |> 
  select(funny, show_product_quickly, patriotic,
         celebrity, danger, animals, use_sex)

superbowl_cor <- cor(superbowl_binary_only)

sb_corrplot <- ggcorrplot(superbowl_cor, show.diag = FALSE, type = "lower",
                          lab = TRUE, lab_size = 2)

ggsave("figures/insights/sb_corrplot.png", plot = sb_corrplot)

# trends of ads in the beer industry 

superbowl_beer <- superbowl_binary_var |> 
  filter(brand == "Bud Light" | brand == "Budweiser") |>
  select(-superbowl_ads_dot_com_url, -youtube_url) |> 
  pivot_longer(cols = funny:use_sex,
               names_to = "ad_characteristic",
               values_to = "count") |> 
  group_by(year, ad_characteristic) |> 
  summarize(count = sum(count)) 

superbowl_beer_year_count <- superbowl_binary_var |> 
  filter(brand == "Bud Light" | brand == "Budweiser") |>
  select(-superbowl_ads_dot_com_url, -youtube_url) |> 
  pivot_longer(cols = funny:use_sex,
               names_to = "ad_characteristic",
               values_to = "count") |> 
  group_by(year) |> 
  summarize(year_count = sum(count))

superbowl_beer_merge <- merge(superbowl_beer, superbowl_beer_year_count,
                              by = "year", all = TRUE)

superbowl_beer_merge <- superbowl_beer_merge |> 
  mutate(normalized_count = count / year_count)

# trends in car industry 

superbowl_car <- superbowl_binary_var |> 
  mutate(brand = case_when(brand == "Hynudai" ~ "Hyundai",
                           brand == "Toyota" ~ "Toyota",
                           brand == "Kia" ~ "Kia")) |>  
  filter(brand == "Hyundai" | brand == "Toyota" | brand == "Kia") |>
  select(-superbowl_ads_dot_com_url, -youtube_url) |> 
  pivot_longer(cols = funny:use_sex,
               names_to = "ad_characteristic",
               values_to = "count") |> 
  group_by(year, ad_characteristic) |> 
  summarize(count = sum(count)) 

# superbowl_binary_var |> 
#   mutate(brand = case_when(brand == "Hynudai" ~ "Hyundai",
#                            brand == "Toyota" ~ "Toyota",
#                            brand == "Kia" ~ "Kia")) |>  
#   filter(brand == "Hyundai" | brand == "Toyota" | brand == "Kia") |>
#   distinct(brand)

superbowl_car_year_count <- superbowl_binary_var |> 
  mutate(brand = case_when(brand == "Hynudai" ~ "Hyundai",
                           brand == "Toyota" ~ "Toyota",
                           brand == "Kia" ~ "Kia")) |>  
  filter(brand == "Hyundai" | brand == "Toyota" | brand == "Kia") |>
  select(-superbowl_ads_dot_com_url, -youtube_url) |> 
  pivot_longer(cols = funny:use_sex,
               names_to = "ad_characteristic",
               values_to = "count") |> 
  group_by(year) |> 
  summarize(year_count = sum(count))

superbowl_car_merge <- merge(superbowl_car, superbowl_car_year_count,
                             by = "year", all = TRUE)

superbowl_car_merge <- superbowl_car_merge |> 
  mutate(normalized_count = count / year_count)

# focusing only on animals, funny, and show product quickly

ad_characteristics <- c("funny",
                        "show_product_quickly",
                        # "patriotic",
                        # "celebrity",
                        # "danger",
                        "animals"
                        # "use_sex"
)
ad_titles <- c(funny = "Advertisement Tried to be Funny",
               show_product_quickly = "Product was Advertised Within the First 10 Seconds",
               # patriotic = "Advertisement Was Patriotic",
               # celebrity = "Advertisement Featured a Celebrity",
               # danger = "Advertisement Involved Danger",
               animals = "Advertisement Included Animals"
               # use_sex = "Advertisement Used Sex to Sell Its Product"
)

ad_colors <- c(funny = "#e78ac3",
               show_product_quickly = "#ffd92f",
               animals = "#66c2a5"
)

# normalize count for line trends
# line trends across both industries 

# keep color the same as it was in the stacked bar chart
# and keep it general across all industries

# beer industry
for (ad in ad_characteristics) {
  
  beer_plot <- superbowl_beer_merge |> 
    filter(ad_characteristic == ad) |> 
    ggplot(aes(year, normalized_count)) +
    geom_point(color = ad_colors[ad]) +
    geom_line(color = ad_colors[ad], linewidth = 1.25) +
    labs(
      title = paste0("Beer Industry Trends: " , ad_titles[ad]),
      x = "Year",
      y = NULL
    ) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    theme_minimal()
  
  ggsave(filename = paste0("figures/insights/beer_industry/", ad, "_in_beer_industry_graph.png"),
         plot = beer_plot,
         units = "in",
         width = 10,
         height = 10)
}

# car industry

for (ad in ad_characteristics) {
  
  car_plot <- superbowl_car_merge |> 
    filter(ad_characteristic == ad) |> 
    ggplot(aes(year, normalized_count)) +
    geom_point(color = ad_colors[ad]) +
    geom_line(color = ad_colors[ad], linewidth = 1.25) +
    labs(
      title = paste0("Automobile Industry Trends: " , ad_titles[ad]),
      x = "Year",
      y = NULL
    ) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    theme_minimal()
  
  ggsave(filename = paste0("figures/insights/car_industry/", ad, "_in_car_industry_graph.png"),
         plot = car_plot,
         units = "in",
         width = 10,
         height = 10)
  
}

# across all brands

superbowl_all <- superbowl_binary_var |> 
  select(-superbowl_ads_dot_com_url, -youtube_url) |> 
  pivot_longer(cols = funny:use_sex,
               names_to = "ad_characteristic",
               values_to = "count") |> 
  group_by(year, ad_characteristic) |> 
  summarize(count = sum(count)) 

superbowl_all_year_count <- superbowl_binary_var |> 
  select(-superbowl_ads_dot_com_url, -youtube_url) |> 
  pivot_longer(cols = funny:use_sex,
               names_to = "ad_characteristic",
               values_to = "count") |> 
  group_by(year) |> 
  summarize(year_count = sum(count))

superbowl_all_merge <- merge(superbowl_all, superbowl_all_year_count,
                             by = "year", all = TRUE)

superbowl_all_merge <- superbowl_all_merge |> 
  mutate(normalized_count = count / year_count)

for (ad in ad_characteristics) {
  
  all_brands_plot <- superbowl_all_merge |> 
    filter(ad_characteristic == ad) |> 
    ggplot(aes(year, normalized_count)) +
    geom_point(color = ad_colors[ad]) +
    geom_line(color = ad_colors[ad], linewidth = 1.25) +
    labs(
      title = paste0("Across All Brands: " , ad_titles[ad]),
      x = "Year",
      y = NULL
    ) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  ggsave(filename = paste0("figures/insights/all_brands/", ad, "_across_all_brands_graph.png"),
         plot = all_brands_plot,
         units = "in",
         width = 10,
         height = 10)
  
}

# all three lines on same graph
all_lines_on_same_graph <- superbowl_all_merge |> 
  filter(ad_characteristic == "funny" | ad_characteristic == "show_product_quickly" | ad_characteristic == "animals") |> 
  mutate(ad_characteristic = factor(ad_characteristic, levels = c("show_product_quickly",
                                                                  "funny",
                                                                  "animals"))) |> 
  ggplot(aes(x = year, y = normalized_count, group = ad_characteristic, color = ad_characteristic)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c(show_product_quickly = "#ffd92f",
                                funny = "#e78ac3",
                                animals = "#66c2a5"),
                     labels = c("Showed Product Quickly",
                                "Featured Humor",
                                "Displayed Animals")
  ) +
  labs(
    title = "Across All Brands: Trends of Humor, Animals, and Quick Displayment of Product",
    x = "Year",
    y = NULL,
    color = "Ad Characteristic"
  ) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent) +
  theme_minimal()

ggsave(filename = "figures/insights/all_brands/all_lines_on_same_graph.png",
       plot = all_lines_on_same_graph)

# zoomed in on 2017

line_graph_2017_zoom <- superbowl_all_merge |> 
  filter(ad_characteristic == "funny" | ad_characteristic == "show_product_quickly" | ad_characteristic == "animals") |>
  mutate(ad_characteristic = factor(ad_characteristic, levels = c("show_product_quickly",
                                                                  "funny",
                                                                  "animals"))) |> 
  ggplot(aes(x = year, y = normalized_count, group = ad_characteristic, color = ad_characteristic)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c(show_product_quickly = "#ffd92f",
                                funny = "#e78ac3",
                                animals = "#66c2a5"),
                     labels = c("Showed Product Quickly",
                                "Featured Humor",
                                "Displayed Animals")) +
  labs(
    title = "Across All Brands: Trends of Humor, Animals, and Quick Displayment of Product",
    x = "Year",
    y = NULL,
    color = "Ad Characteristic"
  ) +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent) +
  coord_cartesian(xlim = c(2016, 2018)) +
  theme_minimal()

ggsave(filename = "figures/insights/all_brands/2017_zoom.png",
       plot = line_graph_2017_zoom)


# look at what years are missing data for cars

# no data on years 2000-2007 and 2015
superbowl_binary_var |> 
  mutate(brand = case_when(brand == "Hynudai" ~ "Hyundai",
                           brand == "Toyota" ~ "Toyota",
                           brand == "Kia" ~ "Kia")) |>  
  filter(brand == "Hyundai") |>
  distinct(year)

# only data on years 2004, 2007, 2012, 2014, 2015, 2016, 2018, 2019, 2020 
superbowl_binary_var |> 
  mutate(brand = case_when(brand == "Hynudai" ~ "Hyundai",
                           brand == "Toyota" ~ "Toyota",
                           brand == "Kia" ~ "Kia")) |>  
  filter(brand == "Toyota") |>
  distinct(year)

# no data on years 2000-2009
superbowl_binary_var |> 
  mutate(brand = case_when(brand == "Hynudai" ~ "Hyundai",
                           brand == "Toyota" ~ "Toyota",
                           brand == "Kia" ~ "Kia")) |>  
  filter(brand == "Kia") |>
  distinct(year)
