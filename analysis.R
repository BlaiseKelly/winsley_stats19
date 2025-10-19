# this script downloads the STATS19 data from the DfT and creates some tables and plots

library(sf)
library(mapview)
library(dplyr)
library(stats19)
library(geographr)
library(reshape2)
library(ggplot2)
library(readODS)
library(cols4all)
library(gt)
library(waffle)
library(stringr)
library(osmactive)
library(clock)
library(tmap)
library(basemaps)

ace# create directory for the plots
dir.create("plots/")
dir.create("data/")

#crashes <- get_stats19("2004", type = "collision")

#casualties <- get_stats19("2004", type = "casualty")

load("data/Winsley_crashes.RData")

# get parish shapes for UK
pc <- st_read("https://files.planning.data.gov.uk/dataset/parish.geojson")

# pick out Winsley and convert to metres coordinate system
pc_winsley <- filter(pc, name == "Winsley") |>
  st_transform(27700)

#bm <- basemaps::basemap_raster(ext=pc_winsley, map_service = "carto", map_type = "light")
bm <- basemaps::basemap_raster(ext=pc_winsley, map_service = "osm", map_type = "topographic")

# plot the boundary
tm1 <- tm_shape(bm)+
  tm_rgb()+
  tm_scalebar()+
  tm_shape(pc_winsley)+
  tm_polygons(fill_alpha = 0, col = "#ff7733", col_alpha = 1, lwd = 10)

tmap_save(tm1, "plots/winsley.png", width = 5000, height = 5000)

# get road network for local area
winsley_osm <- osmactive::get_travel_network(place = "Winsley", boundary = pc_winsley)

# pick out the driving roads stats19 applies to
winsley_d <- get_driving_network(winsley_osm) |>
  st_transform(27700)

ons_cost <- read_ods("https://assets.publishing.service.gov.uk/media/68d421cc275fc9339a248c8e/ras4001.ods", sheet = "Average_value")

# Define the URL and a temporary file path
url <- "https://assets.publishing.service.gov.uk/media/68d421cc275fc9339a248c8e/ras4001.ods"
tmpfile <- tempfile(fileext = ".ods")

# Download the file
download.file(url, destfile = tmpfile, mode = "wb")

# Now read the ODS file from the local path
ons_cost <- read_ods(tmpfile, sheet = "Average_value", skip = 3)

# adjust the names which are badly formatted
ons_cost_form <- ons_cost[-1,1:5]

# replace with manual names
names(ons_cost_form) <- c("collision_data_year","price_year","severity","cost_per_casualty","cost_per_collision")

# make the crash data sf to intersect with Winsley geometry
cra_winsley <- format_sf(crashes) |>
  st_join(pc_winsley) |>
  filter(name == "Winsley")

base_year <- 2010

# fil
cas_winsley <- casualties |>
  filter(collision_index %in% cra_winsley$collision_index)|>
  filter(collision_year >= base_year) |>
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) |>
  select(collision_index, casualty_type, pedestrian_location, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
  group_by(collision_index) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE))

#save(casualties, cra_winsley, file = "data/Winsley_crashes.RData")

cra_winsley_2010_dat <- cra_winsley |>
  filter(collision_index %in% cas_winsley$collision_index) |>
  st_set_geometry(NULL) |>
  select(collision_index, collision_year, speed_limit, time, day_of_week,first_road_number, junction_detail,
         first_road_class, second_road_number, second_road_class, light_conditions,weather_conditions,datetime,
         road_surface_conditions)

cas_rates <- casualties |>
  filter(collision_year >= base_year & collision_index %in% cas_winsley$collision_index) |>
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) |>  # add a column for fatal tally to enable same method to be used for serious and slight
  group_by(collision_index, collision_year) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |>
  group_by(collision_year) |>
  summarise(Fatal = sum(Fatal),
            Serious = sum(Serious,na.rm = TRUE),
            Slight = sum(Slight,na.rm = TRUE))

# baseline values for index plot
bm_vals <- cas_rates |> filter(collision_year == base_year)

# calaute table of indexes
rates <- cas_rates %>%
  transmute(year = collision_year,
            Serious = Serious/bm_vals$Serious*100,
         Slight = Slight/bm_vals$Slight*100)

chart_2 <- melt(rates, "year")

cols <- rev(c("#ff7733", "#1de9b6"))
cust_theme <- theme(panel.grid.major = element_line(size = 2))
# put the elements in a list
dft_theme <- list(cust_theme, scale_color_manual(values = cols))

chart_2 %>%
  ggplot(aes(year, value, color = variable)) +
  geom_line(size = 2, alpha = .8) +
  dft_theme+
  theme(panel.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=100, linetype='dotted', col = 'black')+
  ggtitle(paste0("Chart: Index of casualties by severity, Winsley: 2010 - 2024 (Index 2010=100)")) +
  scale_x_continuous(name = NULL,
                     breaks = seq(2004, 2024, by = 1)  # Add more tick marks
  ) +
  ylab("index")+
  labs(caption = "Source: Stats19")+
  theme(panel.border = element_blank())

ggsave("plots/index.png")

cas_dat <- casualties |>
  filter(collision_index %in% cas_winsley$collision_index) |>
  select(collision_index, age_of_casualty, casualty_imd_decile, casualty_reference)

cas_age <- casualties |>
  filter(collision_index %in% cas_winsley$collision_index) |>
  mutate(age_band = cut(as.numeric(age_of_casualty), breaks=c(0,11,15,19,24,29,39,49,59,69,100),labels=c("0-11","12-15","16-19","20-24","25-29","30-39","40-49","50-59","60-69","70+"))) |>
  group_by(age_band) %>%
  summarise(Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE))

#sum(cas_age$Serious)+sum(cas_age$Slight)

cas_sex <- casualties |>
  filter(collision_index %in% cas_winsley$collision_index) |>
  mutate(age_band = cut(as.numeric(age_of_casualty), breaks=c(0,11,15,19,24,29,39,49,59,69,100),labels=c("0-11","12-15","16-19","20-24","25-29","30-39","40-49","50-59","60-69","70+"))) |>
  group_by(sex_of_casualty) %>%
  summarise(Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE))

sac_all <- casualties |>
  filter(collision_index %in% cas_winsley$collision_index) |>
  mutate(age_band = cut(as.numeric(age_of_casualty), breaks=c(0,11,15,19,24,29,39,49,59,69,100),labels=c("0-11","12-15","16-19","20-24","25-29","30-39","40-49","50-59","60-69","70+"))) |>
  group_by(sex_of_casualty, age_band) %>%
  summarise(Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |>
  filter(!is.na(age_band)) |>
  rowwise() |>
  mutate(All = sum(Slight,Serious))

# add pc_ksi for only Male and Female
sac_all <- sac_all |> ungroup() |> mutate(pc_ksi = (All/sum(All))*100) |> filter(sex_of_casualty %in% c("Male", "Female"))

# Define colours and theme
cols <- rev(c("#001a70", "#ff7733"))
cust_theme <- theme(panel.grid.major = element_line(size = 2))
dft_theme <- list(cust_theme, scale_fill_manual(values = cols))  # use fill, not color

ggplot(sac_all, aes(x = age_band, y = pc_ksi, fill = sex_of_casualty)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0(round(pc_ksi),"%")),  # Round values to 1 decimal place
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  ggtitle(paste0("Chart: Percentage of casualties, by sex and age, Winsley: 2010 to 2024")) +
  dft_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  ylab(NULL)+
  xlab(NULL)+
  labs(caption = "Source: Stats19")

ggsave("plots/sex_age.png")

cra_winsley_2010 <- cra_winsley |>
  filter(collision_year >= 2010) |>
  st_set_geometry(NULL) |>
  select(collision_index, collision_year) |>
  left_join(cas_winsley, by = "collision_index") |>
  melt(c("collision_index", "collision_year")) |>
  filter(value > 0)

cra_other <- cra_winsley_2010 |>
  left_join(cra_winsley_2010_dat, by = "collision_index")

# Define colours and theme
cols <- rev(c("#1de9b6", "#006853"))
cust_theme <- theme(panel.grid.major = element_line(size = 2))
dft_theme <- list(cust_theme, scale_fill_manual(values = cols))  # use fill, not color

# year
year_count <- cas_rates |>
  melt("collision_year") |>
  filter(!variable == "Fatal") |>
  mutate(collision_year = as.character(collision_year))

ggplot(year_count, aes(x = collision_year, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0(round(value),"")),  # Round values to 1 decimal place
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  ggtitle(paste0("Chart: Total casualties by severity and year, Winsley: 2010 to 2024")) +
  dft_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  ylab(NULL)+
  xlab(NULL)+
  labs(caption = "Source: Stats19")

ggsave("plots/year_totals.png")

# road surface
casualty_type <- cra_other |>
  group_by(road_surface_conditions, variable) |>
  summarise(casualties = sum(value)) |>
  ungroup() |>
  mutate(pc_ksi = (casualties/sum(casualties))*100)

ggplot(road_surface, aes(x = road_surface_conditions, y = pc_ksi, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0(round(pc_ksi),"%")),  # Round values to 1 decimal place
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  ggtitle(paste0("Chart: Percentage of casualties, by road surface condition, Winsley: 2010 to 2024")) +
  dft_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  ylab(NULL)+
  xlab(NULL)+
  labs(caption = "Source: Stats19")

ggsave("plots/road_surface.png")

# road surface
road_surface <- cra_other |>
  group_by(road_surface_conditions, variable) |>
  summarise(casualties = sum(value)) |>
  ungroup() |>
  mutate(pc_ksi = (casualties/sum(casualties))*100)

ggplot(road_surface, aes(x = road_surface_conditions, y = pc_ksi, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0(round(pc_ksi),"%")),  # Round values to 1 decimal place
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  ggtitle(paste0("Chart: Percentage of casualties, by road surface condition, Winsley: 2010 to 2024")) +
  dft_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  ylab(NULL)+
  xlab(NULL)+
  labs(caption = "Source: Stats19")

ggsave("plots/road_surface.png")

junction <- cra_other |>
  group_by(junction_detail, variable) |>
  summarise(casualties = sum(value))|>
  ungroup() |>
  mutate(pc_ksi = (casualties/sum(casualties))*100)

ggplot(junction, aes(x = junction_detail, y = pc_ksi, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0(round(pc_ksi),"%")),  # Round values to 1 decimal place
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  ggtitle(paste0("Chart: Percentage of casualties, by junction type, Winsley: 2010 to 2024")) +
  dft_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  ylab(NULL)+
  xlab(NULL)+
  labs(caption = "Source: Stats19")

ggsave("plots/junction_type.png")

speed <- cra_other |>
  group_by(speed_limit, variable) |>
  mutate(speed_limit = paste0(speed_limit, "mph")) |>
  summarise(casualties = sum(value))|>
  ungroup() |>
  mutate(pc_ksi = (casualties/sum(casualties))*100)

ggplot(speed, aes(x = speed_limit, y = pc_ksi, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0(round(pc_ksi),"%")),  # Round values to 1 decimal place
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  ggtitle(paste0("Chart: Percentage of casualties, by speed limit, Winsley: 2010 to 2024")) +
  dft_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  ylab(NULL)+
  xlab(NULL)+
  labs(caption = "Source: Stats19")

ggsave("plots/speed_limit.png")

light <- cra_other |>
  group_by(light_conditions, variable) |>
  summarise(casualties = sum(value))|>
  ungroup() |>
  mutate(pc_ksi = (casualties/sum(casualties))*100)

ggplot(light, aes(x = light_conditions, y = pc_ksi, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0(round(pc_ksi),"%")),  # Round values to 1 decimal place
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  ggtitle(paste0("Chart: Percentage of casualties, by lighting conditions, Winsley: 2010 to 2024")) +
  dft_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  ylab(NULL)+
  xlab(NULL)+
  labs(caption = "Source: Stats19")

ggsave("plots/lighting.png")


weather <- cra_other |>
  group_by(weather_conditions, variable) |>
  summarise(casualties = sum(value))|>
  ungroup() |>
  mutate(pc_ksi = (casualties/sum(casualties))*100)

ggplot(weather, aes(x = weather_conditions, y = pc_ksi, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0(round(pc_ksi),"%")),  # Round values to 1 decimal place
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  ggtitle(paste0("Chart: Percentage of casualties, by speed limit, Winsley: 2010 to 2024")) +
  dft_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  ylab(NULL)+
  xlab(NULL)+
  labs(caption = "Source: Stats19")

ggsave("plots/weather_conditions.png")

dow <- cra_other |>
  group_by(day_of_week, variable) |>
  summarise(casualties = sum(value))

# crash_time <- cas_winsley |>
#   left_join(cra_L5Y, by = "collision_index") |> # join crashes as number of vehicles is included, quicker than calculating from veh table
#   select(datetime, Fatal_Pedestrian, Serious_Pedestrian) |>
#   mutate(#collision_hr = lubridate::hour(datetime),
#     dow = clock::date_weekday_factor(datetime, abbreviate = FALSE),
#     collision_hr = get_hour(datetime),
#     KSI = sum(Fatal_Pedestrian, Serious_Pedestrian)) |>
#   mutate(dow = case_when(dow == "Monday" ~  "Monday to Friday",
#                          dow == "Tuesday" ~  "Monday to Friday",
#                          dow == "Wednesday" ~  "Monday to Friday",
#                          dow == "Thursday" ~  "Monday to Friday",
#                          dow == "Friday" ~  "Monday to Friday",
#                          dow == "Saturday" ~  "Saturday",
#                          dow == "Sunday" ~  "Sunday")) |>
#   #mutate(dow = case_when(dow > 1 & dow < 7 ~  "Monday to Friday", dow == 7 ~ "Saturday", dow == 1 ~ "Sunday")) |>
#   group_by(collision_hr, dow) |>
#   summarise(KSI = sum(KSI)) |>
#   mutate(KSI = if_else(dow == "Monday to Friday", KSI/5, KSI))
#
# MF_peak <- crash_time |> filter(dow == "Monday to Friday") |> arrange(desc(KSI)) |> mutate(hr = str_sub(gsub(" ","", tolower(format(strptime(collision_hr, format = "%H"), "%I %p"))),2))
#
# SS_peak <- crash_time |> filter(dow %in% c("Saturday", "Sunday")) |> group_by(collision_hr) |> summarise(KSI = sum(KSI)) |> arrange(desc(KSI)) |> mutate(hr = str_sub(gsub(" ","", tolower(format(strptime(collision_hr, format = "%H"), "%I %p"))),2))
#
# # define the colour palette
# cols <- rev(c("#ff7733", "#1de9b6","#006853"))
# cust_theme <- theme(panel.grid.major = element_line(size = 2))
# # put the elements in a list
# dft_theme <- list(cust_theme, scale_color_manual(values = cols))
#
# crash_time %>%
#   ggplot(aes(collision_hr, KSI, color = dow)) +
#   geom_line(size = 2, alpha = .8) +
#   dft_theme+
#   theme(panel.background = element_blank(),
#         legend.position = "top", legend.title = element_blank()) +
#   scale_x_continuous(expand = c(0, 0)) +
#   ggtitle(paste0("Chart 4: Reported ", tolower(report_casualty), " KSIs by hour of day and day of week, GB: ", yr2calc-4, " to ", yr2calc)) +
#   ylab(NULL)+
#   labs(x = "Hour starting", caption = "Source: Stats19")

# costs
cwc <- cra_winsley_2010 |>
  mutate(collision_year = as.character(collision_year)) |>
  left_join(ons_cost_form, by = c("collision_year" = "collision_data_year", "variable" = "severity")) |>
  rowwise() |>
  mutate(casualty_cost = sum(value*as.numeric(gsub(",","", cost_per_casualty))),
          collision_cost = sum(value*as.numeric(gsub(",", "", cost_per_collision)))) |>
  group_by(collision_year, variable) |>
  summarise(total_casualties = round(sum(value),1),
            casualty_cost = round(sum(casualty_cost)),
            collision_cost = round(sum(collision_cost)))

cwc_cas_type <- cra_winsley_2010 |>
  mutate(collision_year = as.character(collision_year)) |>
  left_join(ons_cost_form, by = c("collision_year" = "collision_data_year", "variable" = "severity")) |>
  rowwise() |>
  mutate(casualty_cost = sum(value*as.numeric(gsub(",","", cost_per_casualty))),
         collision_cost = sum(value*as.numeric(gsub(",", "", cost_per_collision)))) |>
  group_by(collision_year, variable) |>
  summarise(total_casualties = round(sum(value),1),
            casualty_cost = round(sum(casualty_cost)),
            collision_cost = round(sum(collision_cost)))

cwc_tot <- cwc |>
  ungroup() |>
  rowwise() |>
  mutate(total = casualty_cost+collision_cost) |>
  mutate(casualty_cost = prettyNum(casualty_cost, big.mark = ",", scientific = FALSE),
         collision_cost = prettyNum(collision_cost, big.mark = ",", scientific = FALSE),
         total = prettyNum(total, big.mark = ",", scientific = FALSE))

cc_tot <- sum(as.numeric(gsub(",","", cwc_tot$total)))

# country table
t1 <- gt(cwc_tot,auto_align = TRUE) |>
  cols_width(collision_year ~px(60)) |>
  cols_label(collision_year = md("**Year**"),
             variable = md("**Severity**"),
             total_casualties = md("**Casualties**"),
             casualty_cost = md("**Casualty cost**"),
             collision_cost = md("**Collision cost**"),
             total = md("**Total**")) |>
  tab_footnote(md("**Source: DfT STATS19 and TAG**")) |>
  tab_header(
    title = md(paste0("**Number of reported road casualties and value of prevention by year, Winsley: 2010 to 2024**"))) |>
  tab_options(heading.align = "left",
              column_labels.border.top.style = "none",
              table.border.top.style = "none",
              column_labels.border.bottom.style = "none",
              column_labels.border.bottom.width = 1,
              column_labels.border.bottom.color = "black",
              table_body.border.top.style = "none",
              table_body.border.bottom.color = "white",
              heading.border.bottom.style = "none",
              table.border.bottom.style = "none",) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(columns = c(collision_year)),
      cells_body(columns = c(collision_year))
    )) |>
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body(columns = everything())
  )

gtsave(t1, "plots/annual_table.png")



cas_yr <- cra_winsley_2010 |>
  mutate(collision_year = as.character(collision_year)) |>
  group_by(collision_year) |>
  summarise(total_casualties = round(sum(value),1))

cwc_yr <- cra_winsley_2010 |>
  mutate(collision_year = as.character(collision_year)) |>
  left_join(ons_cost_form, by = c("collision_year" = "collision_data_year", "variable" = "severity")) |>
  rowwise() |>
  mutate(casualty_cost = sum(value*as.numeric(gsub(",","", cost_per_casualty))),
         collision_cost = sum(value*as.numeric(gsub(",", "", cost_per_collision)))) |>
  group_by(collision_year) |>
  summarise(casualty_cost = round(sum(casualty_cost)),
            collision_cost = round(sum(collision_cost)))



chart_0 <- cwc_yr |>
  melt(c("collision_year")) |>
  mutate(value = value/1000000,
         variable = gsub("_", " ", variable))

names(chart_0) <- c("year", "cost category", "cost")

#chart_0$cost <- as.numeric(chart_0$cost)



# Define colours and theme
#cols <- rev(c("#1de9b6", "#006853"))
#cols <- c4a("carto.pastel", n = NROW(unique(chart_0$`cost category`)))
cols <- rev(c("#ff7733", "#1de9b6"))
cust_theme <- theme(panel.grid.major = element_line(size = 2))
# put the elements in a list
dft_theme <- list(cust_theme, scale_color_manual(values = cols))

ggplot(chart_0, aes(x = year, y = cost, fill = `cost category`)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(
    aes(label = NA),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  ggtitle(paste0(
    "Chart: annual value of prevention of collisions in Winsley"
  ),subtitle = "Calculated using collision data from DfT STATS19 and cost data from TAG") +
  dft_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  ylab("Casualty and Collision cost (£ million)") +
  xlab(NULL) +
  labs(caption = "Source: Stats19 and ONS")

ggsave("plots/cc_bar.png")

cra_winsley_2010 <- cra_winsley |>
  filter(collision_year >= 2010)


bm <- basemaps::basemap_raster(ext=pc_winsley, map_service = "carto", map_type = "light")

tm1 <- tm_shape(bm)+
  tm_rgb()+
tm_shape(cra_winsley_2010)+
  tm_dots("collision_severity", size = 0.7)+
  tm_title("all reported collisions between 2010 and 2024")

tmap_save(tm1, "plots/winsley_dots.png")

cas_winsley_map <- casualties |>
  filter(collision_index %in% cra_winsley$collision_index)|>
  filter(collision_year >= 2010) |>
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) |>
  select(collision_index, casualty_type, pedestrian_location, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
  group_by(collision_index, casualty_type) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |>
  left_join(cra_winsley, by = "collision_index") |>
  select(casualty_type, Serious, Slight, geometry) |>
  st_as_sf()

pal <- data.frame(name = unique(cas_winsley_type$casualty_type), pal = c4a("brewer.accent", n = 8))

tm1 <- tm_shape(bm)+
  tm_rgb()+
  tm_shape(cas_winsley_map)+
  tm_bubbles(fill = "casualty_type", shape = "casualty_type", shape.legend = tm_legend_combine("fill"), size = 1)+
  tm_title("Collision location with casualty type represented by shape and colour. Winsley: 2010 and 2024")

tmap_save(tm1, "plots/cas_type_map.png", width = 9000, height = 7000, dpi = 600)

tm2 <- tm_shape(bm)+
  tm_rgb()+
  tm_shape(cas_winsley_map)+
  tm_bubbles(fill = "casualty_type",
             shape = "casualty_type",
             size = "Serious",
             shape.legend = tm_legend_combine("fill"),
             size.legend = tm_legend(title = "Severity")) +
  tm_title("Collision location with casualty type represented by shape and colour and severity represented by size. Winsley: 2010 and 2024")

tmap_save(tm2, "plots/cas_type_sev_map.png", width = 9500, height = 7000, dpi = 650)

cas_winsley_type <- casualties |>
  filter(collision_index %in% cra_winsley$collision_index)|>
  filter(collision_year >= 2010) |>
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0)) |>
  select(collision_index, casualty_type, pedestrian_location, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
  group_by(collision_index, casualty_type) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |>
  left_join(cra_winsley, by = "collision_index") |>
  select(casualty_type, Serious, Slight, geometry) |>
  st_as_sf()

vehicles_df <- data.frame(casualty_type = c("Car occupant", "Cyclist","Horse rider","Motorcycle 125cc and under rider or passenger",
                                            "Motorcycle 50cc and under rider or passenger","Motorcycle over 500cc rider or passenger",
                                            "Taxi/Private hire car occupant", "Van / Goods vehicle (3.5 tonnes mgw or under) occupant"),
                          short_name = c("Car occupant", "Cyclist","Horse rider","Motorcyclist 125cc",
                                         "Motorcyclist 50cc","Motorcyclist over 500cc",
                                         "Taxi occupant", "Van/Goods vehicle occupant"))
# road surface
casualty_type <- cas_winsley_map |>
  st_set_geometry(NULL) |>
  group_by(casualty_type) |>
  summarise(Serious = sum(Serious),
            Slight = sum(Slight)) |>
  melt("casualty_type") |>
  ungroup() |>
  mutate(pc_ksi = (value/sum(value))*100) |>
  left_join(vehicles_df, by = "casualty_type")

# Define colours and theme
cols <- rev(c("#1de9b6", "#006853"))
cust_theme <- theme(panel.grid.major = element_line(size = 2))
dft_theme <- list(cust_theme, scale_fill_manual(values = cols))  # use fill, not color

ggplot(casualty_type, aes(x = short_name, y = pc_ksi, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0(round(pc_ksi),"%")),  # Round values to 1 decimal place
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  ggtitle(paste0("Chart: Percentage of casualties, by casualty type, Winsley: 2010 to 2024")) +
  dft_theme +
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  ylab(NULL)+
  xlab(NULL)+
  labs(caption = "Source: Stats19")

ggsave("plots/casualty_type.png")




#download.file("https://assets.publishing.service.gov.uk/media/68d421cc275fc9339a248c8e/ras4001.ods", destfile = "data/ras4001.ods")


# find ones not included
#pn_in <- filter(cas_winsley_type, !collision_index %in% p_in$collision_index)

cas_winsley_type$osm_id <- winsley_d$osm_id[st_nearest_feature(cas_winsley_type,winsley_d)]

roads_cra_match <- function(osm_network_sf, crash_sf, crs){

  osm_network_sf <- st_transform(osm_network_sf, crs)
  crash_sf <- st_transform(crash_sf, crs)

  crash_sf$osm_id <- osm_network_sf$osm_id[st_nearest_feature(crash_sf,osm_network_sf)]

  return(crash_sf)

}



cas_winsley_osm <- cas_winsley_type |>
  st_set_geometry(NULL) |>
  group_by(osm_id) |>
  summarise(cas_rd = n())

rd_details <- data.frame(osm_id = c("4305512","4317206","4329918","4330011","5038056","5038062","5038063","22337995","35391259","109503140",
                                    "159153552","160483482","238677470","238677471","450896558","567182832","992606166","1389309908"),
                         name = c("B3108","Avoncliffe approach","B3108 Winsley Hill","Dane Rise","NA","NA","Blackberry Lane","Tyning Road","B3108 Winsley Hill","Bradford Road","Winsley Hill","NA","Bradford Road","B3108",
                                  "Bath Road","NA","Murhill Lane","Winsley Road"),
                         maxspeed = c("50mph", "60mph", "50mph", "20mph", "60mph", "60mph", "60mph", "30mph","40mph","30mph","40mph", "40mph", "30mph","50mph", "60mph", "60mph","30mph","40mph"))

winsley_d_cas <- winsley_d |>
  select(osm_id, name, maxspeed) |>
  left_join(cas_winsley_osm, by = "osm_id") |>
  filter(!is.na(cas_rd)) |>
  select(-name, -maxspeed) |>
  left_join(rd_details, by = "osm_id")

cas_speed <- winsley_d_cas |>
  st_set_geometry(NULL) |>
  group_by(maxspeed) |>
  summarise(casualties = sum(cas_rd))



winsley_d_cas$casualties <- winsley_d_cas$cas_rd

tm3 <- tm_shape(bm)+
  tm_rgb()+
tm_shape(winsley_d_cas) +
  tm_lines(col = "casualties", col.scale = tm_scale_continuous(
    n = 10,
    limits = c(0,10),
    values = "rainbow_bgyr_35_85_c73"
  ), lwd = 4)+
  tm_legend(title = "casualties", height = 60, frame = FALSE)+
  tm_title("Number of collisions by osm road link. Winsley: 2010 and 2024")

tmap_save(tm3, "plots/cas_osm_links.png", width = 7500, height = 7000, dpi = 800)

# define the colours for the plot
cols <- rev(c("#00ab3d", "#005bb2","#c81329"))
cust_theme <- theme(panel.grid.major = element_line(size = 2))
# put the elements in a list
dft_theme <- list(cust_theme, scale_color_manual(values = cols))


