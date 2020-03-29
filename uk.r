library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggsci)
library(readxl)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(rgeos)
library(broom)
library(gridExtra)

#------------------------------
# import the data
uk_case <- read_excel("uk_data.xlsx", sheet = "UK Cases")
uk_case$Date <- as.Date(uk_case$Date)

uk_death <- read_excel("uk_data.xlsx", sheet = "UK Deaths")
uk_death $Date <- as.Date(uk_death$Date)
uk_death <- uk_death %>% 
  select(-Deaths) %>% 
  gather(key = "place", value = "value", -Date)

uk_case_country <- read_excel("uk_data.xlsx", sheet = "Countries")
uk_case_country$Date <- as.Date(uk_case_country$Date)
uk_case_country <- uk_case_country %>% 
  gather(key = "place", value = "value", -Date)

uk_case_region <- read_excel("uk_data.xlsx", sheet = "NHS Regions")
uk_case_region$Date <- as.Date(uk_case_region$Date)
uk_case_region <- uk_case_region %>% 
  gather(key = "place", value = "value", -Date)

uk_case_la <- read_excel("uk_data.xlsx", sheet = "UTLAs")
shapefile <- readOGR(dsn="C:/Users/shanquan/Desktop/bounder", layer="Counties_and_Unitary_Authorities_December_2019_Boundaries_UK_BGC")
map_data <- broom::tidy(shapefile, region = "ctyua19cd")
map_data <- left_join(map_data, 
                      uk_case_la %>% select(id, `Area Name`, `43918`), 
                      by = "id")



ggplot(data = uk_death %>% filter(place != "UK")) + 
  geom_point(aes(x = Date, y = value, colour = place)) + 
  # geom_vline(xintercept = as.Date("2020-02-11"), linetype = 2) +
  # geom_smooth(data = mdat %>% filter(date >= as.Date("2020-02-11")), 
  #             aes(x = date, y = value, group = cat, colour = cat)) + 
  # geom_smooth(data = mdat %>% filter(date < as.Date("2020-02-11")), 
  #             aes(x = date, y = value, group = cat, colour = cat)) + 
  scale_colour_manual(values = pal_nejm()(17), name = "") + 
  scale_x_date(name = "Date", 
               date_breaks = "1 day", 
               date_labels = "%b %d") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(data = uk_case_country %>% filter(place != "UK")) + 
  geom_point(aes(x = Date, y = value, colour = place)) + 
  # geom_vline(xintercept = as.Date("2020-02-11"), linetype = 2) +
  # geom_smooth(data = mdat %>% filter(date >= as.Date("2020-02-11")), 
  #             aes(x = date, y = value, group = cat, colour = cat)) + 
  # geom_smooth(data = mdat %>% filter(date < as.Date("2020-02-11")), 
  #             aes(x = date, y = value, group = cat, colour = cat)) + 
  scale_colour_manual(values = pal_nejm()(17), name = "") + 
  scale_x_date(name = "Date", 
               date_breaks = "1 day", 
               date_labels = "%b %d") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

plot1 <- ggplot() + 
  geom_polygon(data = map_data, 
               aes(x = long, y = lat, group = group, fill = `43918`), 
               color = "#FFFFFF", size = 0.25) + 
  coord_fixed(1) +  #This gives the map a nice 1:1 aspect ratio to prevent the map from appearing squashed
  scale_fill_gradient2(name = "Cumulative cases",
                       low = pal_nejm()(2), 
                       mid = pal_nejm()(3), 
                       high = pal_nejm()(1), 
                       na.value = "grey") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        legend.position = 'bottom',
        axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank())
  
plot2 <- ggplot(data = uk_case) + 
  geom_point(aes(x = Date, y = `Cumulative Cases`), colour = pal_nejm()(1)) + 
  geom_smooth(aes(x = Date, y = `Cumulative Cases`), colour = pal_nejm()(1), se = FALSE, span = 0.5) + 
  # geom_vline(xintercept = as.Date("2020-02-11"), linetype = 2) +
  # geom_smooth(data = mdat %>% filter(date >= as.Date("2020-02-11")), 
  #             aes(x = date, y = value, group = cat, colour = cat)) + 
  # geom_smooth(data = mdat %>% filter(date < as.Date("2020-02-11")), 
  #             aes(x = date, y = value, group = cat, colour = cat)) + 
  #scale_colour_manual(values = pal_nejm()(17), name = "") + 
  scale_x_date(name = "Date", 
               date_breaks = "1 day", 
               date_labels = "%b %d") + 
  labs(title = "Cumulative cases in UK") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.x = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

mdat <- uk_case_region %>% filter(place != "UK", place != "Unconfirmed")
mdat2 <- mdat %>% filter(Date == "2020-03-28") %>% arrange(-value)
mdat$place <- factor(mdat$place, levels = mdat2$place)


plot3 <- ggplot(data = mdat) + 
  geom_point(aes(x = Date, y = value, colour = place)) + 
  geom_smooth(aes(x = Date, y = value, colour = place), se = FALSE) + 
  # geom_vline(xintercept = as.Date("2020-02-11"), linetype = 2) +
  # geom_smooth(data = mdat %>% filter(date >= as.Date("2020-02-11")), 
  #             aes(x = date, y = value, group = cat, colour = cat)) + 
  # geom_smooth(data = mdat %>% filter(date < as.Date("2020-02-11")), 
  #             aes(x = date, y = value, group = cat, colour = cat)) + 
  scale_colour_manual(values = pal_nejm()(17), name = "") + 
  scale_x_date(name = "Date", 
               date_breaks = "1 day", 
               date_labels = "%b %d") + 
  labs(title = "Cumulative cases by regions") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.x = element_blank(),
        legend.position = 'bottom',
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  guides(colour = guide_legend(nrow = 1))

grid.arrange(plot1, plot2, plot3,
             layout_matrix = matrix(c(1, 1, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3), nrow = 2, byrow = TRUE))







mdat <- hangzhou_data %>% gather(key = "cat", value = "value", -1)
mdat$date <- as.Date(mdat$date)

mdat <- mdat %>% filter(!cat %in% c("track_contact_people",
                                    "cancel_isolation",
                                    "still_in_isolation",
                                    "discharge",
                                    "total"))
# mdat <- mdat %>% mutate(
#   cat = case_when(
#     cat == "track_contact_people" ~ "追踪疑似病人",
#     cat == "cancel_isolation" ~ "解除疑似病人",
#     cat == "still_in_isolation" ~ "仍在隔离中病人",
#     cat == "total" ~ "确诊病人",
#   )
# )

ggplot(data = mdat) + 
  geom_point(aes(x = date, y = value, colour = cat)) + 
  geom_vline(xintercept = as.Date("2020-02-11"), linetype = 2) +
  geom_smooth(data = mdat %>% filter(date >= as.Date("2020-02-11")), 
              aes(x = date, y = value, group = cat, colour = cat)) + 
  geom_smooth(data = mdat %>% filter(date < as.Date("2020-02-11")), 
              aes(x = date, y = value, group = cat, colour = cat)) + 
  #scale_colour_manual(values = pal_nejm()(17), name = "") + 
  scale_x_date(name = "Date", 
               date_breaks = "1 day", 
               date_labels = "%b %d") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#-----------------------------------------
mdat <- hangzhou_data 
mdat$date <- as.Date(mdat$date)


ggplot(data = mdat) + 
  geom_point(aes(x = date, y = track_contact_people - cancel_isolation)) + 
  geom_vline(xintercept = as.Date("2020-02-11"), linetype = 2) +
  geom_smooth(data = mdat %>% filter(date >= as.Date("2020-02-11")), 
              aes(x = date, y = track_contact_people - cancel_isolation)) + 
  geom_smooth(data = mdat %>% filter(date < as.Date("2020-02-11")), 
              aes(x = date, y = track_contact_people - cancel_isolation)) + 
  scale_colour_manual(values = pal_nejm()(1), name = "") + 
  scale_x_date(name = "Date", 
               date_breaks = "1 day", 
               date_labels = "%b %d") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))