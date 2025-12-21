library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(here)

#------------------------------
# LOADING IN DATA AND SUBSETTING
#------------------------------

# az1 <- read_csv('/Users/nathanialodell/Library/CloudStorage/OneDrive-UW/Thesis/Data/AZ/2013-2019.csv') %>%
#   as_tibble()
# az2 <- read_csv('/Users/nathanialodell/Library/CloudStorage/OneDrive-UW/Thesis/Data/AZ/2020-2024.csv') %>%
#   as_tibble()
# 
# str(az1)
# str(az2)

# az1_mi <- az1[is.na(az1$Latitude), ] # no spatial indication at all
# az2_mi <- az2[is.na(az2$Latitude), ] # no cross street data

# az1 <- az1 %>%
#   dplyr::select(
#     `ID Number`,
#     `Lab Date`,
#     Species,
#     Males,
#     Females,
#     Disease,
#     `Test Result`,
#     Latitude,
#     Longitude
#   ) %>%
#   dplyr::rename(
#     TrapID = "ID Number",
#     Lab_Date = "Lab Date",
#     Result = "Test Result"
#   ) %>%
#   dplyr::mutate(
#     Males = as.integer(Males),
#     Females = as.integer(Females),
#     Lab_Date = as.Date(Lab_Date, "%m/%d/%y"),
#     Disease = ifelse(
#       Disease == "None", NA, Disease
#     )
#   )
# 
# az2 <- az2 %>%
#   dplyr::select(
#     TrapID,
#     Lab_Date,
#     Species,
#     Males,
#     Females,
#     Disease,
#     Result,
#     Latitude,
#     Longitude
#   ) %>%
#   dplyr::mutate(
#     Males = as.integer(Males),
#     Females = as.integer(Females),
#     Lab_Date = as.Date(Lab_Date, "%m/%d/%y")
#   )
# 
# AZ <- rbind(az1, az2)
# write_rds(AZ, "Maricopa (all yr).RDS")

months = c("Jan", "Feb", "Mar", "Apr",
           "May", "Jun", "Jul", "Aug", "Sep",
           "Oct", "Nov", "Dec")

AZ <- readRDS("Maricopa (all yr).RDS") %>%
  dplyr::mutate(
    Year = as.integer(year(Lab_Date)),
    Month = factor(month(Lab_Date), levels = 1:12,
                   labels = months
    )
  )

AZ_long <- AZ %>% # easier to plot with longer format
  pivot_longer(cols = c(Males, Females), names_to = "Sex", values_to = "Count")

#--------------------------------------------------------
# some time series for visual trends (purely descriptive!)
#--------------------------------------------------------

# keep this handy! https://posit.co/wp-content/uploads/2022/10/data-visualization-1.pdf

# only plotting fully identified collections for now; and only females
ggplot(
  data = AZ_long %>%
    dplyr::filter(
      !(Species %in% c("None", "Bird", "Unknown", "Cs species", 
                                    "Ps species", "An species", NA, "None",
                       "Ps columbiae")),
      Sex == "Females"
                  ) %>%
    dplyr::group_by(
      Year, Species, Month
    ) %>%
    dplyr::summarise(
      Total = log(sum(Count) + 1)
    ),
  aes(x = Month, y = Total, color = Species, group = Species)
) +
  geom_line() +
  facet_wrap(~ Year)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("ln(Abundance + 1)")

# need monthly trends moreso than yearly so let's look at a stack barchart...
ggplot(
  data = AZ_long %>%
    dplyr::filter(
      !(Species %in% c("None", "Bird", "Unknown", "Cs species", 
                       "Ps species", "An species", NA, "None"))
    ) %>%
    dplyr::group_by(
      Year, Month, Species, Sex
    ) %>%
    dplyr::summarise(
      Total = sum(Count)
    ),
  aes(fill = Species, y = Total, x = Month)
) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Proportion") +
  facet_wrap(~ Year)

# let's look at this in space across all years
# ggplot(
#   data = AZ_long %>%
#     dplyr::filter(
#       !(Species %in% c("None", "Bird", "Unknown", "Cs species", 
#                        "Ps species", "An species", NA, "None",
#                        "Ps columbiae")),
#       Sex == "Females"
#     ) %>%
#     dplyr::group_by(
#       Species, Latitude, Longitude, TrapID
#     ) %>%
#     dplyr::summarise(
#       Total = log(sum(Count) + 1)
#     ) %>%
#     st_as_sf(
#       coords = c("Latitude", "Longitude")
#     )
# ) # turns out there are some NAs in the lat/long for 1612 observations

# also, there are about 23 observations with 0 females and 0 males, but with a recorded species
# AZ %>% filter(Females != 0 & Males != 0 & !(Species %in% c("None", "Bird", "Unknown", "Cs species", 
#                                                               "Ps species", "An species", NA, "None")))
# AZ <- AZ %>%
#   filter(
#     !(
#       Females == 0 &
#         Males == 0 &
#         !(Species %in% c(
#           "None", "Bird", "Unknown",
#           "Cs species", "Ps species", "An species",
#           NA
#         ))
#     )
#   )

#----------------------------------
# INVESTIGATING MISSING COORDINATES (FINISHED 12/20/2025)
#----------------------------------
# 
# rm(list = c("AZ_long", "coords", "data", "months")) # clean up the environment
# 
# # missing_coords <- AZ[is.na(AZ$Latitude), ] # there's one complete NA here: remove it
# 
# AZ <- AZ[-is.na(AZ$TrapID),]
# missing_coords <- AZ[is.na(AZ$Latitude), ] 
# 
# # might be useful to filter out traps w/ coordinates that have a similar trapID name structure as the ones with missingness
# # gonna need to use regex for this
# # grab the indices with regex matches to trapIDs w/ missing coords
# indices <- c(grep("CC[1-9]+", AZ$TrapID),
#              grep("[1-9][1-9]-[0-9]+", AZ$TrapID),
#              grep("CC-[0-9]+-[0-9]+", AZ$TrapID),
#              grep("SR[A-Z]+-[1-9]", AZ$TrapID),
#              grep("SR[A-Z]+-[A-Z]+ [0-9]+", AZ$TrapID),
#              grep("RT[1-9]+", AZ$TrapID),
#              grep("HC[1-9]", AZ$TrapID))
# # this matches the entire AZ dataset... no cigar 
# 
# AZ <- AZ[!is.na(AZ$Latitude),]
# write_rds(AZ %>%
# dplyr::mutate(
#   Males_ln = log(Males + 1),
#   Females_ln = log(Females + 1)
# ), "Maricopa (all yr).RDS")

#-----------------------
# turning into shapefile 
#-----------------------

CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

AZ_sf <- AZ %>%
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = CRS
  )

AZ_long <- AZ_sf %>% # easier to plot with longer format
  pivot_longer(cols = c(Males, Females), names_to = "Sex", values_to = "Count")

write_sf(AZ_sf, "Maricopa (wide).shp")
write_sf(AZ_long, "Maricopa (long).shp")

#--------------------------------------------------------
# some time series for visual trends (purely descriptive!)
#--------------------------------------------------------
AZ_sf <- read_sf("Maricopa (wide).shp")
AZ_long <- read_sf("Maricopa (long).shp")

# keep this handy! https://posit.co/wp-content/uploads/2022/10/data-visualization-1.pdf

p1 <- ggplot(
  data = AZ_long %>%
    dplyr::filter(
      !(Species %in% c("None", "Bird", "Unknown", "Cs species", 
                       "Ps species", "An species", NA, "None"))
    ) %>%
    dplyr::group_by(
      Year, Month, Species, Sex
    ) %>%
    dplyr::summarise(
      Total = sum(Count)
    ),
  aes(fill = Species, y = Total, x = Month)
) + 
  geom_bar(position="fill", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Proportion") +
  facet_wrap(~ Year)

ggsave(here("Figs", "species_props_AZ.png"), plot = p1)

p2 <- ggplot(
  data = AZ_long %>%
    dplyr::filter(
      !(Species %in% c("None", "Bird", "Unknown", "Cs species", 
                       "Ps species", "An species", NA, "None",
                       "Ps columbiae")),
      Sex == "Females"
    ) %>%
    dplyr::group_by(
      Year, Species, Month
    ) %>%
    dplyr::summarise(
      Total = sum(log(Count + 1))
    ),
  aes(x = Month, y = Total, color = Species, group = Species)
) +
  geom_line() +
  facet_wrap(~ Year)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("ln(Abundance + 1)")

ggsave(here("Figs", "year_trends_AZ.png"), plot = p2)

temp <- AZ_long %>%
  dplyr::group_by(
    geometry, Year # not grouping by trapID for effort because trap names could change without their locations changing
  ) %>%
  dplyr::summarise(
    effort = n()/length(unique(Year))
  )

p3 <- ggplot(
  data = temp
) +
  geom_histogram(aes(effort), binwidth = 
                   2 * IQR(temp$effort) / length(temp$effort)^(1/3)) + # using Freedman-Diaconis rule
  facet_wrap(~Year)

ggsave(here("Figs", "effort_AZ.png"), plot = p3)

p4 <- ggplot(
  data = AZ_sf%>%
    dplyr::filter(
      !(Species %in% c("None", "Bird", "Unknown", "Cs species", 
                       "Ps species", "An species", NA, "None",
                       "Ps columbiae")),
      Females_ln != 0 # this is different than what we filtered out above--some observations have males but no females
    ) %>%
    dplyr::group_by(
      Year, Species, geometry
    ) %>%
    dplyr::summarise(
      Mean = sum(Females_ln)
    ) %>%
    st_jitter(factor = 0.05)
) +
  geom_sf(aes(size = Mean, color = Species), shape = 1, alpha = 0.5) +
  labs(size = "mean(log(abundance + 1))") +
  scale_color_manual(values = c("#EB2D05", "#6A650B", "#0CED25","#0C31ED", "#BC0CED", "#FFAD00")) +
  facet_wrap(~Year)

ggsave(here("Figs", "spatial_sums_AZ.png"), plot = p4)
