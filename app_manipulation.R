install.packages("devtools")
devtools::install_github("bdilday/GeomMLBStadiums")

library(ggplot2)
library(tidyverse)
library(GeomMLBStadiums)
library(readxl)
library(data.table)

setwd("/Users/aidanbeilke/Downloads/")

teams <- read_xlsx("FILE_6054.xlsx")
data <- read.csv("Cape_Database_Fixed_Names_Final.csv") # cape data
data2 <- read.csv("College_Updated_5_24TH 2.csv", nrows = 200000) # 2023 college data



# Spray Chart

data <- rbind(data3, data2)

data <- data |> 
  mutate(Bearing = Bearing * pi / 180,
         hc_x = sin(Bearing) * Distance,
         hc_y = cos(Bearing) * Distance,
         Date = as.Date(data$Date))


teams <- read_xlsx("FILE_6054.xlsx")

team_name <- teams |> 
  mutate(Batter = paste(`Last Name`, `First Name`, sep = ", "),
         Batter = ifelse(Batter == "Gazdar, Jonathan", "Gazdar, Jon Jon", Batter)) |> 
  select(Batter, Team)

data <- data %>%
  left_join(team_name, by = "Batter") %>%
  mutate(Team = coalesce(Team, "other"))


## Heat Maps

data <- data %>%
  mutate(
    is_whiff = as.integer(PitchCall == "StrikeSwinging"),
    OPS_val = case_when(
      PitchCall == "InPlay" & PlayResult == "Single"   ~ 2,
      PitchCall == "InPlay" & PlayResult == "Double"   ~ 3,
      PitchCall == "InPlay" & PlayResult == "Triple"   ~ 4,
      PitchCall == "InPlay" & PlayResult == "HomeRun"  ~ 5,
      TRUE                                             ~ 0
    ),
    Zone = case_when(
      PlateLocSide >= -0.83 & PlateLocSide <= -0.27 & PlateLocHeight <= 3.5 & PlateLocHeight >= 2.867 ~ 1,
      PlateLocSide >= -0.27 & PlateLocSide <= 0.27 & PlateLocHeight <= 3.5 & PlateLocHeight >= 2.867 ~ 2,
      PlateLocSide >= 0.27 & PlateLocSide <= 0.83 & PlateLocHeight <= 3.5 & PlateLocHeight >= 2.867 ~ 3,
      PlateLocSide >= -0.83 & PlateLocSide <= -0.27 & PlateLocHeight >= 2.234 & PlateLocHeight <= 2.867 ~ 4,
      PlateLocSide >= -0.27 & PlateLocSide <= 0.27 & PlateLocHeight >= 2.234 & PlateLocHeight <= 2.867 ~ 5,
      PlateLocSide >= 0.27 & PlateLocSide <= 0.83 & PlateLocHeight >= 2.234 & PlateLocHeight <= 2.867 ~ 6,
      PlateLocSide >= -0.83 & PlateLocSide <= -0.27 & PlateLocHeight >= 1.6 & PlateLocHeight <= 2.34 ~ 7,
      PlateLocSide >= -0.27 & PlateLocSide <= 0.27 & PlateLocHeight >= 1.6 & PlateLocHeight <= 2.34 ~ 8,
      PlateLocSide >= 0.27 & PlateLocSide <= 0.83 & PlateLocHeight >= 1.6 & PlateLocHeight <= 2.34 ~ 9,
      PlateLocSide <= -0.82 & PlateLocHeight >= 2.55 ~ 11,
      PlateLocSide <= 0 & PlateLocHeight >= 3.5 ~ 11,
      PlateLocSide >= 0.82 & PlateLocHeight >= 2.55 ~ 12,
      PlateLocSide >= 0 & PlateLocHeight >= 2.5 ~ 12,
      PlateLocSide <= -0.82 & PlateLocHeight <= 2.55 ~ 13,
      PlateLocSide <= 0 & PlateLocHeight <= 1.6 ~ 13,
      PlateLocSide >= 0.83 & PlateLocHeight <= 2.55 ~ 14,
      PlateLocSide >= 0 & PlateLocHeight <= 1.6 ~ 14,
      TRUE ~ 0
    )
  )

# Barrels

is_barrel <- function(ExitSpeed, Angle) {
  # Check for missing values
  if (is.na(ExitSpeed) || is.na(Angle)) {
    return(NA)  # Return NA if there is missing data
  }
  
  if (ExitSpeed < 98) {
    return(0) # Not a barrel if exit speed is less than 98 mph
  } else {
    # Calculate the expansion of the launch angle range
    additional_degrees = max(0, ExitSpeed - 98)  # Ensures no expansion for speeds less than 98
    min_angle = 26 - 0.5 * additional_degrees  # Adjusting expansion factor to 0.5 degrees per mph
    max_angle = 30 + 0.5 * additional_degrees
    
    # Determine if the launch angle is within the expanded range
    if (Angle >= min_angle & Angle <= max_angle) {
      return(1) # It's a barrel
    } else {
      return(0) # Not a barrel
    }
  }
}

data$barrel = mapply(is_barrel, data$ExitSpeed, data$Angle)


xba <- data |> 
  mutate(is_hit = ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 1, 0)) |> 
  filter(PitchCall == "InPlay") |> 
  select(ExitSpeed, Angle, is_hit) |> 
  mutate(ev = round(ExitSpeed),
         la = round(Angle)) |> 
  group_by(ev, la) |> 
  summarise(is_hit = sum(is_hit, na.rm = T),
            bbe = n(),
            xba = round(is_hit / bbe, 3)) |> 
  arrange(desc(bbe)) |> 
  na.omit() |> 
  select(ev, la, bbe, xba)


data <- data |> 
  mutate(ev = round(ExitSpeed),
         la = round(Angle)) |> 
  left_join(xba, by = c("ev", "la"))


# write.csv(data, "app.csv")

setwd("/Users/aidanbeilke/Desktop/Hawks Apps/Shiny App")
saveRDS(data, "app.rds")


