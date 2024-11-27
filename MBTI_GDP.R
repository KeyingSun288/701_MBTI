library(ggplot2)
library(tidyverse)
library(readr)
library(tableone)
# install.packages("ggpubr")
library(ggpubr)
# install.packages("rworldmap")
library(rworldmap)

# Importing ---------------------------------------------------------------

MBTI <- read_csv("/Users/keyingsun/Desktop/Main/MS Biostats/BS 701/Final Project/MBTI_by country/countries.csv")
head(MBTI)
class(MBTI)

GDP <- read_csv("/Users/keyingsun/Desktop/Main/MS Biostats/BS 701/Final Project/GDPPC/GDPINT.csv", show_col_types = TRUE)
head(GDP)
class(GDP)


# filtering ---------------------------------------------------------------

GDP.Recent <- na.omit(GDP %>% select(Country, "GDP.2022"))
GDP.Recent

GDP.126 <- GDP.Recent %>% inner_join(MBTI, by = "Country") 
GDP.126

MBTI.126 <- MBTI %>% inner_join(GDP.Recent, by = "Country")
MBTI.126


# Quality Check -----------------------------------------------------------

rowSums(x = GDP.126[1:126,3:34], na.rm = TRUE)

print(GDP.126$Country)

excluded <- GDP.Recent %>% anti_join(MBTI, by = "Country")
excluded

duplicate <- GDP.126[duplicated(GDP.126),]
duplicate

# Sums --------------------------------------------------------------------

colnames(GDP.126)

# Make mbti into pivot
mbti_wide <- GDP.126 %>% 
  pivot_longer(cols = "ESTJ-A":"INFJ-A",
               names_to = "Category",
               values_to = "Value")
mbti_wide

# Divide category specifically 
mbti_wide_flag <- mbti_wide %>% 
  mutate(E_flag = if_else(substr(Category, 1, 1)=="E", 1, 0),
         S_flag = if_else(substr(Category, 2, 2)=="S", 1, 0),
         T_flag = if_else(substr(Category, 3, 3)=="T", 1, 0),
         J_flag = if_else(substr(Category, 4, 4)=="J", 1, 0),
         A_flag = if_else(substr(Category, 6, 6)=="A", 1, 0))

mbti_summary <- mbti_wide_flag |> 
  group_by(Country) |> 
  summarise(
    E_sum = sum(Value[E_flag == 1]),
    S_sum = sum(Value[S_flag == 1]),
    T_sum = sum(Value[T_flag == 1]),
    J_sum = sum(Value[J_flag == 1]),
    A_sum = sum(Value[A_flag == 1]))
mbti_summary

master <- mbti_summary %>% mutate(GDP.126$GDP.2022, by = "Country") 
master
colnames(master)

# Plotting ----------------------------------------------------------------

E.scat <- ggplot(master, aes(y = `GDP.126$GDP.2022`, x = E_sum)) +
  geom_point(col = "blue") +
  geom_smooth(method = "lm", col = "red") +
  labs(y = "GDP per capita ($ international)", x = "Extroversion (% pop)", title = "Extroversion vs. GDP per capita") +
  theme_minimal()
E.scat
summary(E.scat)

S.scat <- ggplot(master, aes(y = GDP.126$GDP.2022, x = S_sum)) +
  geom_point(col = "pink") +
  geom_smooth(method = "lm", col = "red") +
  labs(x = "Cognitive Sensing (% pop)", y = "GDP per capita ($ international)", title = "Cognitive Sensing Personalities vs. GDP per capita") +
  theme_minimal()
S.scat

T.scat <- ggplot(master, aes(y = GDP.126$GDP.2022, x = T_sum)) +
  geom_point(col = "darkgreen") +
  geom_smooth(method = "lm", col = "red") +
  labs(y = "GDP per capita ($ international)", x = "Cognitive Type: Thinking (% pop)", title = "Cognitive Thinking Personalities vs. GDP per capita") +
  theme_minimal()
T.scat

J.scat <- ggplot(master, aes(y = GDP.126$GDP.2022, x = J_sum)) +
  geom_point(col = "orange") +
  geom_smooth(method = "lm", col = "red") +
  labs(x = "GDP per capita ($ international)", y = "Rigidness (% pop)", title = "Rigidness vs. GDP per capita") +
  theme_minimal()
J.scat

A.scat <- ggplot(master, aes(y = GDP.126$GDP.2022, x = A_sum)) +
  geom_point(col = "purple") +
  geom_smooth(method = "lm", col = "red") +
  labs(y = "Assertiveness (% pop)", x = "GDP per capita ($ international)", title = "Assertive Personalities vs. GDP per capita") +
  theme_minimal()
A.scat


# Heatmap -----------------------------------------------------------------


# extroversion heatmap
extroversion_data <- data.frame(
  Country = master$Country, # Include all 126 countries
  Extroversion_Sum = master$E_sum)
extroversion_data

world_map <- joinCountryData2Map(extroversion_data, joinCode = "NAME", nameJoinColumn = "Country")


mapCountryData(world_map, nameColumnToPlot = "Extroversion_Sum", 
               mapTitle = "Global Distribution of Extroversion Scores",
               colourPalette = "heat", # You can also try palettes like "terrain", "rainbow"
               oceanCol = "lightblue", # Background color for oceans
               missingCountryCol = "white") # Color for countries with missing data


# sensing heatmap
sensing_data <- data.frame(
  Country = master$Country, # Include all 126 countries
  Sensing_Sum = master$S_sum)
sensing_data

world_map_sensing <- joinCountryData2Map(sensing_data, joinCode = "NAME", nameJoinColumn = "Country")

mapCountryData(world_map_sensing, nameColumnToPlot = "Sensing_Sum", 
               mapTitle = "Global Distribution of Sensing Scores",
               colourPalette = "heat", # You can also try palettes like "terrain", "rainbow"
               oceanCol = "lightblue", # Background color for oceans
               missingCountryCol = "white") # Color for countries with missing data

# world GDP
GDP_data <- data.frame(
  Country = master$Country, # Include all 126 countries
  GDP_Sum = master$`GDP.126$GDP.2022`)
GDP_data

world_map_GDP <- joinCountryData2Map(GDP_data, joinCode = "NAME", nameJoinColumn = "Country")

mapCountryData(world_map_GDP, nameColumnToPlot = "GDP_Sum", 
               mapTitle = "Global Distribution of GDP",
               colourPalette = "heat", # You can also try palettes like "terrain", "rainbow"
               oceanCol = "lightblue", # Background color for oceans
               missingCountryCol = "white")

