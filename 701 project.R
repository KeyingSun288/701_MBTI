library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(gt)

setwd("C:/Users/Zada.Tiimob.2020/Desktop/BIOST/701 project")
mbti<-read_csv("C:/Users/Zada.Tiimob.2020/Desktop/BIOST/701 project/mbti.csv")
crime<-read_csv("C:/Users/Zada.Tiimob.2020/Desktop/BIOST/701 project/crime.csv")

crimedata<-crime %>%
  rename(Country = country)


#creating merged dataset
merged_data <- merge(mbti, crimedata, by = "Country")


#coding all personality types that contain each letter

letter_summary <- merged_data %>%
  summarise(
    E = `ESTJ-A` + `ESTJ-T` + `ESFJ-A` + `ESFJ-T` + `ENFP-A` + `ENFP-T` + `ENFJ-A` + `ENFJ-T` + `ESFP-A` + `ESFP-T`,
    I = `ISTJ-A` + `ISTJ-T` + `ISFJ-A` + `ISFJ-T` + `INFP-A` + `INFP-T` + `INFJ-A` + `INFJ-T` + `ISFP-A` + `ISFP-T`,
    S = `ESTJ-A` + `ESTJ-T` + `ESFJ-A` + `ESFJ-T` + `ISFJ-A` + `ISFJ-T` + `ESFP-A` + `ESFP-T`,
    N = `ENFP-A` + `ENFP-T` + `INFP-A` + `INFP-T` + `INFJ-A` + `INFJ-T`,
    `T` = `ESTJ-A` + `ESTJ-T` + `ENTJ-A` + `ENTJ-T` + `INTP-A` + `INTP-T`,
    `F` = `ESFJ-A` + `ESFJ-T` + `ENFJ-A` + `ENFJ-T` + `INFP-A` + `INFP-T`,
    J = `ESTJ-A` + `ESTJ-T` + `ESFJ-A` + `ESFJ-T` + `ENTJ-A` + `ENTJ-T`,
    P = `ESTP-A` + `ESTP-T` + `ENFP-A` + `ENFP-T` + `INFP-A` + `INFP-T` + `ISFP-A` + `ISFP-T`
  ) %>%
  mutate(Country = merged_data$Country)

#merging letter encoded data with original merged_data 
combined_data <- letter_summary %>%
  inner_join(merged_data, by = "Country")


#graphs: letter proportion to crime rate
ggplot(combined_data, aes(x = E, y = crimeIndex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Crime Index vs. E Proportion")+
  theme_minimal()

ggplot(combined_data, aes(x = I, y = crimeIndex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Crime Index vs. I Proportion")+
  theme_minimal()

ggplot(combined_data, aes(x = S, y = crimeIndex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Crime Index vs. S Proportion")+
  theme_minimal()

ggplot(combined_data, aes(x = N, y = crimeIndex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Crime Index vs. N Proportion")+
  theme_minimal()

ggplot(combined_data, aes(x = `T`, y = crimeIndex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Crime Index vs. T Proportion")+
  theme_minimal()

ggplot(combined_data, aes(x = `F`, y = crimeIndex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Crime Index vs. F Proportion")+
  theme_minimal()

ggplot(combined_data, aes(x = J, y = crimeIndex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Crime Index vs. J Proportion")+
  theme_minimal()

ggplot(combined_data, aes(x = P, y = crimeIndex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Crime Index vs. P Proportion")+
  theme_minimal()


#correlation values for the graphs

letters <- c("E", "I", "S", "N", "T", "F", "J", "P")

correlation_results <- data.frame(Letter = character(), CrimeCorrelation = numeric(), stringsAsFactors = FALSE)

# Loop through each letter and calculate the correlation with crimeIndex
for (letter in letters) {
  # Check if the letter column exists in the data frame
  if (letter %in% colnames(combined_data)) {
    # Calculate the correlation for the current letter
    correlation <- cor(combined_data[[letter]], combined_data$crimeIndex, use = "complete.obs")
    # Append the result to the data frame
    correlation_results <- rbind(correlation_results, data.frame(Letter = letter, CrimeCorrelation = correlation))
  } else {
    print(paste("Column", letter, "does not exist in the data frame."))
  }
}

# summary table
print(correlation_results)
gt(correlation_results)



# PERSONALITY TYPE V CRIME RATE GRAPHS ------------------------------------


PersonalityType<-combined_data %>%
  select(10:41,)

PersonalityType$crimeIndex<-combined_data$crimeIndex

ggplot(PersonalityType, aes(x = `ENTP-A`, y = crimeIndex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Crime Index vs. ENTP-A Proportion")+
  theme_minimal()

ggplot(PersonalityType, aes(x = `ESFJ-A`, y = crimeIndex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Crime Index vs. ENTP-A Proportion")+
  theme_minimal()



##computing all correlations, Combining into data frame
ptypes<-combined_data %>%
  select(10:41,)

types<-colnames(ptypes)

cor_results <- data.frame(PersonalityType = character(),
                                  CrimeCorrelation = numeric(),
                                  stringsAsFactors = FALSE)

for (type in types) {
  # Calculate correlation
  cor_value <- cor(combined_data[[type]], combined_data$crimeIndex, use = "complete.obs")
  
  # Store the result in the correlation results data frame
  cor_results <- rbind(cor_results, data.frame(PersonalityType = type, CrimeCorrelation = cor_value))
  
}

# Print the correlation results as a neat table
print(cor_results)

gt(cor_results)




# BAR PLOTS ---------------------------------------------------------------


# correlation by letter -------------------------------------

factorletters<-as.factor(correlation_results$Letter)
factordata<-data.frame(factorletters, CrimeCorrelation = correlation_results$CrimeCorrelation)

ggplot(correlation_results, aes(x = Letter, y = CrimeCorrelation, fill = CrimeCorrelation < 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "#B71C1C", "FALSE" = "#A5D6A7")) +  # Color positive and negative correlations
  labs(title = "Correlation Between Personality Letters and Crime Index",
       x = "Personality Factor",
       y = "Crime Correlation") +
  theme_minimal()

# correlation by entire personality type (so many it's distracting, maybe don't include)--------------------

ggplot(cor_results, aes(x = PersonalityType, y = CrimeCorrelation, fill = CrimeCorrelation > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "#B71C1C", "FALSE" = "#A5D6A7")) +  # Color positive and negative correlations
  labs(title = "Correlation Between Personality Type and Crime Index",
       x = "Personality Factor",
       y = "Crime Correlation") +
  theme_minimal()
# EXTRA/IGNORE ------------------------------------------------------------


#loop sequence for plots
# Create scatter plot
p <- ggplot(combined_data, aes_string(x = type, y = "crimeIndex")) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = paste("Crime Index vs.", type, "Proportion"),
       x = paste("Proportion of", type),
       y = "Crime Index") +
  theme_minimal()

# Print the plot
print(p)


