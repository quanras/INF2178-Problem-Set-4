library(tidyverse)
library(broom)
library(dplyr)
library(knitr)
library(formattable)

# load in data
# make sure working directory is set to folder containing the file
recidivism_data <- read.csv("3-Year_Recidivism_for_Offenders_Released_from_Prison_in_Iowa.csv")

#rename some columns so they're easier to work with
names(recidivism_data)[names(recidivism_data) == "Return.to.Prison"] <- "recidivist"
names(recidivism_data)[names(recidivism_data) == "Race...Ethnicity"] <- "ethnicity"
names(recidivism_data)[names(recidivism_data) == "Convicting.Offense.Classification"] <- "offense_class"
names(recidivism_data)[names(recidivism_data) == "Offense.Type"] <- "offense_type"
names(recidivism_data)[names(recidivism_data) == "Offense.Subtype"] <- "offense_subtype"
names(recidivism_data)[names(recidivism_data) == "Main.Supervising.District"] <- "district"
names(recidivism_data)[names(recidivism_data) == "Release.Type"] <- "release_type"
names(recidivism_data)[names(recidivism_data) == "Age.At.Release"] <- "age"
names(recidivism_data)[names(recidivism_data) == "Fiscal.Year.Released"] <- "release_year"
names(recidivism_data)[names(recidivism_data) == "Sex"] <- "sex"

# turn missing and false NA values into NAs, then remove NAs
recidivism_data[recidivism_data == ""] <- NA
recidivism_data[recidivism_data == "N/A -"] <- NA
drop_na(recidivism_data, recidivist, release_type, age, sex, ethnicity, offense_type, offense_subtype)

# Remove unclear ethnicity values
recidivism_data <- subset(recidivism_data, ethnicity != "White -" )
recidivism_data <- subset(recidivism_data, ethnicity != "Black -" )

# define parole, full_sentence and special sentence dummy variables
recidivism_data <- recidivism_data %>%
  mutate(parole = ifelse(release_type == "Parole", 1, 0))

recidivism_data <- recidivism_data %>%
  mutate(full_sentence = ifelse(release_type == "Discharged End of Sentence", 1, 0))

recidivism_data <- recidivism_data %>%
  mutate(special_sentence = ifelse(release_type == "Special Sentence", 1, 0))

# figure out which split makes more sense based on sample sizes
count(recidivism_data, parole == 1)
count(recidivism_data, full_sentence == 1)
count(recidivism_data, special_sentence == 1)

# establish control and treatment groups are similar

# distribution: age
ggplot(data = recidivism_data) +
    geom_bar(
        mapping = aes(x = parole, fill = age),
        position = "fill"
        ) + 
    labs(
        x = "Parole Granted (No / Yes)",
        y = "Proportion of sample"
        ) +
    theme_minimal()

# distribution: sex
ggplot(data = recidivism_data) +
  geom_bar(
    mapping = aes(x = parole, fill = sex),
    position = "fill"
  ) + 
  labs(
    x = "Parole Granted (No / Yes)",
    y = "Proportion of sample"
  ) +
  theme_minimal()

# distribution: offense_type
ggplot(data = recidivism_data) +
  geom_bar(
      mapping = aes(x = parole, fill = offense_type), 
      position = "fill"
      ) + 
  labs(
    x = "Parole Granted (No / Yes)",
    y = "Proportion of sample"
    ) +
  theme_minimal()

# distribution: ethnicity
ggplot(data = recidivism_data) +
  geom_bar(
    mapping = aes(x = parole, fill = ethnicity),
    position = "fill"
    ) + 
  labs(
    x = "Parole Granted (No / Yes)",
    y = "Proportion of sample"
    ) +
  theme_minimal()

# distribution: release year
recidivism_data$release_year <- as.factor(recidivism_data$release_year)

ggplot(data = recidivism_data) +
  geom_bar(
    mapping = aes(x = parole, fill = release_year),
    position = "fill"
    ) +
  labs(
    x = "Parole Granted (No / Yes)",
    y = "Proportion of sample"
    ) +
  theme_minimal()

# filter to releas years in common to both groups
recidivism_data <- filter(recidivism_data, release_year == "2010" | release_year == "2011" | release_year == "2012")

# replot release year
recidivism_data$release_year <- as.factor(recidivism_data$release_year)

ggplot(data = recidivism_data) +
  geom_bar(
    mapping = aes(x = parole, fill = release_year),
    position = "fill"
  ) +
  labs(
    x = "Parole Granted (No / Yes)",
    y = "Proportion of sample"
  ) +
  theme_minimal()

# calculate recidivism rate for parole vs non-parole groups

# calulate for parole group
parole <- filter(recidivism_data, parole == 1)
parole_recidivist <- filter(parole, recidivist == "Yes")

n_parole <- nrow(parole)
n_parole_recividist <- nrow(parole_recidivist)

recid_rate_parole <- round((n_parole_recividist / n_parole), digits = 3)

# calculate for non-parole group
no_parole <- filter(recidivism_data, parole == 0)
no_parole_recidivist <- filter(no_parole, recidivist == "Yes")

n_no_parole <-nrow(no_parole)
n_no_parole_recidivist <- nrow(no_parole_recidivist)

recid_rate_no_parole <- round((n_no_parole_recidivist / n_no_parole), digits = 3)

summary <- data.frame(recid_rate_no_parole, recid_rate_parole)
summary <- mutate(summary, delta = recid_rate_parole - recid_rate_no_parole)
kable(summary, caption = "Parole rates - full smaple", col.names = c("Non-parole", "Parole", "Delta"))

# chi squared test

chisq.test(recidivism_data$parole, recidivism_data$recidivist) %>% tidy()

# calculate rates for variables of interest

# sex - parole
n_parole_sex_m <- nrow(filter(parole, sex == "Male"))
n_parole_sex_f <- nrow(filter(parole, sex == "Female"))

recid_parole_m <- round((nrow(filter(parole, sex =="Male" & recidivist == "Yes")) / n_parole_sex_m), digits = 3)
recid_parole_f <- round((nrow(filter(parole, sex == "Female" & recidivist == "Yes")) / n_parole_sex_f), digits = 3) 

# sex - no parole
n_no_parole_sex_m <- nrow(filter(no_parole, sex == "Male"))
n_no_parole_sex_f <- nrow(filter(no_parole, sex == "Female"))

recid_no_parole_sex_m <- round((nrow(filter(no_parole, sex =="Male" & recidivist == "Yes")) / n_no_parole_sex_m), digits = 3)
recid_no_parole_sex_f <- round((nrow(filter(no_parole, sex =="Female" &  recidivist == "Yes")) / n_no_parole_sex_m), digits = 3)

# sex - summary
sex_rows <- c("Male", "Female")
sex_no_parole_rates <- c(recid_no_parole_sex_m, recid_no_parole_sex_f)
sex_parole_rates <- c(recid_parole_m, recid_parole_f)
sex_summary <- mutate(sex_summary, delta = sex_parole_rates - sex_no_parole_rates)
kable(sex_summary, caption = "Recidivism rates for non-parole / parole groups by sex", col.names = c("Sex", "No Parole", "Parole", "Delta"))

# release year - parole
n_parole_release_year_2010 <- nrow(filter(parole, release_year == "2010"))
n_parole_release_year_2011 <- nrow(filter(parole, release_year == "2011"))
n_parole_release_year_2012 <- nrow(filter(parole, release_year == "2012"))

recid_parole_2010 <- round((nrow(filter(parole, release_year == "2010" & recidivist == "Yes")) / n_parole_release_year_2010), digits = 3)
recid_parole_2011 <- round((nrow(filter(parole, release_year == "2011" & recidivist == "Yes")) / n_parole_release_year_2011), digits = 3)
recid_parole_2012 <- round((nrow(filter(parole, release_year == "2012" & recidivist == "Yes")) / n_parole_release_year_2012), digit = 3)

# release year - no parole
n_no_parole_release_year_2010 <- nrow(filter(no_parole, release_year == "2010"))
n_no_parole_release_year_2011 <- nrow(filter(no_parole, release_year == "2011"))
n_no_parole_release_year_2012 <- nrow(filter(no_parole, release_year == "2012"))

recid_no_parole_2010 <- round((nrow(filter(no_parole, release_year == "2010" & recidivist == "Yes")) / n_no_parole_release_year_2010), digits = 3)
recid_no_parole_2011 <- round((nrow(filter(no_parole, release_year == "2011" & recidivist == "Yes")) / n_no_parole_release_year_2011), digits = 3)
recid_no_parole_2012 <- round((nrow(filter(no_parole, release_year == "2012" & recidivist == "Yes")) / n_no_parole_release_year_2012), digits = 3)

# release year summary
year_rows <- c("2010", "2011", "2012")
year_no_parole_rates <- c(recid_no_parole_2010, recid_no_parole_2011, recid_no_parole_2012)
year_parole_rates <- c(recid_parole_2010, recid_parole_2011, recid_parole_2012)
year_summary <- data.frame(year_rows, year_no_parole_rates, year_parole_rates)
year_summary <- mutate(year_summary, delta = year_parole_rates - year_no_parole_rates)
kable(year_summary, caption = "Recidivism rates for non-parole / parole groups by release year", col.names = c("Release Year", "No Parole Group", "Parole Group", "Delta"))

# offense type - parole
n_parole_offense_drug <- nrow(filter(parole, offense_type == "Drug"))
n_parole_offense_property <- nrow(filter(parole, offense_type == "Property"))
n_parole_offense_public_order <- nrow(filter(parole, offense_type == "Public Order"))
n_parole_offense_violent <- nrow(filter(parole, offense_type == "Violent"))
n_parole_offense_other <- nrow(filter(parole, offense_type == "Other"))

recid_parole_drug <- round((nrow(filter(parole, offense_type == "Drug" & recidivist == "Yes")) / n_parole_offense_drug), digits = 3)
recid_parole_property <- round((nrow(filter(parole, offense_type == "Property" & recidivist == "Yes")) / n_parole_offense_property), digits = 3)
recid_parole_public_order <- round((nrow(filter(parole, offense_type == "Public Order" & recidivist == "Yes")) / n_parole_offense_public_order), digits = 3)
recid_parole_violent <- round((nrow(filter(parole, offense_type == "Violent" & recidivist == "Yes")) / n_parole_offense_violent), digits = 3)
recid_parole_other <- round((nrow(filter(parole, offense_type == "Other" & recidivist == "Yes")) / n_parole_offense_drug), digits = 3)

# offense_type - no parole
n_no_parole_offense_drug <- nrow(filter(no_parole, offense_type == "Drug"))
n_no_parole_offense_property <- nrow(filter(no_parole, offense_type == "Property"))
n_no_parole_offense_public_order <- nrow(filter(no_parole, offense_type == "Public Order"))
n_no_parole_offense_violent <- nrow(filter(no_parole, offense_type == "Violent"))
n_no_parole_offense_other <- nrow(filter(no_parole, offense_type == "Other"))

recid_no_parole_drug <- round((nrow(filter(no_parole, offense_type == "Drug" & recidivist == "Yes")) / n_no_parole_offense_drug), digits = 3)
recid_no_parole_property <- round((nrow(filter(no_parole, offense_type == "Property" & recidivist == "Yes")) / n_no_parole_offense_property), digits = 3)
recid_no_parole_public_order <- round((nrow(filter(no_parole, offense_type == "Public Order" & recidivist == "Yes")) / n_no_parole_offense_public_order), digits = 3)
recid_no_parole_violent <- round((nrow(filter(no_parole, offense_type == "Violent" & recidivist == "Yes")) / n_no_parole_offense_violent), digits = 3)
recid_no_parole_other <- round((nrow(filter(no_parole, offense_type == "Other" & recidivist == "Yes")) / n_no_parole_offense_drug), digits = 3)

# offense type summary
offense_rows <- c("Drug", "Property", "Public Order", "Violent", "Other")
offense_no_parole_rates <- c(recid_no_parole_drug, recid_no_parole_property, recid_no_parole_public_order, recid_no_parole_violent, recid_no_parole_other)
offense_parole_rates <- c(recid_parole_drug, recid_parole_property, recid_parole_public_order, recid_parole_violent, recid_parole_other)
offense_summary <- data.frame(offense_rows, offense_no_parole_rates, offense_parole_rates)
offense_summary <- mutate(offense_summary, delta = offense_parole_rates - offense_no_parole_rates)
kable(offense_summary, caption = "Recidivism rates in non-parole / parole groups by Offense Type", col.names = c("Offense Type", "No Parole Group", "Parole Group", "Delta"))

# age - parole
n_parole_age_under25 <- nrow(filter(parole, age == "Under 25"))
n_parole_age_25_34 <- nrow(filter(parole, age == "25-34"))
n_parole_age_35_44 <- nrow(filter(parole, age == "35-44"))
n_parole_age_45_54 <- nrow(filter(parole, age == "45-54"))
n_parole_age_55_older <- nrow(filter(parole, age == "55 and Older"))

recid_parole_age_under25 <- round((nrow(filter(parole, age == "Under 25" & recidivist == "Yes")) / n_parole_age_under25), digits = 3)
recid_parole_age_25_34 <- round((nrow(filter(parole, age == "25-34" & recidivist == "Yes")) / n_parole_age_25_34), digits = 3)
recid_parole_age_35_44 <- round((nrow(filter(parole, age == "35-44" & recidivist == "Yes")) / n_parole_age_35_44), digits = 3)
recid_parole_age_45_54 <- round((nrow(filter(parole, age == "45-54" & recidivist == "Yes")) / n_parole_age_45_54), digits = 3)
recid_parole_age_55_older <- round((nrow(filter(parole, age == "55 and Older" & recidivist == "Yes")) / n_parole_age_55_older), digits = 3)

# age - no parole
n_no_parole_age_under25 <- nrow(filter(no_parole, age == "Under 25"))
n_no_parole_age_25_34 <- nrow(filter(no_parole, age == "25-34"))
n_no_parole_age_35_44 <- nrow(filter(no_parole, age == "35-44"))
n_no_parole_age_45_54 <- nrow(filter(no_parole, age == "45-54"))
n_no_parole_age_55_older <- nrow(filter(no_parole, age == "55 and Older"))

recid_no_parole_age_under25 <- round((nrow(filter(no_parole, age == "Under 25" & recidivist == "Yes")) / n_no_parole_age_under25), digits = 3)
recid_no_parole_age_25_34 <- round((nrow(filter(no_parole, age == "25-34" & recidivist == "Yes")) / n_no_parole_age_25_34), digits = 3)
recid_no_parole_age_35_44 <- round((nrow(filter(no_parole, age == "35-44" & recidivist == "Yes")) / n_no_parole_age_35_44), digits = 3)
recid_no_parole_age_45_54 <- round((nrow(filter(no_parole, age == "45-54" & recidivist == "Yes")) / n_no_parole_age_45_54), digits = 3)
recid_no_parole_age_55_older <- round((nrow(filter(no_parole, age == "55 and Older" & recidivist == "Yes")) / n_no_parole_age_55_older), digits = 3)

# age summary
age_rows <- c("Under 25", "25-34", "35-44", "44-54", "55 and Older")
age_no_parole_rates <- c(recid_no_parole_age_under25, recid_no_parole_age_25_34, recid_no_parole_age_35_44, recid_no_parole_age_45_54, recid_no_parole_age_55_older)
age_parole_rates <- c(recid_parole_age_under25, recid_parole_age_25_34, recid_parole_age_35_44, recid_parole_age_45_54, recid_parole_age_55_older)
age_summary <- data.frame(age_rows, age_no_parole_rates, age_parole_rates)
age_summary <- mutate(age_summary, delta = age_parole_rates - age_no_parole_rates)
kable(age_summary, caption = "Recidivism rates for non-parole / parole groups by age", col.names = c("Age Range", "Non-parole", "Parole", "Delta"))

# ethnicity - parole

n_parole_native_hispanic <- nrow(filter(parole, ethnicity == "American Indian or Alaska Native - Hispanic"))
n_parole_native_nonhispanic <- nrow(filter(parole, ethnicity == "American Indian or Alaska Native - Non-Hispanic"))
n_parole_asian_hispanic <- nrow(filter(parole, ethnicity == "Asian or Pacific Islander - Hispanic"))
n_parole_asian_nonhispanic <- nrow(filter(parole, ethnicity == "Asian or Pacific Islander - Non-Hispanic"))
n_parole_black_hispanic <- nrow(filter(parole, ethnicity == "Black - Hispanic"))
n_parole_black_nonhispanic <- nrow(filter(parole, ethnicity == "Black - Non-Hispanic"))
n_parole_white_hispanic <- nrow(filter(parole, ethnicity == "White - Hispanic"))
n_parole_white_nonhispanic <- nrow(filter(parole, ethnicity == "White - Non-Hispanic"))

recid_parole_native_hispanic <- round((nrow(filter(parole, ethnicity == "American Indian or Alaska Native - Hispanic" & recidivist == "Yes")) / n_parole_native_hispanic), digits = 3)
recid_parole_native_nonhispanic <- round((nrow(filter(parole, ethnicity == "American Indian or Alaska Native - Non-Hispanic" & recidivist == "Yes")) / n_parole_native_nonhispanic), digits = 3)
recid_parole_asian_hispanic <- round((nrow(filter(parole, ethnicity == "Asian or Pacific Islander - Hispanic" & recidivist == "Yes")) / n_parole_asian_hispanic), digits = 3)
recid_parole_asian_nonhispanic <- round((nrow(filter(parole, ethnicity == "Asian or Pacific Islander - Non-Hispanic" & recidivist == "Yes")) / n_parole_asian_nonhispanic), digits = 3)
recid_parole_black_hispanic <- round((nrow(filter(parole, ethnicity == "Black - Hispanic" & recidivist == "Yes")) / n_parole_black_hispanic), digits = 3)
recid_parole_black_nonhispanic <- round((nrow(filter(parole, ethnicity == "Black - Non-Hispanic" & recidivist == "Yes")) / n_parole_black_nonhispanic), digits = 3)
recid_parole_white_hispanic <- round((nrow(filter(parole, ethnicity == "White - Hispanic" & recidivist == "Yes")) / n_parole_white_hispanic), digits = 3)
recid_parole_white_nonhispanic <- round((nrow(filter(parole, ethnicity == "White - Non-Hispanic" & recidivist == "Yes")) / n_parole_white_nonhispanic), digits = 3)

# ethnicity - no parole

n_no_parole_native_hispanic <- nrow(filter(no_parole, ethnicity == "American Indian of Alaska Native - Hispanic"))
n_no_parole_native_nonhispanic <- nrow(filter(no_parole, ethnicity == "American Indian of Alaska Native - Non-Hispanic"))
n_no_parole_asian_hispanic <- nrow(filter(no_parole, ethnicity == "Asian or Pacific Islander - Hispanic"))
n_no_parole_asian_nonhispanic <- nrow(filter(no_parole, ethnicity == "Asian or Pacific Islander - Non-Hispanic"))
n_no_parole_black_hispanic <- nrow(filter(no_parole, ethnicity == "Black - Hispanic"))
n_no_parole_black_nonhispanic <- nrow(filter(no_parole, ethnicity == "Black - Non-Hispanic"))
n_no_parole_white_hispanic <- nrow(filter(no_parole, ethnicity == "White - Hispanic"))
n_no_parole_white_nonhispanic <- nrow(filter(no_parole, ethnicity == "White - Non-Hispanic"))

recid_no_parole_native_hispanic <- round((nrow(filter(no_parole, ethnicity == "American Indian of Alaska Native - Hispanic" & recidivist == "Yes")) / n_parole_native_hispanic), digits = 3)
recid_no_parole_native_nonhispanic <- round((nrow(filter(no_parole, ethnicity == "American Indian of Alaska Native - Non-Hispanic" & recidivist == "Yes")) / n_parole_native_nonhispanic), digits = 3)
recid_no_parole_asian_hispanic <- round((nrow(filter(no_parole, ethnicity == "Asian or Pacific Islander - Hispanic" & recidivist == "Yes")) / n_parole_asian_hispanic), digits = 3)
recid_no_parole_asian_nonhispanic <- round((nrow(filter(no_parole, ethnicity == "Asian or Pacific Islander - Non-Hispanic" & recidivist == "Yes")) / n_parole_asian_nonhispanic), digits = 3)
recid_no_parole_black_hispanic <- round((nrow(filter(no_parole, ethnicity == "Black - Hispanic" & recidivist == "Yes")) / n_parole_black_hispanic), digits = 3)
recid_no_parole_black_nonhispanic <- round((nrow(filter(no_parole, ethnicity == "Black - Non-Hispanic" & recidivist == "Yes")) / n_parole_black_nonhispanic), digits = 3)
recid_no_parole_white_hispanic <- round((nrow(filter(no_parole, ethnicity == "White - Hispanic" & recidivist == "Yes")) / n_parole_white_hispanic), digits = 3)
recid_no_parole_white_nonhispanic <- round((nrow(filter(no_parole, ethnicity == "White - Non-Hispanic" & recidivist == "Yes")) / n_parole_white_nonhispanic), digits = 3)

# ethnicity summary
ethnicity_rows <- c("American Indian of Alaska Native - Hispanic", "American Indian of Alaska Native - Non-Hispanic", "Asian or Pacific Islander - Hispanic", "Asian or Pacific Islander - Non-Hispanic", "Black - Hispanic", "Black - Non-Hispanic", "White - Hispanic", "White - Non-Hispanic")
ethnicity_no_parole_rates <- c(recid_no_parole_native_hispanic, recid_no_parole_native_nonhispanic, recid_no_parole_asian_hispanic, recid_no_parole_asian_nonhispanic, recid_no_parole_black_hispanic, recid_no_parole_black_nonhispanic, recid_no_parole_white_hispanic, recid_no_parole_white_nonhispanic)
ethnicity_parole_rates <- c(recid_parole_native_hispanic, recid_parole_native_nonhispanic, recid_parole_asian_hispanic, recid_parole_asian_nonhispanic, recid_parole_black_hispanic, recid_parole_black_nonhispanic, recid_parole_white_hispanic, recid_parole_white_nonhispanic)
ethnicity_summary <- data.frame(ethnicity_rows, ethnicity_no_parole_rates, ethnicity_parole_rates)
ethnicity_summary <- mutate(ethnicity_summary, delta = ethnicity_parole_rates - ethnicity_no_parole_rates)
kable(ethnicity_summary, caption = "Recidivism rates for non-parole / parole groups by ethnicity", col.names = c("Ethnicity", "Non-parole", "Parole", "Delta"))

# package citations
citation("tidyverse")
citation("broom")
citation("dplyr")
citation("knitr")
citation("formattable")
