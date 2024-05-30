################################## Renv init
#renv::init() Only gets done once, commented out afterwards

################################## Set seed for reproducibility
set.seed(1337)

################################## Install packages
# You may have to initialise renv
renv::restore()

renv::install("plyr@1.8.8", prompt = FALSE)
renv::install("tidyverse@2.0.0", prompt = FALSE)
renv::install("tidyr@1.3.0", prompt = FALSE)
renv::install("readr@2.1.4", prompt = FALSE)
renv::install("purrr@1.0.2", prompt = FALSE)
renv::install("forcats@1.0.0", prompt = FALSE)
renv::install("ggplot2@3.4.2", prompt = FALSE)
renv::install("dplyr@1.1.2", prompt = FALSE)
renv::install("tibble@3.2.1", prompt = FALSE)
renv::install("stringr@1.5.0", prompt = FALSE)
renv::install("vroom@1.6.3", prompt = FALSE)
renv::install("svglite@2.1.1", prompt = FALSE)
renv::install("writexl@1.4.2", prompt = FALSE)
renv::install("readxl@1.4.3", prompt = FALSE)
renv::install("styler@1.10.1", prompt = FALSE)
renv::install("viridis@0.6.4", prompt = FALSE)
renv::install("ordinal@2022.11-16", prompt = FALSE)
renv::install("lmtest@0.9-40", prompt = FALSE)

################################## Load packages
## Appears tidyverse does not play nicely with renv, have to call packages I need manually
lapply(
  c("renv", # For project management
    "plyr", # Data wrangling, part of tidyverse but not automatically loaded with it. Always load plyr before dply to avoid known issues
    "ggplot2", # Tidyverse. Data wrangling, processing and presentation.
    "dplyr", # Tidyverse. Data wrangling, processing and presentation.
    "tidyr", # Tidyverse. Data wrangling, processing and presentation.
    "readr", # Tidyverse. Data wrangling, processing and presentation.
    "purrr", # Tidyverse. Data wrangling, processing and presentation.
    "tibble", # Tidyverse. Data wrangling, processing and presentation.
    "stringr", # Tidyverse. Data wrangling, processing and presentation.
    "forcats", # Tidyverse. Data wrangling, processing and presentation.
    "vroom", # Faster data wrangling
    "lubridate", # Working with dates, part of tidyverse but not automatically loaded with it
    "svglite", # To make svg files with ggsave
    "writexl", # Writing excel files
    "readxl", # Reading excel files
    "styler", # Fixing formating to tidyverse styleguide
    "viridis", # Color palette
    "ordinal", # For machine learning
    "lmtest" # for likelyhood ratio test
    #"VGAM" # For machine learning
  ),
  library,
  character.only = TRUE
)

################################## Snapshot
#renv::snapshot() # Only gets done once unless more packages are installed, commented out afterwards

################################## Import data

raw_input_data <- read_excel("./input/input_dataframe.xlsx")

################################## Calculate % of how many credits students could have gotten on the pharmacy program since the HT22 semester started
input_df <- raw_input_data %>% 
  mutate("Percentage_Of_Possible_Credits_Achieved" = (Credits_2023_09_04 / Max_Achievable_Credits) * 100)

################################## Participant exlusions
## Also, change the student with a Swedish grade of 0 to NA.
## This student entered the pharmacy program through an unusual method via a residential college for adult education
## Normally, you can't enter any tertiary education in Sweden with a grade of 0, so this data point doesn't make any sense
input_df_without_excluded <- input_df %>% 
  dplyr::filter(Excluded == FALSE) %>% 
  dplyr::select(-Excluded, - Reason_For_Exclusion) %>% 
  dplyr::mutate("Mean_Swedish_Grade" = na_if(Mean_Swedish_Grade, 0))

# How many participants were excluded?
paste(nrow(input_df) - nrow(input_df_without_excluded), 
      "students were excluded")

# What was the cause for exklusion?
input_df %>% 
  count(Reason_For_Exclusion)

################################## Descriptive statistics
## What proportion of students have 100% of possible credits?
paste0(
  signif(
    x = input_df_without_excluded %>% 
  count(Percentage_Of_Possible_Credits_Achieved) %>% 
  mutate("sum_n" = sum(n)) %>% 
  mutate("percentage_of_whole" = (n / sum(n)) * 100) %>% 
  dplyr::filter(Percentage_Of_Possible_Credits_Achieved == 100) %>% 
  dplyr::select(percentage_of_whole) %>% 
  deframe(),
  digits = 2),
"% of students got 100% of possible credits in the pharmacy program"
)

## How many students had 60 as maximum possible credits in pharmacy program, and how many had a different number?
input_df_without_excluded %>% 
  count(Max_Achievable_Credits)

# Add this as a variable
input_df_without_excluded <- input_df_without_excluded %>% 
  mutate("100_percent_of_possible_credits_achieved" = Percentage_Of_Possible_Credits_Achieved == 100)

# Add variable about if the student had 60 as maximum achievable credits or not
input_df_without_excluded <- input_df_without_excluded %>% 
  mutate("60_credits_as_maximum_achievable_first_year_pharmacy_program" = Max_Achievable_Credits == 60)

## Stacked barplot
# Selection variable of columns for stacked barplot
barplot_selection <- c("60_credits_as_maximum_achievable_first_year_pharmacy_program",
                       "Swedish_Grade_SAS",
                       "Swedish_Grade_SFL",
                       "Swedish_Grade_Old",
                       "CEFR_Test_Taken_By_Student",
                       "Language_Assessments_Performed",
                       "100_percent_of_possible_credits_achieved")

# Stacked barplot
barplot_df <- input_df_without_excluded %>%
  tidyr::pivot_longer(cols = all_of(barplot_selection),
                      names_to = "Variable",
                      values_to = "Value") %>%
  dplyr::group_by(Variable, Value) %>%
  summarize(Count = n())

# Calculate the percentage of TRUE values for each variable
percentage_df <- barplot_df %>%
  group_by(Variable) %>%
  summarize(Total = sum(Count)) %>%
  left_join(barplot_df, by = "Variable") %>%
  mutate(Percentage = ifelse(Value == TRUE, Count / Total * 100, 0))

barplot_participant_data <- ggplot(percentage_df, aes(x = Variable, y = Count, fill = Value)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_bar(stat = "identity") +
  geom_text(data = percentage_df %>% filter(Percentage > 0), aes(label = scales::percent(Percentage / 100, accuracy = 0.1), y = Count + 0.5), vjust = -0.5) +  # Add percentage labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(percentage_df$Count), by = 5), minor_breaks = seq(0, max(percentage_df$Count), by = 1))

print(barplot_participant_data)

ggsave(filename = "./output/figures/barplot_participant_data.pdf",
       plot = barplot_participant_data)

################################## Create new variables based on other variables as needed
# Create ordinal variable based on median language test results.
# Also create based on summed values
processed_data <- input_df_without_excluded %>% 
  dplyr::mutate("CEFR_Test_Grammar_And_Vocabulary_Coded" = CEFR_Test_Grammar_And_Vocabulary) %>% 
  dplyr::mutate("CEFR_Test_Grammar_And_Vocabulary_Coded" = str_replace(CEFR_Test_Grammar_And_Vocabulary_Coded,
                                                        ">C1",
                                                        "1")) %>% 
  dplyr::mutate("CEFR_Test_Grammar_And_Vocabulary_Coded" = str_replace(CEFR_Test_Grammar_And_Vocabulary_Coded,
                                                        "C1",
                                                        "2")) %>%
  dplyr::mutate("CEFR_Test_Grammar_And_Vocabulary_Coded" = str_replace(CEFR_Test_Grammar_And_Vocabulary_Coded,
                                                        "B2",
                                                        "3")) %>%
  dplyr::mutate("CEFR_Test_Grammar_And_Vocabulary_Coded" = str_replace(CEFR_Test_Grammar_And_Vocabulary_Coded,
                                                        "B1",
                                                        "4")) %>%
  dplyr::mutate("CEFR_Test_Grammar_And_Vocabulary_Coded" = as.integer(CEFR_Test_Grammar_And_Vocabulary_Coded)) %>% 
  dplyr::mutate("CEFR_Test_Reading_Comprehension_Coded" = CEFR_Test_Reading_Comprehension) %>% 
  dplyr::mutate("CEFR_Test_Reading_Comprehension_Coded" = str_replace(CEFR_Test_Reading_Comprehension_Coded,
                                              ">C1",
                                              "1")) %>% 
  dplyr::mutate("CEFR_Test_Reading_Comprehension_Coded" = str_replace(CEFR_Test_Reading_Comprehension_Coded,
                                              "C1",
                                              "2")) %>%
  dplyr::mutate("CEFR_Test_Reading_Comprehension_Coded" = str_replace(CEFR_Test_Reading_Comprehension_Coded,
                                              "B2",
                                              "3")) %>%
  dplyr::mutate("CEFR_Test_Reading_Comprehension_Coded" = str_replace(CEFR_Test_Reading_Comprehension_Coded,
                                              "B1",
                                              "4")) %>%
  dplyr::mutate("CEFR_Test_Reading_Comprehension_Coded" = as.integer(CEFR_Test_Reading_Comprehension_Coded)) %>% 
  dplyr::mutate("CEFR_Test_Listening_Comprehension_Coded" = CEFR_Test_Listening_Comprehension) %>% 
  dplyr::mutate("CEFR_Test_Listening_Comprehension_Coded" = str_replace(CEFR_Test_Listening_Comprehension_Coded,
                                                    ">C1",
                                                    "1")) %>% 
  dplyr::mutate("CEFR_Test_Listening_Comprehension_Coded" = str_replace(CEFR_Test_Listening_Comprehension_Coded,
                                                    "C1",
                                                    "2")) %>%
  dplyr::mutate("CEFR_Test_Listening_Comprehension_Coded" = str_replace(CEFR_Test_Listening_Comprehension_Coded,
                                                    "B2",
                                                    "3")) %>%
  dplyr::mutate("CEFR_Test_Listening_Comprehension_Coded" = str_replace(CEFR_Test_Listening_Comprehension_Coded,
                                                    "B1",
                                                    "4")) %>%
  dplyr::mutate("CEFR_Test_Listening_Comprehension_Coded" = as.integer(CEFR_Test_Listening_Comprehension_Coded)) %>% 
  rowwise() %>%
  mutate("CEFR_Test_Median_Coded" = median(c(CEFR_Test_Grammar_And_Vocabulary_Coded, CEFR_Test_Reading_Comprehension_Coded, CEFR_Test_Listening_Comprehension_Coded))) %>% 
  mutate("CEFR_Test_Summed_Coded" = sum(c(CEFR_Test_Grammar_And_Vocabulary_Coded, CEFR_Test_Reading_Comprehension_Coded, CEFR_Test_Listening_Comprehension_Coded))) %>% 
  mutate(CEFR_Test_Summed_Coded = ifelse(is.na(CEFR_Test_Grammar_And_Vocabulary_Coded),# All values should either exist or all be NA, but since we sum best to ensure we don't get misleading sums
                                        NA,
                                        CEFR_Test_Summed_Coded)) %>% 
  mutate(CEFR_Test_Summed_Coded = ifelse(is.na(CEFR_Test_Reading_Comprehension_Coded),
                                         NA,
                                         CEFR_Test_Summed_Coded)) %>% 
  mutate(CEFR_Test_Summed_Coded = ifelse(is.na(CEFR_Test_Listening_Comprehension_Coded),
                                         NA,
                                         CEFR_Test_Summed_Coded))

# Create median and summed value for Language_Assessment_Written_Communication and Language_Assessment_Oral_Communication so that this subjective assessment as a whole can be correlated with other variables.
# The variables need to be re-scaled to the same scale first.
processed_data <- processed_data %>% 
  mutate("Language_Assessment_Oral_Communication_rescaled" = scale(Language_Assessment_Oral_Communication, 
                        center = min(processed_data$Language_Assessment_Oral_Communication, 
                                               na.rm = TRUE), 
                        scale = max(processed_data$Language_Assessment_Oral_Communication, na.rm = TRUE) - min(processed_data$Language_Assessment_Oral_Communication, na.rm = TRUE))) %>% 
  mutate("Language_Assessment_Written_Communication_rescaled" = scale(Language_Assessment_Written_Communication, 
                                     center = min(processed_data$Language_Assessment_Written_Communication, 
                                                  na.rm = TRUE), 
                                     scale = max(processed_data$Language_Assessment_Written_Communication, na.rm = TRUE) - min(processed_data$Language_Assessment_Written_Communication, na.rm = TRUE))) %>% 
  rowwise() %>%
  mutate("Language_Assessment_Median_Rescaled" = median(c(Language_Assessment_Oral_Communication_rescaled, Language_Assessment_Written_Communication_rescaled))) %>%
  mutate("Language_Assessment_Summed" = sum(c(Language_Assessment_Oral_Communication, Language_Assessment_Written_Communication))) %>% 
  mutate(Language_Assessment_Summed = ifelse(is.na(Language_Assessment_Oral_Communication), # All values should either exist or all be NA, but since we sum best to ensure we don't get misleading sums
                                        NA,
                                        Language_Assessment_Summed)) %>% 
  mutate(Language_Assessment_Summed = ifelse(is.na(Language_Assessment_Written_Communication),
                                        NA,
                                        Language_Assessment_Summed))

# Create factor variable named for type of Swedish course
processed_data <- processed_data %>% 
  mutate("SV" = Swedish_Grade_SAS) %>% 
  mutate("SV" = str_replace(SV, "FALSE", "SFL")) %>% 
  mutate("SV" = str_replace(SV, "TRUE", "SAS")) %>% 
  mutate("SV" = as.factor(SV))

# Create factor variable named for Language_Assessment_Oral_Communication
processed_data <- processed_data %>% 
  mutate("Oral_As_Factor" = factor(Language_Assessment_Oral_Communication, levels = c("1", "2")))

# Dichotomise language assessment into B and C level
# This could be done in many ways
# Based on the distribution in our data, the following makes sense:
# The coded median is 1 or 2 = C
# The coded median is 3 or 4 = B
processed_data <- processed_data %>% 
  dplyr::mutate(Dichotomised_CEFR_Test = ifelse(CEFR_Test_Median_Coded <= 2, "C", "B")) %>% 
  dplyr::mutate(Dichotomised_CEFR_Test = as.factor(Dichotomised_CEFR_Test))

# Synthesise a new variable based on the sum of the language tests and assessments
# Only do this for students where we have complete test and assessment data, otherwise the sum will be misleading!
processed_data <- processed_data %>% 
  dplyr::mutate(Summed_Language_Tests_And_Assessments = sum(CEFR_Test_Summed_Coded, Language_Assessment_Summed)) %>% 
  dplyr::mutate(Summed_Language_Tests_And_Assessments = ifelse(is.na(CEFR_Test_Summed_Coded), 
                                                               NA, 
                                                               Summed_Language_Tests_And_Assessments)) %>% 
  dplyr::mutate(Summed_Language_Tests_And_Assessments = ifelse(is.na(Language_Assessment_Summed), 
                                                               NA, 
                                                               Summed_Language_Tests_And_Assessments))

# Create ordinal variable for SV (a bit weird statistically, but there's only 2 possible values so the lack of a natural order is ok)
processed_data <- processed_data %>% 
  mutate("Swedish_Grade_Type_As_Ordinal_Variable" = SV) %>% 
  mutate("Swedish_Grade_Type_As_Ordinal_Variable" = str_replace(Swedish_Grade_Type_As_Ordinal_Variable, "SFL", "1")) %>% 
  mutate("Swedish_Grade_Type_As_Ordinal_Variable" = str_replace(Swedish_Grade_Type_As_Ordinal_Variable, "SAS", "2")) %>% 
  mutate("Swedish_Grade_Type_As_Ordinal_Variable" = as.integer(Swedish_Grade_Type_As_Ordinal_Variable))
  

################################## PCA
quantitative_variables_for_pca <- processed_data %>% 
  dplyr::select(Pseudonym,
                SV,
                Percentage_Of_Possible_Credits_Achieved,
                Mean_Swedish_Grade,
                CEFR_Test_Grammar_And_Vocabulary_Coded,
                CEFR_Test_Reading_Comprehension_Coded,
                CEFR_Test_Listening_Comprehension_Coded,
                Language_Assessment_Written_Communication,
                Language_Assessment_Oral_Communication)

# Perform PCA
pca_result <- quantitative_variables_for_pca %>%
  column_to_rownames("Pseudonym") %>% 
  drop_na() %>% 
  select(-SV) %>%  # Exclude the color variable from PCA which denotes what type of Swedish grade students had
  prcomp(., center = TRUE, scale. = TRUE)  # Perform PCA with mean-centering and scaling

# Calculate the proportion of variance explained by each PC
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Create a data frame with the PCA results
pca_data <- as.data.frame(pca_result$x)

# Add the "SV" column back to the PCA data frame
pca_data$SV <- quantitative_variables_for_pca %>%
  drop_na() %>% 
  select(Pseudonym, SV) %>% 
  deframe()

# Create a PCA plot using ggplot2
pca_plot <- ggplot(pca_data, aes(PC1, PC2, color = SV)) +
  scale_color_brewer(palette = "Paired") +
  geom_point() +
  labs(
    x = paste0("PC1 (", round(100 * variance_explained[1], 2), "% of variance explained)"),
    y = paste0("PC2 (", round(100 * variance_explained[2], 2), "% of variance explained)"),
    color = "SV"
  ) +
  theme_minimal() +
  labs(title = "PCA based on multiple variables, see script")

print(pca_plot)
ggsave(filename = "./output/figures/PCA.pdf",
       plot = pca_plot)

################################## Plots for Swedish grades from upper secondary school
# Stacked barplot
expanded_grid_grades_plot <- expand.grid(SV = c("SFL", "SAS"),
                                         Mean_Swedish_Grade = processed_data$Mean_Swedish_Grade) %>% 
  dplyr::filter(Mean_Swedish_Grade != "NA")

grades_plot_df <- processed_data %>% 
  group_by(SV) %>% 
  count(Mean_Swedish_Grade) %>% 
  drop_na() %>% 
  dplyr::full_join(expanded_grid_grades_plot,
                   by = c("SV", "Mean_Swedish_Grade")) %>% 
  dplyr::mutate(n = replace_na(n, 0)) %>% 
  distinct()

grades_stacked_barplot <- ggplot(data = grades_plot_df,
       aes(x = SV,
           y = n,
           fill = Mean_Swedish_Grade)) +
  geom_bar(stat = "identity") + 
  labs(fill = "Grades") +
  scale_fill_viridis_c() +
  scale_y_continuous(breaks = seq(0, 
                                  45, 
                                  by = 5),
                     minor_breaks = seq(0, 
                                        45, 
                                        by = 1),
                     limits = c(0,
                                45)) +
  theme_linedraw()

print(grades_stacked_barplot)

ggsave(filename = "./output/figures/grades_stacked_barplot.pdf",
       plot = grades_stacked_barplot,
       width = 19.05/2,
       height = 22.23/2,
       unit = "cm")

################################## Plots for language tests
language_test_plot_df <- processed_data %>% 
  dplyr::select(Pseudonym ,SV, CEFR_Test_Grammar_And_Vocabulary_Coded, CEFR_Test_Reading_Comprehension_Coded, CEFR_Test_Listening_Comprehension_Coded) %>% 
  pivot_longer(cols = contains("coded"),
               names_to = "sub_test",
               values_to = "test_Coded_score") %>% 
  group_by(SV, sub_test) %>% 
  count(test_Coded_score) %>% 
  drop_na()

language_test_plot_df <- processed_data %>% 
  dplyr::select(Pseudonym ,SV, CEFR_Test_Grammar_And_Vocabulary, CEFR_Test_Reading_Comprehension, CEFR_Test_Listening_Comprehension) %>% 
  pivot_longer(cols = 3:5,
               names_to = "sub_test",
               values_to = "test_score") %>% 
  dplyr::mutate(test_score = str_replace(test_score, ">C1", "C2")) %>% 
  mutate(test_score = factor(test_score, levels = c("C2", "C1", "B2", "B1"))) %>% 
  group_by(SV, sub_test) %>% 
  count(test_score) %>% 
  drop_na()

language_test_plot <- ggplot(data = language_test_plot_df,
       aes(x = SV,
           y = n,
           fill = test_score)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_bar(stat = "identity") + 
  #scale_fill_viridis_c() +
  scale_y_continuous(breaks = seq(0, 
                                  25, 
                                  by = 5),
                     minor_breaks = seq(0, 
                                        25, 
                                        by = 1),
                     limits = c(0,
                                25)) +
  theme_linedraw() +
  facet_wrap(vars(sub_test))

print(language_test_plot)

ggsave(filename = "./output/figures/language_test_plot.pdf",
       plot = language_test_plot,
       width = 19.05/2,
       height = 22.23/2,
       unit = "cm")

################################## Plots for language assessment
# Both
language_assessment_plot_df <- processed_data %>% 
  dplyr::select(Pseudonym ,SV, Language_Assessment_Oral_Communication, Language_Assessment_Written_Communication) %>% 
  pivot_longer(cols = 3:4,
               names_to = "sub_assessment",
               values_to = "assessment_score") %>% 
  group_by(SV, sub_assessment) %>% 
  mutate("assessment_score" = as.factor(assessment_score)) %>% 
  count(assessment_score) %>% 
  drop_na()

language_assessment_plot <- ggplot(data = language_assessment_plot_df,
                             aes(x = SV,
                                 y = n,
                                 fill = assessment_score)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_bar(stat = "identity") + 
  #scale_fill_viridis_c() +
  scale_y_continuous(breaks = seq(0, 
                                  45, 
                                  by = 5),
                     minor_breaks = seq(0, 
                                        45, 
                                        by = 1),
                     limits = c(0,
                                45)) +
  theme_linedraw() +
  facet_wrap(vars(sub_assessment))

print(language_assessment_plot)

ggsave(filename = "./output/figures/language_assessment_plot.pdf",
       plot = language_assessment_plot)

# Separately
language_oral_plot_df <- processed_data %>% 
  dplyr::select(Pseudonym ,SV, Language_Assessment_Oral_Communication) %>% 
  group_by(SV) %>% 
  mutate("Language_Assessment_Oral_Communication" = as.factor(Language_Assessment_Oral_Communication)) %>% 
  count(Language_Assessment_Oral_Communication) %>% 
  drop_na()

language_oral_plot <- ggplot(data = language_oral_plot_df,
                                   aes(x = SV,
                                       y = n,
                                       fill = Language_Assessment_Oral_Communication)) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Dark2") +
  #scale_fill_viridis_c() +
  scale_y_continuous(breaks = seq(0, 
                                  45, 
                                  by = 5),
                     minor_breaks = seq(0, 
                                        45, 
                                        by = 1),
                     limits = c(0,
                                45)) +
  theme_linedraw()

print(language_oral_plot)

ggsave(filename = "./output/figures/language_oral_plot.pdf",
       plot = language_oral_plot,
       width = 19.05/2,
       height = 22.23/2,
       unit = "cm")

language_essay_plot_df <- processed_data %>% 
  dplyr::select(Pseudonym ,SV, Language_Assessment_Written_Communication) %>% 
  group_by(SV) %>% 
  mutate("Language_Assessment_Written_Communication" = as.factor(Language_Assessment_Written_Communication)) %>% 
  count(Language_Assessment_Written_Communication) %>% 
  drop_na()

language_essay_plot_df <- ggplot(data = language_essay_plot_df,
                             aes(x = SV,
                                 y = n,
                                 fill = Language_Assessment_Written_Communication)) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Dark2") +
  #scale_fill_viridis_c() +
  scale_y_continuous(breaks = seq(0, 
                                  45, 
                                  by = 5),
                     minor_breaks = seq(0, 
                                        45, 
                                        by = 1),
                     limits = c(0,
                                45)) +
  theme_linedraw()

print(language_essay_plot_df)

ggsave(filename = "./output/figures/language_essay_plot.pdf",
       plot = language_essay_plot_df,
       width = 19.05/2,
       height = 22.23/2,
       unit = "cm")

# Descriptive statistics of percentages
figure_2BCD_percentages <- processed_data %>%
  dplyr::select(all_of(c("SV", "CEFR_Test_Grammar_And_Vocabulary_Coded", "CEFR_Test_Reading_Comprehension_Coded", "CEFR_Test_Listening_Comprehension_Coded", "Language_Assessment_Written_Communication", "Language_Assessment_Oral_Communication"))) %>% 
  pivot_longer(cols = -SV, names_to = "Test", values_to = "Score") %>% 
  drop_na() %>% 
  group_by(SV, Test, Score) %>% 
  count() %>%
  group_by(SV, Test) %>%  # Calculate total count for each test
  mutate(total_count = sum(n)) %>% 
  ungroup() %>% 
  mutate(percentage = n / total_count * 100) %>%  # Calculate percentage of total count
  mutate("percentage_rounded" = round(percentage))

# Percentage of students with C1 or C2 CEFR score
figure_2BCD_percentages %>% 
  dplyr::filter(Test %in% c("CEFR_Test_Grammar_And_Vocabulary_Coded", "CEFR_Test_Reading_Comprehension_Coded", "CEFR_Test_Listening_Comprehension_Coded")) %>% 
  dplyr::filter(Score == 1 | Score == 2) %>% 
  dplyr::group_by(SV, Test) %>% 
  dplyr::summarise(Percentage_C1_Minimum = sum(percentage)) %>% 
  dplyr::mutate("percentage_rounded" = round(Percentage_C1_Minimum))

# Percentage of students with B1 CEFR score
figure_2BCD_percentages %>% 
  dplyr::filter(Test %in% c("CEFR_Test_Grammar_And_Vocabulary_Coded", "CEFR_Test_Reading_Comprehension_Coded", "CEFR_Test_Listening_Comprehension_Coded")) %>% 
  dplyr::filter(Score == 4) %>% 
  dplyr::group_by(SV, Test) %>% 
  dplyr::summarise(Percentage_B1 = sum(percentage)) %>% 
  dplyr::mutate("percentage_rounded" = round(Percentage_B1))

# Percentage of students, looking at assessment scores
figure_2BCD_percentages %>% 
  dplyr::filter(Test %in% c("Language_Assessment_Oral_Communication", "Language_Assessment_Written_Communication"))

################################## Descriptive statistics such as median and IQR
print("Descriptive statistics for admission grades by SV group")
processed_data %>% 
  select(Mean_Swedish_Grade, SV, Pseudonym) %>% 
  drop_na() %>% 
  group_by(SV) %>% 
  summarise(Median_Admission_Grades = median(Mean_Swedish_Grade),
            Mean_Admission_Grades = mean(Mean_Swedish_Grade),
            IQR_Admission_Grades = IQR(Mean_Swedish_Grade)) # Default for R is algorithm type 7

print("Descriptive statistics for having B1 or B2 language test result in at least one sub-test")
processed_data %>% 
  select(CEFR_Test_Listening_Comprehension_Coded, CEFR_Test_Reading_Comprehension_Coded ,CEFR_Test_Grammar_And_Vocabulary_Coded, SV, Pseudonym) %>% 
  drop_na() %>% 
  group_by(Pseudonym) %>% 
  mutate(Max_Test_Coded_Score = max(CEFR_Test_Listening_Comprehension_Coded, CEFR_Test_Reading_Comprehension_Coded ,CEFR_Test_Grammar_And_Vocabulary_Coded)) %>% 
  group_by(SV) %>% 
  count(Max_Test_Coded_Score) %>% 
  mutate(Total_n = sum(n)) %>% 
  group_by(SV, Max_Test_Coded_Score) %>% 
  mutate(Percentage_Worst_Test_Score = round((n / Total_n) * 100))

# 37% of SAS students have B2 in at least 1 test. 
# Since 37% of SAS students have B2 in the listening comprehension test, this should mean that all students who have B2 in another test also has B2 in the listening comprehension test.
processed_data %>% 
  select(CEFR_Test_Listening_Comprehension_Coded, CEFR_Test_Reading_Comprehension_Coded ,CEFR_Test_Grammar_And_Vocabulary_Coded, SV, Pseudonym) %>% 
  drop_na() %>% 
  group_by(Pseudonym) %>% 
  mutate(Max_Test_Coded_Score = max(CEFR_Test_Listening_Comprehension_Coded, CEFR_Test_Reading_Comprehension_Coded ,CEFR_Test_Grammar_And_Vocabulary_Coded)) %>% 
  dplyr::filter(Max_Test_Coded_Score == 4)

################################## Perform statistical tests and adjust p-values for multiple testing
# Custom function to perform a statistical test
perform_statistical_test <- function(data, 
                                     dependent_variable, 
                                     independent_variable, 
                                     test_method,
                                     filter_column = NULL,
                                     filter_values = NULL) {
  
  # Check if specified variables exist
  if (!dependent_variable %in% colnames(data) || !independent_variable %in% colnames(data)) {
    stop("The specified columns do not exist in the dataframe.")
  }
  
  # Filter the data if filter_column and filter_values are provided
  if (!is.null(filter_column) && !is.null(filter_values)) {
    filtered_data <- data[data[[filter_column]] %in% filter_values, ]
    filtered_rows <- nrow(data) - nrow(filtered_data)
    if (filtered_rows > 0) {
      message(paste("Filtered out", filtered_rows, "rows due to missing values in", dependent_variable, "or", independent_variable))
    }
  } else {
    # Proceed without filtering if filter_column or filter_values are not provided
    filtered_data <- data
  }
  
  if (test_method == "spearman") {
    test_result <- cor.test(x = filtered_data[[independent_variable]], 
                            y = filtered_data[[dependent_variable]], 
                            method = "spearman",
                            exact = FALSE)
    
    # Extract Spearman's rho value
    rho_value <- test_result$estimate
    
  } else if (test_method == "wilcox") {
    test_result <- wilcox.test(as.numeric(filtered_data[[dependent_variable]]) ~ filtered_data[[independent_variable]],
                               exact = FALSE)
    
    # For the Wilcoxon test, rho_value doesn't exist, so set it to NA
    rho_value <- NA
  } else {
    stop("Invalid test_method specified.")
  }
  
  return(list(test_result, rho_value))  # Return a list with both the test result and rho value
}

# Function for performing multiple tests
perform_multiple_tests <- function(test_list, method = "fdr") {
  results <- tibble()  # Initialize as a tibble
  
  for (test_info in test_list) {
    data <- get(test_info$data_df)  # Get the dataframe using the name
    dependent_variable <- test_info$dependent_variable
    independent_variable <- test_info$independent_variable
    test_method <- test_info$test_method
    filter_column <- test_info$filter_column
    filter_values <- test_info$filter_values
    color_variable <- test_info$color_variable
    min_scale <- test_info$min_scale
    max_scale <- test_info$max_scale
    breaks_gap <- test_info$breaks_gap
    data_df <- test_info$data_df  # Get the dataframe name
    
    # Apply filtering if filter_column and filter_values are provided
    if (!is.null(filter_column) && !is.null(filter_values)) {
      # Check if the column exists
      if (!(filter_column %in% colnames(data))) {
        stop("The filter_column does not exist in the dataframe.")
      }
      
      # Ensure that filter_values is a list of vectors
      if (!is.list(filter_values)) {
        filter_values <- list(filter_values)
      }
      
      # Filter the dataframe
      filtered_data <- data[data[[filter_column]] %in% unlist(filter_values), ]
    } else {
      filtered_data <- data
      filter_column <- NA  # Assign NA instead of NULL
      filter_values <- NA  # Assign NA instead of NULL
    }
    
    # Filter out rows with NA values in dependent and/or independent variable columns
    filtered_data <- filtered_data[complete.cases(filtered_data[[dependent_variable]], filtered_data[[independent_variable]]), ]
    
    # Check if any rows were filtered out due to missing values
    if (nrow(filtered_data) < nrow(data)) {
      cat("Warning: Missing values detected in the filtered data for test:",
          test_method,
          ".",
          "Filtered out",
          nrow(data) - nrow(filtered_data),
          "rows.\n")
    }
    
    result_list <- perform_statistical_test(data = filtered_data, 
                                            dependent_variable = dependent_variable, 
                                            independent_variable = independent_variable, 
                                            test_method = test_method)
    
    # Extract test_result and rho_value from the list
    test_result <- result_list[[1]]
    rho_value <- result_list[[2]]
    
    # Store the results in a tibble
    result_row <- tibble(
      Test = test_method,
      Dependent_Variable = dependent_variable,
      Independent_Variable = independent_variable,
      Original_P_Value = test_result$p.value,
      Spearman_Rho = rho_value,
      n = nrow(filtered_data),  # Number of observations
      Filter_Column = filter_column,
      Filter_Values = paste(unlist(filter_values), collapse = ", "),  # Combine filter_values
      Color_Variable = color_variable,
      Min_Scale = min_scale,
      Max_Scale = max_scale,
      Breaks_Gap = breaks_gap,
      data_df = data_df
    )
    
    results <- bind_rows(results, result_row)
  }
  
  # Adjust p-values for multiple testing using the specified method
  results$Adjusted_P_Value <- p.adjust(results$Original_P_Value, method = method)
  
  return(results)
}

#### Plotting functions
## Function for spearman
plot_spearman_results <- function(data,
                                  dependent_variable, 
                                  independent_variable,
                                  color_variable,
                                  Adjusted_P_Value,
                                  rho,
                                  min_scale = NULL,
                                  max_scale = NULL,
                                  breaks_gap = NULL,
                                  filter_column = NA,
                                  filter_values = NA) {
  
  if (!is.na(filter_column) && length(filter_values) > 0) {
    plot_name <- paste0("spearman_plot_", dependent_variable, "_vs_", independent_variable, "_filtered_by_", paste(filter_values, collapse = "_"))
    filter_values <- filter_values %>% 
      str_split(pattern = ", ") %>%
      unlist()
    filtered_data <- data %>%
      dplyr::filter(!!sym(filter_column) %in% filter_values)
  } else {
    plot_name <- paste0("spearman_plot_", dependent_variable, "_vs_", independent_variable)
    filtered_data <- data
  }
  
  # Removing NA values in plotting variables to not get errors for it later when plotting
  filtered_data <- filtered_data %>% dplyr::filter(!is.na(!!sym(dependent_variable))) %>% 
    dplyr::filter(!is.na(!!sym(independent_variable)))
  
  gg <- ggplot(filtered_data,
               aes(x = !!rlang::sym(independent_variable),
                   y = !!rlang::sym(dependent_variable),
                   color = !!sym(color_variable))) +
    geom_count(aes(color = !!sym(color_variable)),
               alpha = 0.6) + # Geom_count helps with overplotting, but sometimes they are drawn in the wrong order. Reduced alpha helps detect this.
    scale_size_continuous(breaks = 1:15) + # In our data, 15 is the max counts for the spearman analysis
    expand_limits(size = 15) + # Ensures same scale across samples
    ggtitle(paste0(dependent_variable, 
                   "\ncorrelated with\n", 
                   independent_variable,
                   "\n",
                   "Spearman's rho = ",
                   signif(rho, digits = 3),
                   "\n",
                   "fdr-corrected p = ", 
                   signif(Adjusted_P_Value, digits = 3),
                   " with spearman")) +
    theme_classic()
  
  # Check if color_variable is "SV" and assign manual colors accordingly
  if (color_variable == "SV") {
    gg <- gg + scale_color_manual(values = c("SAS" = "#4BC26C", "SFL" = "#440D54"))
  }
  
  # Apply scale adjustments only if provided
  if (!is.na(min_scale) && !is.na(max_scale) && !is.na(breaks_gap)) {
    gg <- gg + scale_y_continuous(breaks = seq(min_scale, 
                                               max_scale, 
                                               by = breaks_gap),
                                  limits = c(min_scale,
                                             max_scale))
  }
  
  print(gg)
  
  assign(plot_name, gg, envir = .GlobalEnv)
  
  plot_filename_pdf <- paste0("./output/figures/Spearman_", independent_variable, "_correlated_with_", dependent_variable, "_filtered_by_", paste(filter_values, collapse = "_"), ".pdf")
  if (!is.na(plot_filename_pdf)) {
    ggsave(filename = plot_filename_pdf, 
           plot = gg)
  }
  
  plot_filename_svg <- paste0("./output/figures/Spearman_", independent_variable, "_correlated_with_", dependent_variable, "_filtered_by_", paste(filter_values, collapse = "_"), ".svg")
  if (!is.na(plot_filename_svg)) {
    ggsave(filename = plot_filename_svg, 
           plot = gg)
  }
  
  invisible(NULL)  # Return nothing
}

## Function for wilcox
plot_wilcoxon_results <- function(data,
                                  dependent_variable, 
                                  independent_variable,
                                  color_variable,
                                  Adjusted_P_Value,
                                  min_scale = NULL,
                                  max_scale = NULL,
                                  breaks_gap = NULL,
                                  filter_column = NA,
                                  filter_values = NA) {
  
  if (!is.na(filter_column) && length(filter_values) > 0) {
    plot_name <- paste0("MannWhitneyU_", independent_variable, "_compared_for_", dependent_variable, "_filtered_by_", paste(filter_values, collapse = "_"))
    filter_values <- filter_values %>% 
      str_split(pattern = ", ") %>%
      unlist()
    filtered_data <- data %>%
      dplyr::filter(!!sym(filter_column) %in% filter_values)
  } else {
    plot_name <- paste0("MannWhitneyU_", independent_variable, "_compared_for_", dependent_variable)
    filtered_data <- data
  }
  
  # Removing NA values in plotting variables to not get errors for it later when plotting
  filtered_data <- filtered_data %>% dplyr::filter(!is.na(!!sym(dependent_variable))) %>% 
    dplyr::filter(!is.na(!!sym(independent_variable)))
  
  gg <- ggplot(filtered_data,
               aes(x = !!rlang::sym(independent_variable),
                   y = !!rlang::sym(dependent_variable))) +
    geom_violin() +
    geom_count(aes(color = !!sym(color_variable)),
               alpha = 0.6) + # Geom_count helps with overplotting, but sometimes they are drawn in the wrong order. Reduced alpha helps detect this.
    scale_size_continuous(breaks = seq(from = 1, to = 23, by = 2)) + #23 is the highest count in the figures using Mann-Whitney U
    expand_limits(size = 40) + # Ensures same scale across samples
    ggtitle(paste0(dependent_variable, 
                   "\ntested between groups in variable:\n", 
                   independent_variable,
                   "\n",
                   "fdr-corrected p = ", 
                   signif(Adjusted_P_Value, digits = 3), 
                   " with Mann-Whitney U")) +
    theme_classic()
  
  # Check if color_variable is "SV" and assign manual colors accordingly
  if (color_variable == "SV") {
    gg <- gg + scale_color_manual(values = c("SAS" = "#4BC26C", "SFL" = "#440D54"))
  }
  
  # Apply scale adjustments only if provided
  if (!is.na(min_scale) && !is.na(max_scale) && !is.na(breaks_gap)) {
    gg <- gg + scale_y_continuous(breaks = seq(min_scale, 
                                               max_scale, 
                                               by = breaks_gap),
                                  limits = c(min_scale,
                                             max_scale))
  }
  
  print(gg)
  
  assign(plot_name, gg, envir = .GlobalEnv)
  
  plot_filename_pdf <- paste0("./output/figures/MannWhitneyU_", independent_variable, "_compared_for_", dependent_variable, "_filtered_by_", paste(filter_values, collapse = "_"), ".pdf")
  if (!is.na(plot_filename_pdf)) {
    ggsave(filename = plot_filename_pdf, 
           plot = gg)
  }
  
  plot_filename_svg <- paste0("./output/figures/MannWhitneyU_", independent_variable, "_compared_for_", dependent_variable, "_filtered_by_", paste(filter_values, collapse = "_"), ".svg")
  if (!is.na(plot_filename_svg)) {
    ggsave(filename = plot_filename_svg, 
           plot = gg)
  }
  
  invisible(NULL)  # Return nothing
}

################################## 2024-04-18 analysis
language_sub_tests_df <- processed_data %>% # Need new dataframe for some analysis
  dplyr::select(Pseudonym, SV, CEFR_Test_Listening_Comprehension_Coded, CEFR_Test_Reading_Comprehension_Coded, CEFR_Test_Grammar_And_Vocabulary_Coded) %>% 
  tidyr::pivot_longer(3:5, names_to = "Language_Sub_Test", values_to = "Language_Test_Score") %>% 
  tidyr::drop_na()

# Define a list of tests to perform
tests_to_perform <- list(
  # Do the students who had Swedish as a second language have different admission grades in Swedish?
  # Figure 2 A
  list(test_method = "wilcox", 
       dependent_variable = "Mean_Swedish_Grade", 
       independent_variable = "SV",
       color_variable = "SV",
       min_scale = 0,
       max_scale = 20,
       breaks_gap = 1,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Do the students who had Swedish as a second language score differently for CEFR listening comprehension?
  # Figure 2 B
  list(test_method = "wilcox", 
       dependent_variable = "CEFR_Test_Listening_Comprehension_Coded", 
       independent_variable = "SV",
       color_variable = "SV",
       min_scale = 1,
       max_scale = 4,
       breaks_gap = 1,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Do the students who had Swedish as a second language score differently for CEFR reading comprehension?
  # Figure 2 B
  list(test_method = "wilcox", 
       dependent_variable = "CEFR_Test_Reading_Comprehension_Coded", 
       independent_variable = "SV",
       color_variable = "SV",
       min_scale = 1,
       max_scale = 4,
       breaks_gap = 1,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Do the students who had Swedish as a second language score differently for CEFR words and grammar?
  # Figure 2 B
  list(test_method = "wilcox", 
       dependent_variable = "CEFR_Test_Grammar_And_Vocabulary_Coded", 
       independent_variable = "SV",
       color_variable = "SV",
       min_scale = 1,
       max_scale = 4,
       breaks_gap = 1,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Do the students who had Swedish as a second language get assessed differently for oral communication?
  # Figure 2 C
  list(test_method = "wilcox", 
       dependent_variable = "Language_Assessment_Oral_Communication", 
       independent_variable = "SV",
       color_variable = "SV",
       min_scale = 1,
       max_scale = 2,
       breaks_gap = 1,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Do the students who had Swedish as a second language get assessed differently for written communication?
  # Figure 2 D
  list(test_method = "wilcox", 
       dependent_variable = "Language_Assessment_Written_Communication", 
       independent_variable = "SV",
       color_variable = "SV",
       min_scale = 1,
       max_scale = 3,
       breaks_gap = 1,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Are the language proficiency scores different between any of the three sub-tests?
  # Appendix, related to figure 2
  list(test_method = "wilcox", 
       dependent_variable = "Language_Test_Score", 
       independent_variable = "Language_Sub_Test",
       color_variable = "SV",
       min_scale = 1,
       max_scale = 3,
       breaks_gap = 1,
       filter_column = "Language_Sub_Test",
       filter_values = c("CEFR_Test_Listening_Comprehension_Coded", "CEFR_Test_Reading_Comprehension_Coded"),
       data_df = "language_sub_tests_df"),
  list(test_method = "wilcox", 
       dependent_variable = "Language_Test_Score", 
       independent_variable = "Language_Sub_Test",
       color_variable = "SV",
       min_scale = 1,
       max_scale = 3,
       breaks_gap = 1,
       filter_column = "Language_Sub_Test",
       filter_values = c("CEFR_Test_Listening_Comprehension_Coded", "CEFR_Test_Grammar_And_Vocabulary_Coded"),
       data_df = "language_sub_tests_df"),
  list(test_method = "wilcox", 
       dependent_variable = "Language_Test_Score", 
       independent_variable = "Language_Sub_Test",
       color_variable = "SV",
       min_scale = 1,
       max_scale = 3,
       breaks_gap = 1,
       filter_column = "Language_Sub_Test",
       filter_values = c("CEFR_Test_Reading_Comprehension_Coded", "CEFR_Test_Grammar_And_Vocabulary_Coded"),
       data_df = "language_sub_tests_df"),
  # Is the Swedish assessment correlated with the Swedish test?
  # Figure 3 A
  list(test_method = "spearman", 
       dependent_variable = "CEFR_Test_Summed_Coded", 
       independent_variable = "Language_Assessment_Summed",
       color_variable = "SV",
       min_scale = NULL,
       max_scale = NULL,
       breaks_gap = NULL,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Are high school grades in Swedish correlated with the Swedish test?
  # Figure 3 B
  list(test_method = "spearman", 
       dependent_variable = "CEFR_Test_Summed_Coded", 
       independent_variable = "Mean_Swedish_Grade",
       color_variable = "SV",
       min_scale = NULL,
       max_scale = NULL,
       breaks_gap = NULL,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Are high school grades in Swedish correlated with the Swedish test?
  # Sub-group analysis with SFL
  # Appendix, related to figure 3
  list(test_method = "spearman", 
       dependent_variable = "CEFR_Test_Summed_Coded", 
       independent_variable = "Mean_Swedish_Grade",
       color_variable = "SV",
       min_scale = NULL,
       max_scale = NULL,
       breaks_gap = NULL,
       filter_column = "SV",
       filter_values = c("SFL"),
       data_df = "processed_data"),
  # Are high school grades in Swedish correlated with the Swedish test?
  # Sub-group analysis with SAS
  # Appendix, related to figure 3
  list(test_method = "spearman", 
       dependent_variable = "CEFR_Test_Summed_Coded", 
       independent_variable = "Mean_Swedish_Grade",
       color_variable = "SV",
       min_scale = NULL,
       max_scale = NULL,
       breaks_gap = NULL,
       filter_column = "SV",
       filter_values = c("SAS"),
       data_df = "processed_data"),
  # Is the Swedish test correlated with performance in the pharmacy program?
  # Figure 4 A
  list(test_method = "spearman", 
       dependent_variable = "Percentage_Of_Possible_Credits_Achieved", 
       independent_variable = "CEFR_Test_Summed_Coded",
       color_variable = "SV",
       min_scale = 0,
       max_scale = 100,
       breaks_gap = 10,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Is the Swedish assessment correlated with performance in the pharmacy program?
  # Figure 4 B
  list(test_method = "spearman", 
       dependent_variable = "Percentage_Of_Possible_Credits_Achieved", 
       independent_variable = "Language_Assessment_Summed",
       color_variable = "SV",
       min_scale = 0,
       max_scale = 100,
       breaks_gap = 10,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Is the summed language tests and assessments correlated with performance in the pharmacy program?
  # Figure 4 C
  list(test_method = "spearman", 
       dependent_variable = "Percentage_Of_Possible_Credits_Achieved", 
       independent_variable = "Summed_Language_Tests_And_Assessments",
       color_variable = "SV",
       min_scale = 0,
       max_scale = 100,
       breaks_gap = 10,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Are high school grades in Swedish correlated with performance in the pharmacy program?
  # Figure 4 D
  list(test_method = "spearman", 
       dependent_variable = "Percentage_Of_Possible_Credits_Achieved", 
       independent_variable = "Mean_Swedish_Grade",
       color_variable = "SV",
       min_scale = 0,
       max_scale = 100,
       breaks_gap = 10,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Do the students who had Swedish as a second language perform differently in the pharmacy program than those who had ordinary Swedish education (either the old or new course)?
  # Figure 5 A
  list(test_method = "wilcox", 
       dependent_variable = "Percentage_Of_Possible_Credits_Achieved", 
       independent_variable = "SV",
       color_variable = "SV",
       min_scale = 0,
       max_scale = 100,
       breaks_gap = 10,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Do the students who had Swedish as a second language perform differently in the pharmacy program than those who had ordinary Swedish education (either the old or new course)?
  # Done this with mann-whitney, but to compare strength of correlation also do spearman.
  # Figure 5 A
#  list(test_method = "spearman", 
#       dependent_variable = "Percentage_Of_Possible_Credits_Achieved", 
#       independent_variable = "Swedish_Grade_Type_As_Ordinal_Variable",
#       color_variable = "SV",
#       min_scale = 0,
#       max_scale = 100,
#       breaks_gap = 10,
#       filter_column = "SV",
#       filter_values = c("SFL", "SAS"),
#       data_df = "processed_data"),
  # Is the Swedish assessment (oral) correlated with performance in the pharmacy program?
  # Figure 5 B
  list(test_method = "wilcox", 
       dependent_variable = "Percentage_Of_Possible_Credits_Achieved", 
       independent_variable = "Oral_As_Factor",
       color_variable = "SV",
       min_scale = 0,
       max_scale = 100,
       breaks_gap = 10,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Is the Swedish assessment (oral) correlated with performance in the pharmacy program?
  # Done this with mann-whitney, but to compare strength of correlation also do spearman.
  # Figure 5 B
#  list(test_method = "spearman", 
#       dependent_variable = "Percentage_Of_Possible_Credits_Achieved", 
#       independent_variable = "Language_Assessment_Oral_Communication",
#       color_variable = "SV",
#       min_scale = 0,
#       max_scale = 100,
#       breaks_gap = 10,
#       filter_column = "SV",
#       filter_values = c("SFL", "SAS"),
#       data_df = "processed_data"),
  # Is the Swedish assessment (written) correlated with performance in the pharmacy program?
  # Appendix, related to figure 5 B
  list(test_method = "spearman", 
       dependent_variable = "Percentage_Of_Possible_Credits_Achieved", 
       independent_variable = "Language_Assessment_Written_Communication",
       color_variable = "SV",
       min_scale = 0,
       max_scale = 100,
       breaks_gap = 10,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data"),
  # Are the students who took the Swedish test assessed differently for their Swedish?
  # Testing the oral assessment since this is where the differences between groups is the most extreme.
  # Appendix figure
  list(test_method = "wilcox", 
       dependent_variable = "Language_Assessment_Oral_Communication", 
       independent_variable = "CEFR_Test_Taken_By_Student",
       color_variable = "SV",
       min_scale = NULL,
       max_scale = NULL,
       breaks_gap = NULL,
       filter_column = "SV",
       filter_values = c("SFL", "SAS"),
       data_df = "processed_data")
)

# Perform multiple tests and adjust p-values using the FDR method
statistical_test_results <- perform_multiple_tests(test_list = tests_to_perform, 
                                                   method = "fdr") %>% 
  mutate("significant_adjusted_pvalue" = Adjusted_P_Value < 0.05)

print(statistical_test_results)

write_xlsx(x = statistical_test_results,
           path = "./output/tables/statistical_test_results.xlsx")

# Loop to plot all tests in tibble
for (i in 1:nrow(statistical_test_results)) {
  dependent_variable <- statistical_test_results$Dependent_Variable[i]
  independent_variable <- statistical_test_results$Independent_Variable[i]
  Adjusted_P_Value <- statistical_test_results$Adjusted_P_Value[i]
  rho <- statistical_test_results$Spearman_Rho[i]
  filter_column <- statistical_test_results$Filter_Column[i]
  filter_values <- statistical_test_results$Filter_Values[i]
  data_df <- statistical_test_results$data_df[i]
  
  # Check if filter_column is NA or if it exists in data
  if (!is.na(filter_column) && !filter_column %in% colnames(get(data_df))) {
    warning("Column specified in filter_column not found in the tibble.")
    next  # Skip to the next iteration if filter_column is not found or NA
  }
  
  # Call the corresponding plotting function based on the test method
  if (statistical_test_results$Test[i] == "spearman") {
    plot_spearman_results(data = get(data_df),
                          dependent_variable = dependent_variable,
                          independent_variable = independent_variable,
                          color_variable = statistical_test_results$Color_Variable[i],
                          Adjusted_P_Value = Adjusted_P_Value,
                          rho = rho,
                          min_scale = statistical_test_results$Min_Scale[i],
                          max_scale = statistical_test_results$Max_Scale[i],
                          breaks_gap = statistical_test_results$Breaks_Gap[i],
                          filter_column = filter_column,
                          filter_values = filter_values)
  } else if (statistical_test_results$Test[i] == "wilcox") {
    plot_wilcoxon_results(data = get(data_df),
                          dependent_variable = dependent_variable,
                          independent_variable = independent_variable,
                          color_variable = statistical_test_results$Color_Variable[i],
                          Adjusted_P_Value = Adjusted_P_Value,
                          min_scale = statistical_test_results$Min_Scale[i],
                          max_scale = statistical_test_results$Max_Scale[i],
                          breaks_gap = statistical_test_results$Breaks_Gap[i],
                          filter_column = filter_column,
                          filter_values = filter_values)
  } else {
    warning("Invalid test method:", statistical_test_results$Test[i])
  }
}

warnings() # It is just the missing values throwing errors

################################## Export data
write_xlsx(x = processed_data,
           path = "./output/tables/processed_data.xlsx")
