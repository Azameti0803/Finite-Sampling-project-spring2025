install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")


# Load libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(dplyr)








#selecting the file containing the dataset
  surveyfilepath <- file.choose()
print(surveyfilepath)  # Display the selected file path

# Extract directory path
surveydirpath <- dirname(surveyfilepath)

# Set working directory
#getwd()
setwd(surveydirpath)

writeLines(surveyfilepath, "surveyfilepath.txt")

# Read the saved file path
surveyfilepath <- readLines("surveyfilepath.txt")

# Load the data
surveyprojectdata <- read.csv(surveyfilepath)
View(surveyprojectdata)


###rename columns



colnames(surveyprojectdata) <- c(
  "timestamp",
  "age_group",
  "gender",
  "driver_category",
  "driving_experience",
  "drive_daily",
  "familiar_with_signs",
  "know_speed_limits",
  "safety_training",
  "aware_pedestrian_rules",
  "know_distracted_penalty",
  "seatbelt_usage",
  "phone_use_last_6mo",
  "speeding_habit",
  "yield_pedestrians",
  "drowsy_driving",
  "safety_campaign_effective",
  "leading_cause",
  "suggestions"
)

View(surveyprojectdata)

str(surveyprojectdata)


# Remove NAs
# Check column names
colnames(surveyprojectdata)

surveyprojectdata<- surveyprojectdata[ , -20]


View(surveyprojectdata)

str(surveyprojectdata)
# Inspect your column
surveyprojectdata$leading_cause

summary(surveyprojectdata)



# Convert to UTF-8 encoding to fix any encoding issues
surveyprojectdata$leading_cause <- iconv(surveyprojectdata$leading_cause, from = "", to = "UTF-8")

# Remove NA and "N/A" responses
clean_leading_cause <- na.omit(surveyprojectdata$leading_cause)
clean_leading_cause <- clean_leading_cause[!tolower(clean_leading_cause) %in% c("n/a", "na")]

# Ensure encoding is clean
clean_leading_cause <- iconv(clean_leading_cause, from = "", to = "UTF-8")

# Create corpus
corpus <- VCorpus(VectorSource(clean_leading_cause))

# Text cleaning
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

# Create Term Document Matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)

# Check all words and their frequencies (optional)
print(word_freq)
hist(word_freq)

# Generate Word Cloud with many words
wordcloud(words = names(word_freq),
          freq = word_freq,
          min.freq = 1,           # ensures even rare words appear
          max.words = 50,        # increase this if needed
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"),
          scale = c(3.5, 0.8))    # adjust scale for better display






# Bar plot of top leading causes (after cleaning)
# Create a data frame of word frequencies
word_freq_df <- data.frame(
  word = names(word_freq),
  freq = word_freq
)

# Load ggplot2 for plotting
library(ggplot2)

# Plot: Top 15 most frequent words (adjust number as needed)
ggplot(head(word_freq_df, 15), aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Most Commonly Mentioned Leading Causes of Road Accidents",
       x = "Cause",
       y = "Frequency") +
  theme_minimal(base_size = 13)




# Create a new column for categorized causes
surveyprojectdata$cause_category <- NA

# Assign categories based on keywords
surveyprojectdata$cause_category[grepl("distracted driving|distract|Younger drivers|phone|text|Unsafe driving|not paying attention|", tolower(surveyprojectdata$leading_cause))] <- "Distracted Driving"
surveyprojectdata$cause_category[grepl("speed", tolower(surveyprojectdata$leading_cause))] <- "Speeding"
surveyprojectdata$cause_category[grepl("drink|drunk|dui|Drunk ", tolower(surveyprojectdata$leading_cause))] <- "Impaired Driving"
surveyprojectdata$cause_category[grepl("reckless|careless|impatient", tolower(surveyprojectdata$leading_cause))] <- "Reckless Driving"
surveyprojectdata$cause_category[grepl("new driver|inexperience", tolower(surveyprojectdata$leading_cause))] <- "Inexperienced Driving"
surveyprojectdata$cause_category[grepl("weather", tolower(surveyprojectdata$leading_cause))] <- "Weather Conditions"
surveyprojectdata$cause_category[grepl("red light|right of way", tolower(surveyprojectdata$leading_cause))] <- "Traffic Violations"
surveyprojectdata$cause_category[grepl("N/A", tolower(surveyprojectdata$leading_cause))] <- "No response"
surveyprojectdata$cause_category[grepl("roads", tolower(surveyprojectdata$leading_cause))] <-"Road Conditions"






# Count frequency of each category
category_counts <- table(surveyprojectdata$cause_category)
category_df <- as.data.frame(category_counts)
colnames(category_df) <- c("Cause", "Frequency")


# Bar chart of categorized causes


ggplot(category_df, aes(x = reorder(Cause, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Categorized Leading Causes of Road Accidents in Bloomington-Normal",
       x = "Cause",
       y = "Frequency") +
  theme_minimal(base_size = 13)


# First calculate percentages
freq <- category_df$Frequency
labels <- category_df$Cause
percentages <- round(100 * freq / sum(freq), 1)
labels_with_percent <- paste(labels, "-", percentages, "%")

# Now plot the pie chart
pie(freq,
    labels = labels_with_percent,
    main = "Categorized Leading Causes of Road Accidents in Bloomington-Normal",
    col = rainbow(length(labels)),
    cex = 0.5) # Adjust label size if needed



# Bar chart of categorized causes


ggplot(category_df, aes(x = reorder(Cause, Frequency),
                              y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Categorized Leading Causes of Road Accidents in Bloomington-Normal",
       x = "Cause",
       y = "Frequency") +
  theme_minimal(base_size = 10)










colnames(surveyprojectdata)

survey_clean <- surveyprojectdata[-4, ]

View(survey_clean)
# Save  data
#write.csv(surveyprojectdata,"surveydata.csv", row.names = FALSE)
#saveRDS(surveyprojectdata, file = "surveydata.rds")
# Save  data
# Load it back later
#surveyprojectdata <- read.csv("surveyprojectdata.csv")
#surveyprojectdata <- readRDS("surveyprojectdata.rds")





# Combine "51 +" and "51+" into a single category "51+"
survey_clean$age_group <- ifelse(survey_clean$age_group %in% c("51 +", "51+"), "51+", survey_clean$age_group)


# Combine "16-25" and "18-25" into a single "16-25" group
survey_clean <- survey_clean %>%
  mutate(age_group = ifelse(age_group %in% c("16-25", "18-25"), "16-25", age_group))

# Make age_group a factor with a logical order
survey_clean <- survey_clean %>%
  mutate(age_group = factor(age_group, levels = c("16-25", "26-35", "36-50", "51+")))

# Check the updated age_group
table(survey_clean$age_group)

View(survey_clean)

############
##################
################
##################
##################
#####################
###################
# Save  data
#write.csv(survey_clean,"survey_clean.csv", row.names = FALSE)
#saveRDS(survey_clean, file = "survey_clean.rds")
# Save  data
# Load it back later
#survey_clean <- read.csv("survey_clean.csv")
#survey_clean <- readRDS("survey_clean.rds")

#View(survey_clean)

# --- 1. Frequency of raw causes (top 10 for readability)
raw_df <- survey_clean %>%
  count(leading_cause, name = "Frequency") %>%
  arrange(desc(Frequency)) %>%
  top_n(10, Frequency) %>%
  mutate(type = "Raw Responses")

# --- 2. Frequency of categorized causes
cat_df <- survey_clean %>%
  count(cause_category, name = "Frequency") %>%
  rename(leading_cause = cause_category) %>%
  mutate(type = "Categorized")

# --- 3. Combine both
combined_df <- bind_rows(raw_df, cat_df)

# --- 4. Plot
ggplot(combined_df, aes(x = reorder(leading_cause, Frequency), y = Frequency, fill = type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~type, scales = "free_y") +
  labs(
    title = "Raw vs Categorized Leading Causes of Road Accidents in Bloomington-Normal",
    x = "",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("Raw Responses" = "darkorange", "Categorized" = "darkgreen")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")


######EDA
# Basic structure

library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)

str(survey_clean)

# Summary of all variables
summary(survey_clean)

# Check for missing values
colSums(is.na(survey_clean))


generate_frequency_tables <- function(data, exclude_cols = c("timestamp", "leading_cause", "suggestions")) {
  cat("Frequency Tables for Categorical Variables:\n\n")
  
  # Loop through columns
  for (col in names(data)) {
    # Check if the column is categorical and not excluded
    if ((is.factor(data[[col]]) || is.character(data[[col]])) && !(col %in% exclude_cols)) {
      cat("Variable:", col, "\n")
      print(table(data[[col]], useNA = "ifany"))
      cat("\n---------------------------\n")
    }
  }
}




# Call the function on your survey dataset
# Call the function on your survey dataset
generate_frequency_tables(survey_clean)
table(survey_clean$age_group)



table(survey_clean$driver_category, survey_clean$cause_category)

# Function to create cross-tabulation between two categorical variables
cross_tabulate <- function(data, var1, var2) {
  if (!all(c(var1, var2) %in% names(data))) {
    stop("One or both variables are not in the dataset.")
  }
  
  tab <- table(data[[var1]], data[[var2]])
  cat("\nCross-tabulation between", var1, "and", var2, ":\n")
  print(tab)
  
  return(tab)
}

cross_tabulate(survey_clean, "driver_category", "cause_category")
prop.table(cross_tabulate(survey_clean, "driver_category", "cause_category"), margin = 1)  # Row-wise percentages






ggplot(survey_clean, aes(x = driver_category)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Driver Categories", x = "Driver Category", y = "Count") +
  theme_minimal()



category_freq <- survey_clean %>%
  filter(!is.na(cause_category)) %>%
  count(cause_category)



########cause_category pie chart
ggplot(category_freq, aes(x = "", y = n, fill = cause_category)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Categorized Causes") +
  theme_void() +
  scale_fill_brewer(palette = "Set3")

##############driving_experience bar plot

ggplot(survey_clean, aes(x = driving_experience)) +
  geom_histogram(binwidth = 5, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of Driving Experience", x = "Years of Experience", y = "Count") +
  theme_minimal()




library(plotly)

plot_interactive_bars <- function(data, exclude_cols = c("timestamp", "leading_cause", "suggestions")) {
  cat("Interactive Bar Plots for Categorical Variables:\n\n")
  
  for (col in names(data)) {
    if ((is.factor(data[[col]]) || is.character(data[[col]])) && !(col %in% exclude_cols)) {
      p <- ggplot(data, aes_string(x = col)) +
        geom_bar(fill = "darkgreen") +
        labs(title = paste("Interactive Bar Plot of", col), x = col, y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      print(ggplotly(p))
    }
  }
}


plot_interactive_bars(survey_clean)

str(survey_clean)

#####################################################
#############################
#########################simple category
####################
###############################
####################################################

# Create driver_fault variable
survey_clean <- survey_clean %>%
  mutate(driver_fault = ifelse(cause_category %in% c(
    "Distracted Driving", 
    "Impaired Driving", 
    "Inexperienced Driving", 
    "Reckless Driving", 
    "Speeding", 
    "Traffic Violations"
  ), 1, 0))

# Check the new variable
table(survey_clean$driver_fault)
######################################################
# Create a new column for categorized causes


# Bar chart of categorized causes


ggplot(category_df, aes(x = reorder(Cause, Frequency),
                              y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Categorized Leading Causes of Road Accidents in Bloomington-Normal",
       x = "Cause",
       y = "Frequency") +
  theme_minimal(base_size = 10)








# Remove NAs
# Check column names
colnames(survey_clean)







######EDA
# Basic structure

library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)

str(survey_clean)

# Summary of all variables
summary(survey_clean)

# Check for missing values
colSums(is.na(surveyprojectdata))



plot_categorical_bars <- function(data, exclude_cols = c("timestamp", "leading_cause", "suggestions")) {
  cat("Bar Plots for Categorical Variables:\n\n")
  
  for (col in names(data)) {
    if ((is.factor(data[[col]]) || is.character(data[[col]])) && !(col %in% exclude_cols)) {
      plot <- ggplot(data, aes_string(x = col)) +
        geom_bar(fill = "steelblue") +
        labs(title = paste("Bar Plot of", col), x = col, y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      print(plot)
    }
  }
}

plot_categorical_bars(survey_clean)





#####################################################
#############################
#########################survey design estimation
####################
###############################
####################################################
#########survey design estimation
#####################################################
#############################
#########################survey design estimation
####################
###############################
####################################################
#########survey design estimation
library(survey)
library(sampling)
library(SDAResources)

table(survey_clean$driver_category)

# Define stratified survey design (without fpc)
# Define stratified survey design (without fpc)
# Define stratified survey design (without fpc)
# Define stratified survey design (without fpc)
# Define stratified survey design (without fpc)


strat_design <- svydesign(
  ids = ~1, 
  strata = ~driver_category, 
  data = survey_clean
)



# Estimate proportion of "Distracted Driving"
svymean(~I(cause_category == "Distracted Driving"), design = strat_design, na.rm = TRUE)
svymean(~cause_category, design = strat_design, na.rm = TRUE)
svymean(~driver_fault, design = strat_design, na.rm = TRUE)
##########comparing


swow <- svymean(~driver_fault + seatbelt_usage +
                  seatbelt_usage +
                  phone_use_last_6mo +
                  speeding_habit +
                  drowsy_driving, design = strat_design)

swow
confint(swow, level=.95, 
        df=degf(strat_design)) #note that df = n-H = 44-3




svywow<-svymean(~factor(cause_category),
                design = strat_design, na.rm = TRUE)
svywow
confint(svywow, level=.95, 
        df=degf(strat_design)) #note that df = n-H = 44-3




########################

srs_design <- svydesign(id = ~1, data = survey_clean)

svymean(~factor(cause_category), design = srs_design, na.rm = TRUE)




# Extract SEs under both designs
se_strat <- SE(svymean(~factor(cause_category), design = strat_design, na.rm = TRUE))
se_srs   <- SE(svymean(~factor(cause_category), design = srs_design, na.rm = TRUE))

# Compare SEs
comparison <- data.frame(
  Cause = names(se_strat),
  SE_SRS = as.vector(se_srs),
  SE_Stratified = as.vector(se_strat)
)

print(comparison)




###################
###################
######################
#########################
######################
# Define stratified survey design (WITH fpc)
# Define stratified survey design (WITH fpc)
# Define stratified survey design (WITH fpc)
# Define stratified survey design (WITH fpc)
# Define stratified survey design (WITH fpc)
####################
##################
#################
# Quick check
table(survey_clean$driver_fault)

# Estimate the proportion of driver's fault
mean(survey_clean$driver_fault)


# 95% confidence interval for proportion
prop.test(sum(survey_clean$driver_fault), nrow(survey_clean))

str(survey_clean)



# Convert all character columns to factors
survey_clean[sapply(survey_clean, is.character)] <- 
  lapply(survey_clean[sapply(survey_clean, is.character)], as.factor)



summary(survey_clean)



###################
###################
######################
#########################
#########survey design estimation
library(survey)
library(sampling)
library(SDAResources)

View(survey_clean)






driver_categoryname <- c("College Faculty/Staff","College Student","Commercial Driver")
View(survey_clean)


# Set total population and sample size
# with total size n=44
N=3000
sampsize <- c(1400,1500,100)

table(survey_clean$driver_category)

######################################use stratum approach##########
# Sort the data by stratum
newsurvey_data<-survey_clean[order(survey_clean$driver_category), ]
View(newsurvey_data)

table(newsurvey_data$driver_category)




###############analysis#####
###############analysis#####
boxplot(driving_experience ~ driver_category,
        xlab = "driver category", 
        ylab = "driving_experience", data = newsurvey_data)




# create a variable containing population stratum sizes, for use in fpc 
popsize_recode <- c('College Faculty/Staff' = 1400, 
                    'College Student' = 1500, 
                    'Commercial Driver' = 100)



###################

newsurvey_data$popsize <- popsize_recode[newsurvey_data$driver_category]
table(newsurvey_data$popsize) #check the new variable
View(newsurvey_data)




# Step 1: Calculate sample sizes by group
sample_sizes <- table(newsurvey_data$driver_category)

# Step 2: Create the probability column
newsurvey_data$probability <- sample_sizes[newsurvey_data$driver_category] / newsurvey_data$popsize

# Step 3: Create the weight column (inverse of probability)
newsurvey_data$weight <- 1 / newsurvey_data$probability

# Step 4: (Optional) Check your new columns
head(newsurvey_data[, c(
"driver_category","driver_fault", "popsize", "probability", "weight")], n=10)

tail(newsurvey_data[, c(
  "driver_category","driver_fault", "popsize", "probability", "weight")], n=20)

# Check that the sampling weights sum to the population sizes for each stratum
tapply(newsurvey_data$weight,newsurvey_data$driver_category,sum)

# input design information for agstrat
dstr <- svydesign(id = ~1, strata = ~driver_category,
                  weights = ~weight, fpc = ~popsize,
                  data = newsurvey_data)

dstr

swfpc <- svymean(~driver_fault + seatbelt_usage +
                  seatbelt_usage +
                  phone_use_last_6mo +
                  speeding_habit +
                  drowsy_driving, design = dstr)

swfpc

confint(swfpc, level=.95, df=degf(dstr)) 


################

# calculate mean, SE and confidence interval druver fault
smean<-svymean(~driver_fault, dstr)
smean

confint(smean, level=.95, df=degf(dstr)) # note that df = n-H = 44-3

# calculate total, SE and CI
stotal<-svytotal(~driver_fault, dstr)
stotal

# calculate confidence intervals using the degrees of freedom
confint(stotal, level=.95,df= degf(dstr))


# calculate mean and se of acres92 by regions
svyby(~driver_fault, by=~driver_category, dstr, svymean, keep.var = TRUE)

# calculate total and se of acres92 by regions
svyby(~driver_fault, ~driver_category, dstr, svytotal, keep.var = TRUE)


svymean(~driver_fault + seatbelt_usage +
          seatbelt_usage +
          phone_use_last_6mo +
          speeding_habit +
          drowsy_driving, design = dstr)


# Overall mean driving experience
svymean(~driving_experience, design = dstr)


# Mean driving experience by driver_category
svyby(~driving_experience, ~driver_category, design = dstr, svymean)


library(ggplot2)

# Create a dataframe for plotting
means_by_group <- svyby(~driver_fault, ~driver_category, dstr, svymean)

# Basic barplot
ggplot(means_by_group, aes(x = driver_category, y = driver_fault)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = driver_fault - se, ymax = driver_fault + se), width = 0.2) +
  labs(title = "Estimated Mean Driver Fault by Driver Category",
       x = "Driver Category", y = "Proportion at Fault") +
  theme_minimal()



confint(svymean(~driver_fault + seatbelt_usage +
                  phone_use_last_6mo +
                  speeding_habit +
                  drowsy_driving, design = dstr), 
        level = 0.95, df = degf(dstr))

###########################################################
############################################################
#####################further results

library(dplyr)

# Summarize probability and weight by driver_category
summary_table <- newsurvey_data %>%
  group_by(driver_category) %>%
  summarise(
    avg_probability = mean(probability),
    avg_weight = mean(weight),
    n = n()
  )

# View the clean table
print(summary_table)




# Get svymean object
prop_means <- svymean(~driver_fault + seatbelt_usage +
                        phone_use_last_6mo + 
                        speeding_habit + 
                        drowsy_driving, design = dstr)

# Confidence Intervals (95%)
prop_confint <- confint(prop_means, level = 0.95, df = degf(dstr))

# View the CI table
print(prop_confint)


library(ggplot2)

# Convert svymean output to a data frame
prop_df <- as.data.frame(prop_means)
prop_df$variable <- rownames(prop_df)

# Add the confidence intervals
conf_df <- as.data.frame(prop_confint)
colnames(conf_df) <- c("lower", "upper")
prop_df <- cbind(prop_df, conf_df)

# Plot
ggplot(prop_df, aes(x = reorder(variable, mean), y = mean)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkred") +
  coord_flip() +
  labs(
    title = "Estimated Proportions with 95% Confidence Intervals",
    x = "Variables",
    y = "Proportion"
  ) +
  theme_minimal()

####################################
#
###########################################################
############################################################
##########comparing STR AND SRS
####################################################
###################################################################
##################################################





## Stratified Design
dstrc <- svydesign(id = ~1, strata = ~driver_category,
                   weights = ~weight, fpc = ~popsize,
                   data = newsurvey_data)

# Estimate the mean of driver_fault using the stratified design
mean_stratified <- svymean(~driver_fault, dstrc)
mean_stratified


# Simple Random Sampling (SRS) Design


# Step 1: Calculate total population size (N)
total_population_size <- 3000  # Sum of all population sizes

# Step 2: Define sample size (n)
sample_size_srs <- nrow(newsurvey_data)  # This should be the size of your sample

# Step 3: Calculate the SRS weight (inverse of selection probability)
newsurvey_data$weight_srs <- total_population_size / sample_size_srs

# Check the new weights
head(newsurvey_data[, c("driver_category", "weight_srs")])

# Create SRS design object with the new weights
srs_design <- svydesign(id = ~1, data = newsurvey_data, 
                        weights = ~weight_srs, fpc = rep(3000,44))

# Estimate the mean of driver_fault using SRS with the new weights
mean_srs <- svymean(~driver_fault, srs_design)
mean_srs

# Calculate confidence intervals for the mean of driver_fault
conf_srs <- confint(mean_srs, level = 0.95)
conf_srs


# calculate total, SE and CI
srstotal<-svytotal(~driver_fault, srs_design)
srstotal

# calculate confidence intervals using the degrees of freedom
confint(srstotal, level=.95,df= degf(srs_design))

# Standard Error and Confidence Interval for Stratified Design
se_stratified <- svymean(~driver_fault, dstr)
conf_stratified <- confint(se_stratified, level = 0.95)
se_stratified
conf_stratified

# Standard Error and Confidence Interval for SRS Design
se_srs <- svymean(~driver_fault, srs_design)
conf_srs <- confint(se_srs, level = 0.95)
se_srs
conf_srs



# Plotting comparison of the means with confidence intervals
ggplot() +
  geom_bar(aes(x = "Stratified", y = mean_stratified), stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(x = "Stratified", ymin = conf_stratified[1], ymax = conf_stratified[2]), width = 0.2) +
  geom_bar(aes(x = "SRS", y = mean_srs), stat = "identity", fill = "orange") +
  geom_errorbar(aes(x = "SRS", ymin = conf_srs[1], ymax = conf_srs[2]), width = 0.2) +
  labs(title = "Comparison of Stratified vs SRS Estimates of Driver Fault",
       x = "Sampling Design", y = "Proportion of Driver Fault") +
  theme_minimal()
