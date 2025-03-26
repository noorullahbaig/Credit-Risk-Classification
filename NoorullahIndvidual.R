#Data Import
library(dplyr)
library(ggplot2)
library(tidyverse)


data = read.csv("/Users/noorullah/Downloads/5. credit_risk_classification.csv")

#Checking for null values
sum(is.na(data))

#Checking for duplicates
nrow(data[duplicated(data),])

#Converting class to factor
data$class = as.factor(data$class)

#Converting personal status to factor
data$personal_status = as.factor(data$personal_status)
summary(data$personal_status)

#Objective 1: To investigate the impact of personal_status on credit risk classification. 

#Pie chart showing distribution of personal status
status_counts <- data %>%
  group_by(personal_status) %>%
  summarise(Frequency = n(), .groups = "drop") %>%  # Summarize to get counts for each status
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 1))  # Add percentage column
status_counts <- status_counts %>%
  mutate(Label = paste(Percentage, "%"))
ggplot(status_counts, aes(x = "", y = Frequency, fill = personal_status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Personal Status",
       x = NULL, y = NULL, fill = "Personal Status") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4) +
  theme_void() +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))


#Preparing data for visualization
summary_data <- data %>%
  group_by(personal_status, class) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  mutate(Proportion = Frequency / sum(Frequency), 
         Percentage = round(Frequency / sum(Frequency) * 100, 1),
         Label = paste0(Percentage, "%"))

#Frequency bar chart for personal status
ggplot(summary_data, aes(x = personal_status, y = Frequency, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency of Good and Bad Credit Ratings by Personal Status",
       x = "Personal Status",
       y = "Frequency",
       fill = "Credit Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Proportion Bar Chart of personal status including credit class
ggplot(summary_data, aes(x = personal_status, y = Proportion, fill = class)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(Proportion)),
            position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Credit Ratings by Personal Status",
       x = "Personal Status",
       y = "Proportion",
       fill = "Credit Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Creating a new column called "Gender"
data$gender <- factor(ifelse(grepl("^female", credit_risk$personal_status), "female", "male"))
summary(data$gender)

# Pie chart to show gender distribution
gender_counts <- table(data$gender)
percentages <- round(prop.table(gender_counts) * 100, 1)
library(plotrix)
pie3D(gender_counts, 
      labels = paste(percentages, "%", "(", as.numeric(gender_counts), ")"),
      explode = 0.2,             
      col = c("pink", "skyblue"),
      radius = 1.0,              
      labelcex = 1.0,
      theta = 0.6)   
title(main = "Distribution of Gender", cex.main = 1.5)
legend("topright", 
       legend = names(gender_counts),
       fill = c("pink", "skyblue"),                  
       title = "Gender", 
       cex = 1.3)                      

#Preparing data for chart
count_gender <- data %>%
  count(gender, class) %>%
  group_by(gender) %>%
  mutate(Proportion = n / sum(n))

#Proportion Bar Chart with gender including credit class
ggplot(count_gender, aes(x = gender, y = Proportion, fill = class)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(Proportion)),
            position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Credit Ratings by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "Credit Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Chi Square Test
gendertable = table(data$gender,data$class)
chisq.test(gendertable)

# Perform Two-Proportion Z-Test
test_result <- prop.test(
  x = gender_stats$bad_count,
  n = gender_stats$total_count
)

#Confidence Interval Lollipop Chart
gender_stats <- data %>%
  group_by(gender) %>%
  summarise(
    bad_count = sum(class == "bad"),
    total_count = n(),
    proportion = bad_count / total_count,
    ci = list(prop.test(bad_count, total_count)$conf.int) # Store CI as a list
  ) %>%
  mutate(
    ci_lower = map_dbl(ci, ~ .[1]), # Extract lower CI
    ci_upper = map_dbl(ci, ~ .[2])  # Extract upper CI
  )
ggplot(gender_stats, aes(x = gender, y = proportion)) +
  geom_point(size = 3, color = "red") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "blue") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Bad Credit Ratings by Gender with 95% Confidence Intervals",
    subtitle = paste("p-value =", signif(test_result$p.value, digits = 3)),
    x = "Gender",
    y = "Proportion with Bad Credit Rating"
  ) +
  theme_minimal()

#Cramer's V test for gender
library(vcd)
assocstats(gendertable)


#Convert class into binary
data <- data %>%
  mutate(class_bin = ifelse(class == "bad", 0, 1))

#Logistic Regression for gender
logistic_model <- glm(class_bin ~ gender, data = data, family = "binomial")
summary(logistic_model)

# Add predicted probabilities to the dataset
data$predicted_prob <- predict(logistic_model, type = "response")
prob_summary <- data %>%
  group_by(gender) %>%
  summarise(mean_predicted_prob = mean(predicted_prob),
            .groups = "drop")

#Bar chart to visualize predicted probabilities
ggplot(prob_summary, aes(x = gender, y = mean_predicted_prob, fill = gender)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +
  labs(title = "Predicted Probability of Good Credit Class by Gender",
       x = "Gender",
       y = "Predicted Probability",
       fill = "Gender") +
  theme_minimal()





#Objective: To understand the relation between employment duration and class
#Cleaning employment column
data$employment = as.factor(data$employment)
summary(data$employment)

#Changing category names for more clarity
data$employment_categories = ifelse(data$employment == "<1", "Less than 1 year",
                                    ifelse(data$employment == ">=7", "7 or more years", 
                                           ifelse(data$employment == "1<=X<4", "1 to 3 years", 
                                                  ifelse(data$employment == "4<=X<7", "4 to 6 years", 
                                                         ifelse(data$employment == "unemployed", "Unemployed", "NULL")))))
data$employment_categories = factor(data$employment_categories, 
                                    levels = c("Unemployed", 
                                               "Less than 1 year", 
                                               "1 to 3 years", 
                                               "4 to 6 years", 
                                               "7 or more years"))
summary(data$employment_categories)


#Pie chart to show the distribution of employment duration
employment_summary = as.data.frame(table(data$employment_categories))
colnames(employment_summary) = c("employment categories","frequency")
employment_summary$proportions = (employment_summary$frequency / sum(employment_summary$frequency)) * 100
labels = paste0(employment_summary$employment_categories, " (", round(employment_summary$proportion, 1), "%)")
pie(
  employment_summary$frequency,
  labels = labels,
  col = c("#AEC6CF", "#77DD77", "#FFB7B2", "#C9A0DC", "#FFDAB9"),
  main = "Distribution of Employment Categories"
)
legend(
  "topright",
  legend = employment_summary$`employment categories`,
  fill = c("#AEC6CF", "#77DD77", "#FFB7B2", "#C9A0DC", "#FFDAB9"),
  title = "Employment Categories",
  cex = 0.4)

#Frequency bar for employment categories
ggplot(data, aes(x = employment_categories, fill = class)) +
  geom_bar() +
  labs(title = "Frequency of Credit Ratings by Employemnt Category",
       x = "Employment Category",
       y = "Frequency",
       fill = "Credit Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Filtering and excluding the unemployed values
employed_data = data %>% 
  filter(employment_categories != "Unemployed")
employed_data$employment_categories = droplevels(employed_data$employment_categories, 
                                                 employed_data$employment_categories == "Unemployed")


#Mosaic plot to visualize the spread of employed people and credit class
install.packages("ggmosaic")
library("ggmosaic")
ggplot(employed_data) +
  geom_mosaic(aes(x = product(employment_categories), fill = class), na.rm = TRUE) + 
  labs(
    title = "Mosaic Plot of Credit Class by Employment Categories",
    x = "Employment Categories",
    y = "Proportion",
    fill = "Credit Class"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Adjust x-axis labels (no rotation)
    axis.title.y = element_text(margin = margin(r = 10))  # Add margin to y-axis title
  )


#Chi square test between employment and class
employment_table = table(employed_data$class,employed_data$employment_categories)
chisq.test(employment_table)

#Cramers V for employment and class
assocstats(employment_table)

#Proportion chart for credit risk per category
ggplot(employed_data, aes(x = employment_categories, fill = class)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Credit Classes by Employment Categories",
       x = "Employment Categories", y = "Proportion", fill = "Credit Class") +
  theme_minimal()

#Logistic Regression for employment categories
logistic_model_employed = glm(class_bin ~ employment_categories, family = "binomial", data = employed_data)
summary(logisitic_model_employed)

#Calculating probability of being bad credit risk per category
odds = exp(coef(logisitic_model_employed))
Probability = odds / (1 + odds) * 100
bad_credit_data <- data.frame(
  Employment_Category = c("Less than 1 year", "1 to 3 years", "4 to 6 years", "More than 7 years"),
  Percentage_Bad = Probability
)

#Preparing data for bar chart
bad_credit_data$Employment_Category <- factor(
  bad_credit_data$Employment_Category,
  levels = c("Less than 1 year", "1 to 3 years", "4 to 6 years", "More than 7 years")
)

#bar chart to show chance of being bad credit risk per category
ggplot(bad_credit_data, aes(x = Employment_Category, y = Percentage_Bad, fill = Employment_Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Percentage of Bad Credit Risk by Employment Category",
    x = "Employment Categories",
    y = "Percentage of Bad Credit Risk (%)"
  ) +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


#Objective 3: To investigate the impact of property magnitude and number of dependents on credit risk. 

#Turning property magnitude column into factor
data$property_magnitude = as.factor(data$property_magnitude)
summary(data$property_magnitude)

#Preparing data for pie chart
property_count <- data %>%
  group_by(property_magnitude) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 1))
property_count <- property_count %>%
  mutate(Label = paste(Percentage, "%"))

#Pie chart to show distribution of property magnitude
ggplot(property_count, aes(x = "", y = Frequency, fill = property_magnitude)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Property Magnitude",
       x = NULL, y = NULL, fill = "Property Magnitude") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4) + 
  scale_fill_manual(values = c("red","royalblue","yellow","purple")) +
  theme_void() +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))


#frequency bar chart fro property magnitude
ggplot(data, aes(x = property_magnitude, fill = class)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Credit Classes by Property Magnitude",
       x = "Properrty Magnitude", y = "Count", fill = "Credit Class") +
  theme_minimal()

#Making number of dependents into a factor.
data$new_num_dependents = as.factor(data$new_num_dependents)
summary(data$new_num_dependents)

#Number of dependents pie chart
dependents_counts <- table(data$new_num_dependents)
percentages <- round(prop.table(dependents_counts) * 100, 1)
labels <- paste(percentages, "%", "(", as.numeric(dependents_counts), ")")
colors <- c("royalblue", "brown")
pie3D(dependents_counts, 
      labels = labels, 
      explode = 0,              
      col = colors,             
      radius = 0.8,             
      labelcex = 1.0,           
      theta = 1.2)              
title(main = "Distribution of Number of Dependents", cex.main = 1.5)
legend("topright", 
       legend = names(dependents_counts),  
       fill = colors,           
       title = "Number of Dependents", 
       cex = 0.5)              

#Frequency bar chart for number of dependents
ggplot(data, aes(x = new_num_dependents, fill = class)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Credit Classes by Number of Dependents",
       x = "Number of Dependents", y = "Count", fill = "Credit Class") +
  theme_minimal()

#grouped bar chart for property magnitude and number of dependents by credit class
ggplot(data, aes(x = property_magnitude, fill = class)) +
  geom_bar(position = "fill") +  # Fill makes it proportionate
  facet_wrap(~ new_num_dependents) +  # Separate panels by no_of_dependents
  labs(title = "Distribution of Credit Class by Property Magnitude and Dependents",
       x = "Property Magnitude",
       y = "Proportion",
       fill = "Credit Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

#Chi square test between number of dependents and class
table_dependents = table(data$new_num_dependents, data$class)
chisq.test(table_dependents)

#Chi square test between property magnitude and class
table_property = table(data$property_magnitude, data$class)
chisq.test(table_property)


#Cleaning data for cramer's v matrix
library(vcd)
library(gtools)
library(reshape2)
cramers_data <- data.frame(
  data$property_magnitude,
  data$new_num_dependents,
  data$class
)
cramers_v_matrix <- function(cramers_data) {
  vars <- colnames(cramers_data)
  n <- length(vars)
  matrix <- matrix(NA, ncol = n, nrow = n)
  colnames(matrix) <- rownames(matrix) <- vars
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        matrix[i, j] <- 1
      } else {
        tbl <- table(cramers_data[[vars[i]]], cramers_data[[vars[j]]])
        cramer_v <- assocstats(tbl)$cramer
        matrix[i, j] <- cramer_v
      }
    }
  }
  return(matrix)
}

cramer_matrix <- cramers_v_matrix(cramers_data)
cramer_df <- melt(cramer_matrix)
colnames(cramer_df) <- c("Variable1", "Variable2", "CramersV")

#Creating a heatmap to visualize cramer's v values
ggplot(cramer_df, aes(x = Variable1, y = Variable2, fill = CramersV)) +
  geom_tile(color = "white") +  # Add tile borders
  scale_fill_gradient2(low = "white", high = "blue", midpoint = 0.2, name = "Cramér's V") +
  geom_text(aes(label = round(CramersV, 2)), color = "black", size = 4) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Cramér's V Matrix Heatmap",
    x = "Variables",
    y = "Variables"
  )


#Logistic Regression using number of dependents and property magnitude
model <- glm(class_bin ~ new_num_dependents * property_magnitude,family = binomial, data = data)
summary(model)

#Cleaning model to plot odds
library(broom)
model_summary <- tidy(model, conf.int = TRUE, exponentiate = TRUE)

#odds plot for visualization
ggplot(model_summary, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  scale_y_log10() + 
  labs(
    title = "Odds Ratios for Model Coefficients",
    x = "Terms",
    y = "Odds Ratio (log scale)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

