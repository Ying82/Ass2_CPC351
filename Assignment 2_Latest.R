## Load necessary library
library(ggplot2)
library(reshape2)
library(knitr)

#------------Q1----------
# working directory
setwd("C:\\Users\\A\\Downloads\\Ass2_351")
# load dataset
dataset <- read.csv("Food_Nutrition_Dataset.csv")
# display structure
str(dataset)
# display summary
dataset$food_name <- as.factor(dataset$food_name)
dataset$category  <- as.factor(dataset$category)
summary(dataset)

#-----------Q2------------
missing_val <- colSums(is.na(dataset))
print(missing_val)
bar_mid <- barplot(missing_val,
                   main = "Missing Values per Column",
                   ylab = "Number of Missing Values",
                   col = "pink",
                   xaxt = "n")  

# Add angled labels
text(x = bar_mid, 
     y = par("usr")[3] - 0.5, 
     labels = names(missing_val), 
     srt = 45,   # degrees angle
     adj = 1,    # right align
     xpd = TRUE) # draw at outside plot area


#-------------Q3--------------
#display number unique food categories
num_unique_categories <- length(unique(dataset$category))
cat("Number of unique food categories:", num_unique_categories, "\n")

# number of food each category
category_counts <- table(dataset$category)

# convert to data frame
category_df <- as.data.frame(category_counts)
colnames(category_df) <- c("Category", "Count")

ggplot(category_df, aes(x = Count, y = reorder(Category, Count))) +
  geom_bar(stat = "identity", fill = "blue", width = 0.6) +
  labs(title = "Number of Foods per Category",
       x = "Number of Foods",
       y = "Food Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) 

#----------------Q4------------------
# sort 
sorted_foods <- dataset[order(-dataset$calories), ]

# top 10 rows
top_10_caloric_foods <- sorted_foods[1:10, c("food_name", "category", "calories")]

# print top 10 result
print(top_10_caloric_foods)

# nicer table
kable(top_10_caloric_foods, row.names = FALSE, caption = "Top 10 Foods by Calories")

# -----------------Q5-----------------------
# group by category and calculate averages 
q5_summary <- aggregate(
  cbind(calories, protein, carbs, fat) ~ category,
  data = dataset,
  FUN = function(x) mean(x, na.rm = TRUE)
)

# rename columns for clarity
names(q5_summary) <- c("category", "Avg_Calories", "Avg_Protein", "Avg_Carbs", "Avg_Fat")

# round all numeric columns to 2 decimal places
q5_summary[, 2:5] <- round(q5_summary[, 2:5], 2)

# print result
print(q5_summary)

#-------------Q6-------------
hist(dataset$calories,
     breaks = 20,               # number of bins
     col = "yellow",         # bar color
     border = "black",          # bar border color
     main = "Calories Distribution of Foods",
     xlab = "Calories",
     ylab = "Count")

#--------------Q7-----------------
# start the plot x-axis=calories y-axis=category
ggplot(food_data, aes(x = calories, y = reorder(category, calories, FUN = median))) +  # create boxplots, set box color, transparency
  geom_boxplot(fill = "magenta", alpha = 0.7) +
  labs(
    title = "Boxplot of Calories by Category",
    x = "Food Category",
    y = "Calories"
  ) +
  # Apply a theme
  theme_minimal()

#----------Q8---------
ggplot(dataset, aes(x = protein, y = calories, color = category)) +
  geom_point(size = 2, alpha = 0.8) +                 # adjust point size and transparency
  labs(
    title = "Calories vs Protein by Category",
    x = "Protein",
    y = "Calories"
  ) + 
  theme_minimal() + #clearer view
  guides(color = guide_legend(ncol = 2))       # split legend into 2 columns to save space

#-------------Q9---------- 
# select only the numeric variables needed for correlation analysis
num_data_q9 <- dataset[, c("calories", "protein", "carbs", "fat", "iron", "vitamin_c")]  
# compute the correlation matrix while ignoring rows with missing values
corr_matrix <- cor(num_data_q9, use = "complete.obs")
# convert the correlation matrix into long format for ggplot heatmap
corr_melt <- melt(corr_matrix)

# Plot heatmap
ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    name = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap of Nutritional Variables",
       x = "",
       y = "")

# ------------------Q10------------------
# calculate average Vitamin C per category
vit_summary <- aggregate(vitamin_c ~ category,
                         data = food_data,
                         FUN = function(x) mean(x, na.rm = TRUE))

# sort from highest to lowest
vit_summary <- vit_summary[order(-vit_summary$vitamin_c), ]

# top 10
top_10_vit_c <- vit_summary[1:10, ]

# plot bar chart and ranked from highest to lowest
ggplot(top_10_vit_c, aes(x = reorder(category, vitamin_c), y = vitamin_c)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(
    title = "Top 10 Food Categories by Average Vitamin C",
    x = "Category",
    y = "Average Vitamin C (mg)"
  ) +
  # Apply theme
  theme_minimal()

#--------------Q11-------------------
# Compute 95th percentile ignoring missing value
fat_95 <- quantile(dataset$fat, 0.95, na.rm = TRUE)
print(fat_95)

# Subset top 5% highest fat
high_fat <- subset(dataset, fat >= fat_95)
# Rank by fat decreasingly
high_fat_rank <- high_fat[order(-high_fat$fat),]
high_fat_rank[, c("food_name", "category", "fat")]

# Frequency table count by category
highfat_category_counts <- table(high_fat$category)
high_fat_counts <- as.data.frame(highfat_category_counts)
colnames(high_fat_counts) <- c("category", "count")

# Sort descending by count
high_fat_counts <- high_fat_counts[order(-high_fat_counts$count), ]
# change the internal factor ordering for better visualization
high_fat_counts$category <- factor(
  high_fat_counts$category,
  levels = high_fat_counts$category  # current sorted order
)
high_fat_counts

# Add label column (count + percentage)
high_fat_counts$label <- paste0(
  high_fat_counts$count, "\n(", 
  round(high_fat_counts$count / sum(high_fat_counts$count) * 100, 1), "%)"
)

# Visualize highest fat based on the category
# Pie chart
ggplot(high_fat_counts, aes(x = "", y = count, fill = category)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3) +
  theme_void() +
  labs(title = "High-Fat Foods by Category (95th Percentile)") +
  theme(legend.position = "right")

#-----------------Q12----------------------------
# select top 3 categories
selected_categories <- c(
  "Fruits and Fruit Juices",
  "Vegetables and Vegetable Products",
  "Cakes and pies"
)
top3_categories <- subset(dataset, category %in% selected_categories)

# Violin plot for top3 categories using ggplot
ggplot(top3_categories, aes(x = category, y = carbs, fill = category)) +
  # Violin plot: shows the distribution shape (colored area), trimmed to data range
  geom_violin(trim = TRUE) +            
  # Boxplot: shows median, quartiles, and outliers within the violin
  geom_boxplot(width = 0.15, fill = "white") +
  labs(
    title = "Distribution of Carbohydrates Across Selected Food Categories",
    x = "Food Category",
    y = "Carbohydrates (g)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# --------------Q13--------------------
# Sums up all calories within each category
  category_totals <- aggregate(calories ~ category,
                               data = food_data,
                               FUN = function(x) sum(x, na.rm = TRUE))
  
# plot stacked bar chart
  ggplot(category_totals, aes(x = "", y = calories, fill = category)) +
    geom_col(width = 0.5) +
    labs(
      title = "Total Calorie Contribution by Category",
      x = "All Categories",
      y = "Total Calories"
    ) +
    theme_minimal() +
    # removes the x-axis text because there is only one "stack".
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

#----------------Q14---------------
# calculate density
protein_density <- density(dataset$protein, na.rm = TRUE)  # ignores missing values

plot(protein_density, 
     main = "Density of Protein Content Across Foods",
     xlab = "Protein (g)",
     ylab = "Density",
     col = "blue",
     lwd = 2)

#--------------Q15-----------------
ggplot(dataset, aes(x = carbs, y = calories, size = iron)) +
  geom_point(alpha = 0.5, color = "blue") +
  scale_size_continuous(range = c(1, 15)) +   # adjust min and max bubble size
  labs(title = "Calories vs Carbs with Iron Content",
       x = "Carbs (g)",
       y = "Calories",
       size = "Iron (mg)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # center the title

#----------------Q16a-------------
# Create a new, clean dataset by keeping only rows where the calorie value is NOT zero.
# This step prevents division-by-zero errors
data_q16_clean <- dataset[dataset$calories != 0, ]

#Calculate the ratio 
data_q16_clean$protein_calorie_ratio <- 
  data_q16_clean$protein / data_q16_clean$calories

#Display the calculated ratio for first few rows
print("List of Protein-to-Calorie Ratios for food items (First 6 Rows):")
print(head(data_q16_clean[, c("food_name", "protein_calorie_ratio")]))

#---------------Q16b-------------
#sort the clean dataset by ratio in descending order.
top_10_ratio_foods <- data_q16_clean[
  order(data_q16_clean$protein_calorie_ratio, decreasing = TRUE), 
]

# Select the top 10 rows from sorted data
top_10_ratio_foods <- head(top_10_ratio_foods, 10)

#Display top 10 rows
print("Top 10 Foods with Highest Protein-to-Calorie Ratio:")
print(top_10_ratio_foods[, c("food_name", "protein_calorie_ratio")])

#---------------Q16c-------------
ggplot(top_10_ratio_foods, 
       aes(x = protein_calorie_ratio, 
           #Ensures the food name automatically sorted from smallest to largest ratio
           y = reorder(food_name, protein_calorie_ratio))) + 
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Top 10 Foods by Protein-to-Calorie Ratio",
       x = "Protein-to-Calorie Ratio (g/Calorie)",
       y = "Food Item") +
  #Apply simple background theme
  theme_minimal() +
  #Title placement
  theme(plot.title = element_text(hjust = 0.5))

#---------------Q17a-------------
# Standard Calorie Conversion Factors: Protein: 4 kcal/g, Carbs: 4 kcal/g, Fat: 9 kcal/g
dataset$protein_cal <- dataset$protein * 4
dataset$carbs_cal <- dataset$carbs * 4
dataset$fat_cal <- dataset$fat * 9
dataset$macronutrient_total_cal <- dataset$protein_cal + dataset$carbs_cal + dataset$fat_cal

# Calculate the mean calorie contribution for all macro-nutrients PER CATEGORY.
macronutrient_summary <- aggregate(
  cbind(protein_cal, carbs_cal, fat_cal, macronutrient_total_cal) ~ category,
  data = dataset,
  FUN = function(x) mean(x, na.rm = TRUE) 
)

# Calculate percentage contribution 
macronutrient_summary$protein_pct <- ifelse(macronutrient_summary$macronutrient_total_cal == 0, 0, 
                            macronutrient_summary$protein_cal / macronutrient_summary$macronutrient_total_cal * 100)
macronutrient_summary$carbs_pct <- ifelse(macronutrient_summary$macronutrient_total_cal == 0, 0,
                            macronutrient_summary$carbs_cal / macronutrient_summary$macronutrient_total_cal * 100)
macronutrient_summary$fat_pct <- ifelse(macronutrient_summary$macronutrient_total_cal == 0, 0,
                            macronutrient_summary$fat_cal / macronutrient_summary$macronutrient_total_cal * 100)

#Display the percentage contribution of protein,carbs and fat to the total calories
print("Percentage Contribution by Category:")
print(macronutrient_summary[, c("category", "protein_pct", "carbs_pct", "fat_pct")])

#---------------Q17b-----------------
# Reshape data for plotting
macronutrient_long <- melt(macronutrient_summary,
                           id.vars = "category",
                           measure.vars = c("protein_pct", "carbs_pct", "fat_pct"),
                           variable.name = "Macronutrient",
                           value.name = "Percentage")

# Clean up and reorder Macronutrient names for the legend
macronutrient_long$Macronutrient <- factor(macronutrient_long$Macronutrient, 
                                           levels = c("fat_pct", "carbs_pct", "protein_pct"),
                                           labels = c("Fat (9 kcal/g)", "Carbs (4 kcal/g)", "Protein (4 kcal/g)"))

ggplot(macronutrient_long, 
       aes(x = category, y = Percentage, fill = Macronutrient)) +
  # position="stack" ensures the segments are stacked on top of each other
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Average Macronutrient Calorie Percentage Contribution by Food Category",
       x = "Food Category",
       y = "Percentage of Total Macronutrient Calories (%)",
       fill = "Macronutrient") +
  scale_fill_manual(values = c("Protein (4 kcal/g)" = "skyblue",  
                               "Carbs (4 kcal/g)" = "pink",     
                               "Fat (9 kcal/g)" = "purple")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5))



