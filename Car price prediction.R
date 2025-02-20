#Load necessary libraries
library(modeest)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(rpart.plot)
library(shiny)
library(randomForest)

#Load the dataset
car_ad<- read.csv("D:\\Master's Data Analytics\\MPS_Quarter 3\\ALY 6040\\car_ad.csv")
str(car_ad)


                         ####       Data Cleaning       ####
# Price
car_ad$price[car_ad$price == 0] <- NA
price_impute <- ave(car_ad$price, car_ad$model, car_ad$year, FUN = function(x) ifelse(x == 0, mean(x, na.rm = TRUE), x))
car_ad$price <- ifelse(is.na(car_ad$price), price_impute[match(interaction(car_ad$model, car_ad$year), interaction(names(price_impute)[1:2], names(price_impute)[3:4]))], car_ad$price)

# Mileage
car_ad$mileage[car_ad$mileage == 0] <- NA
mileage_impute <- ave(car_ad$mileage, car_ad$model, car_ad$year, FUN = function(x) ifelse(x == 0, mean(x, na.rm = TRUE), x))
car_ad$mileage <- ifelse(is.na(car_ad$mileage), mileage_impute[match(interaction(car_ad$model, car_ad$year), interaction(names(mileage_impute)[1:2], names(mileage_impute)[3:4]))], car_ad$mileage)

# Engine Type
car_ad$engType[car_ad$engType == ""] <- NA
engType_impute <- ave(car_ad$engType, car_ad$model, FUN = function(x) ifelse(x == "", names(which.max(table(x))), x))
car_ad$engType <- ifelse(is.na(car_ad$engType), engType_impute[match(car_ad$model, names(engType_impute))], car_ad$engType)


# Finding total missing values by columns
total_missing <- sapply(car_ad, function(x) sum(is.na(x) | x == ""))
print(total_missing)

# bar chart to visualize missing values in each column
column_names <- names(car_ad)
missing_pct <- total_missing / nrow(car_ad) * 100   # Calculate missing value percentages

# Transform data for ggplot2
missing_plot <- data.frame(
  column_name = names(total_missing),
  missing_values = total_missing,
  percentage = paste0(format(missing_pct, digits = 1), "%")
)
# Create the ggplot bar chart
ggplot(missing_plot, aes(x = column_name, y = total_missing)) +
  geom_bar(stat = "identity", width = 0.5, color = "skyblue") +
  geom_text(aes(label = percentage, x = column_name, y = total_missing + 1), hjust = 0.5, vjust = 0, size = 4) +
  labs(title = "Missing Values per Column", x = "Variables Name", y = "Number of Missing Values") +
  theme_minimal() +
  scale_y_discrete(expand = c(0, 0.2))



# Data Cleaning on Column Price
missing_values_by_body <- car_ad %>%
  group_by(car) %>%
  summarise_all(~ sum(is.na(.)))

car_values <- unique(car_ad$car)

for (car in car_values) {
  car_subset <- car_ad[car_ad$car == car, ]
  
  # Use a function that supports na.rm to find the mode
  mode_price <- mfv(car_subset$price, na_rm = TRUE)  # Using the mfv function
  
  car_ad$price[car_ad$car == car] <-
    ifelse(is.na(car_ad$price[car_ad$car == car]), mode_price, car_ad$price[car_ad$car == car])
}
sum(is.na(car_ad$price))
na_omit_car_ad <- na.omit(car_ad$price)


# Column Mileage
# Group by car type and calculate average mileage
average_mileage <- car_ad %>%
  group_by(car) %>%
  summarize(avg_mileage = mean(mileage, na.rm = TRUE))

# Fill missing values in mileage with corresponding average based on car type
car_ad$mileage[is.na(car_ad$mileage)] <- car_ad[is.na(car_ad$mileage), "car"] %>%
  map_dbl(function(car_type) {
    # Use dplyr::filter for clarity and consistency (if installed)
    avg <- average_mileage %>% filter(car == car_type) %>% pull(avg_mileage)
    if (is.na(avg)) {
      # Handle cases where there's no car type average (e.g., new car type)
      NA
    } else {
      avg
    }
  })

# Check for remaining NAs (optional)
sum(is.na(car_ad$mileage))


# Column Engine Volume
# replacing missing values of engV column by finding mean by grouping body type of the car
missing_values_by_body <- car_ad %>%
  group_by(body) %>%
  summarise_all(~ sum(is.na(.)))
print("Missing values for each column based on the 'body' column:")
print(missing_values_by_body)

body_values <- unique(car_ad$body)

# Loop through each body and replace missing values in engV column with the mean
for (body in body_values) {
  body_subset <- car_ad[car_ad$body == body, ]
  mean_volume <- mean(body_subset$engV, na.rm = TRUE)
  
  car_ad$engV[car_ad$body == body] <- 
    ifelse(is.na(car_ad$engV[car_ad$body == body]), mean_volume, car_ad$engV[car_ad$body == body])
}



# Column Drive
car_ad$drive_binary <- ifelse(car_ad$drive == "full", 1,
                              ifelse(car_ad$drive == "rear", 0,
                                     ifelse(car_ad$drive == "front", 2, NA)))

# replacing missing values of drive column by finding mean by grouping model of the car
missing_values_by_model <- car_ad %>%
  group_by(model) %>%
  summarise_all(~ sum(is.na(.)))
print("Missing values for each column based on the 'model' column:")
print(missing_values_by_model)

model_values <- unique(car_ad$model)

# Loop through each model category and replace missing values in drive column with the mean
for (model in model_values) {
  model_subset <- car_ad[car_ad$model == model, ]
  mean_drive <- mean(model_subset$drive_binary, na.rm = TRUE)
  
  car_ad$drive_binary[car_ad$model == model] <- 
    ifelse(is.na(car_ad$drive_binary[car_ad$model == model]), mean_drive, car_ad$drive_binary[car_ad$model == model])
}

car_ad$engV <- round(car_ad$engV, 2)
car_ad$mileage <- trunc(car_ad$mileage)
car_ad$drive_binary <- trunc(car_ad$drive_binary)

str(car_ad)

# creating a new variable: age of the car
car_ad$Age <- 2024 - car_ad$year

# B. Removing Irrelevant Features & rows with missing values
car_ad <- car_ad %>% select(-car, -year, -drive)
car_ad <- na.omit(car_ad)  


# C. Dealing with Outliers
# Define a function to detect and handle outliers using z-score
handle_outliers <- function(x) {
  z <- abs((x - mean(x)) / sd(x))
  threshold <- 3  # Adjust the threshold as needed (e.g., 2 or 3)
  x[z > threshold] <- NA  # Set outliers to NA
  return(x)
}

# Apply the function to relevant numerical columns (e.g., 'price', 'mileage', 'engV')
car_ad$price <- handle_outliers(car_ad$price)
car_ad$mileage <- handle_outliers(car_ad$mileage)

# Create a scatter plot
plot(car_ad$price, car_ad$mileage, pch = 16, col = ifelse(is.na(car_ad$price) | is.na(car_ad$mileage), "red", "blue"),
     main = "Scatter Plot of Price vs Mileage with Outliers",
     xlab = "Price", ylab = "Mileage")

# Highlight outliers in red
outliers <- which(is.na(car_ad$price) | is.na(car_ad$mileage))
points(car_ad$price[outliers], car_ad$mileage[outliers], pch = 16, col = "red")

#Section 2
# Scatter plot matrix to visualize relationships
pairs(car_ad[, c("price","mileage","Age")])

# Correlation matrix
cor_matrix <- cor(car_ad[, c("price", "mileage", "Age")])
print(cor_matrix)

corrplot::corrplot(cor_matrix, method = "circle")

# Choose variables with high correlation with the target variable (e.g., 'price')
selected_variables <- names(which(cor_matrix[, "price"] > 0.5 | cor_matrix[, "price"] < -0.5))
print(selected_variables)


#                    R SHINY    
# UI
ui <- fluidPage(
  titlePanel("Car Price Prediction App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Select Predictive Model:", choices = c("Decision Tree", "Linear Regression")),
      selectInput("body_type", "Select Body Type:", choices = unique(car_ad$body)),
      numericInput("year", "Enter Car Year:", value = 2010, min = 1980, max = 2024),
      numericInput("engine_volume", "Enter Engine Volume:", value = 2.0, min = 0, max = 10, step = 0.1),
      actionButton("predictButton", "Predict Price"),
      verbatimTextOutput("predictionText")
    ),
    
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Server
server <- function(input, output) {
  # Reactive data for training
  model_data <- reactive({
    filter(car_ad, body == input$body_type) %>%
      select(price, Age, engV, body) %>%
      drop_na()  # Remove rows with missing values
  })
  
  # Train predictive models based on user selection
  model <- reactive({
    switch(input$model,
           "Decision Tree" = rpart(price ~ Age + engV, data = model_data(), method = "anova"),
           "Linear Regression" = lm(price ~ Age + engV, data = model_data()))
  })
  
  # Predictions
  output$predictionText <- renderText({
    if (input$predictButton > 0) {
      new_data <- data.frame(Age = 2024 - input$year, engV = input$engine_volume, body = input$body_type)
      # Remove missing values in new data
      new_data <- na.omit(new_data)
      
      prediction <- predict(model(), newdata = new_data)
      paste("Predicted Price: $", round(prediction, 2))
    }
  })
  
  # Scatter plot
  output$scatterPlot <- renderPlot({
    plot(model_data()$Age, model_data()$price, pch = 16, col = "blue",
         main = "Scatter Plot of Age vs Price",
         xlab = "Age", ylab = "Price")
  })
}

# Run the app
shinyApp(ui, server)
