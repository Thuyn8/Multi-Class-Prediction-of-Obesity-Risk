# load libraries used for the project
    library(dplyr)
    library(table1)
    library(ggplot2)
    library(psych)
# Set working directory and load the dataset
    setwd('~/Documents/github/Multi-Class-Prediction-of-Obesity-Risk/data')
    obesity <- read.csv('test.csv')

# summarize the missing data --> no missing data
    sapply(obesity[,-1] , function(i) 100*mean(is.na(i)))
# Convert character data into factor
    final_obesity <- obesity %>% lapply(function(i) {
        if (is.character(i)) i= factor(i, levels= unique(i))
        else  i }) %>%  as.data.frame()

# The columns name's meanings: 
## Eating habits:
    # Frequent consumption of high caloric food (FAVC), 
    # Frequency of consumption of vegetables (FCVC), 
    # Number of main meals (NCP), 
    # Consumption of food between meals (CAEC), 
    # Consumption of water daily (CH20), 
    # and Consumption of alcohol (CALC). 
    label(final_data$FAVC) <- "Frequent consumption of high caloric food"
    label(final_data$FCVC) <- "Frequency of consumption of vegetables"
    label(final_data$NCP)  <- "Number of main meals"
    label(final_data$CAEC) <- "Consumption of food between meals"
    # label(final_data$CH20) <- "Consumption of water daily"
    label(final_data$CALC) <- "Consumption of alcohol"
## physical condition are: 
    # Calories consumption monitoring (SCC), 
    # Physical activity frequency (FAF), 
    # Time using technology devices (TUE), 
    # Transportation used (MTRANS)
    label(final_data$SCC) <- "Calories consumption mornitoring"
    label(final_data$FAF) <-"Physical activity frequency"
    label(final_data$TUE) <- "Time using technology devices"
    label(final_data$MTRANS) <- "Transportation used"
## scatter plot of height and weight
    ggplot(data = final_data, aes(Height, Weight)) + geom_point() + geom_smooth()
## Calculate BMI = weight(kg) / height^2 (in m)
    final_data$BMI <- final_data$Weight/(final_data$Height^2)
## create BMI categories 
##  •Underweight Less than 18.5
    # •Normal 18.5 to 24.9
    # •Overweight 25.0 to 29.9
    # •Obesity I 30.0 to 34.9
    # •Obesity II 35.0 to 39.9
    # •Obesity III Higher than 40
    
    final_data$BMI_cat <- final_data$BMI %>% cut(breaks= c(12.8,18.5, 24.9, 29.9, 34.9,40, 51.3),
                                                 label = c("Underweight","Normal", "Overweight",
                                                           "Obesity I", "Obesity II", "Obesity III"))
    table1(~.|BMI_cat, data = final_data[,-1])
    
    