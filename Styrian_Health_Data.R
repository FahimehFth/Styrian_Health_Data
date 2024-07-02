library(ggplot2)
library(gridExtra)
library(grid)
library(ggcorrplot)
library(ggrepel)
library(ggpmisc)
library(skimr)
library(lubridate)
library(ggmosaic)
library(dplyr)
library(nnet)
library(MASS)
library(caret)
library(rnaturalearth)
library(sp)
library(sf)
library(janitor)
library(viridis)

#read *.csv file from the system drive
health_data <- readxl::read_xlsx(file.choose(), na = "null")
colnames(health_data)[colnames(health_data) == "schaetzwert_by_dia"] <- "schaetzwert_bp_dia"

health_data$postleitzahl <- as.character(health_data$postleitzahl)
health_data[c("postleitzahl", "gemeinde", "bezirk", "bundesland")][
  is.na(health_data[c("postleitzahl", "gemeinde", "bezirk", "bundesland")])] <- "null"

head(health_data)

#Summary
summary(health_data)

#Variables Type
str(health_data)

Styrian_data <- NULL
Styrian_data <- health_data

#Change data type of the columns 
health_data$terminal <- as.factor(health_data$terminal)
health_data$postleitzahl <- as.factor(health_data$postleitzahl)
health_data$gemeinde <- as.factor(health_data$gemeinde)
health_data$bezirk <- as.factor(health_data$bezirk)
health_data$bundesland <- as.factor(health_data$bundesland)
health_data$befinden <- as.factor(health_data$befinden)
health_data$geschlecht <- as.factor(health_data$geschlecht)
health_data$schaetzwert_bp_sys <- as.numeric(health_data$schaetzwert_bp_sys)
health_data$schaetzwert_bp_dia <- as.numeric(health_data$schaetzwert_bp_dia)

#New variable 'age'
health_data$age <- 2006 - health_data$geburtsjahr


data <- health_data
data$messwert_bp_sys[1]
seq(0, nrow(data))

for (count in seq(1, nrow(data))){
  if((data$messwert_bp_sys[count] %in% c(0:119)) & (data$messwert_bp_dia[count] %in% c(0:79)))
    data$category[count] <- "Optimal"
  if((data$messwert_bp_sys[count] %in% c(120:129)) | (data$messwert_bp_dia[count] %in% c(80:84)))
    data$category[count] <- "Normal"
  if((data$messwert_bp_sys[count] %in% c(130:139)) | (data$messwert_bp_dia[count] %in% c(85:89)))
    data$category[count] <- "High Normal"
  if((data$messwert_bp_sys[count] %in% c(140:159)) | (data$messwert_bp_dia[count] %in% c(90:99)))
    data$category[count] <- "Stage 1 Hypertension"
  if((data$messwert_bp_sys[count] %in% c(160:179)) | (data$messwert_bp_dia[count] %in% c(100:109)))
    data$category[count] <- "Stage 2 Hypertension"
  if((data$messwert_bp_sys[count] %in% c(180:250)) | (data$messwert_bp_dia[count] %in% c(110:150)))
    data$category[count] <- "Stage 3 Hypertension"
}

for (count in seq(1, nrow(data))){
  if((data$messwert_bp_sys[count] %in% c(0:119)) & (data$messwert_bp_dia[count] %in% c(0:79)))
    data$subcategory[count] <- "Optimal"
  if((data$messwert_bp_sys[count] %in% c(120:129)) | (data$messwert_bp_dia[count] %in% c(80:84)))
    data$subcategory[count] <- "Normal"
  if((data$messwert_bp_sys[count] %in% c(130:139)) | (data$messwert_bp_dia[count] %in% c(85:89)))
    data$subcategory[count] <- "High Normal"
  if((data$messwert_bp_sys[count] %in% c(140:250)) | (data$messwert_bp_dia[count] %in% c(90:150)))
    data$subcategory[count] <- "Hypertension"
  
}

health_data <- data

health_data$category <- as.factor(health_data$category)
health_data$category <- factor(health_data$category, 
                               levels = c("Optimal", "Normal", "High Normal", "Stage 1 Hypertension",
                                          "Stage 2 Hypertension", "Stage 3 Hypertension"))

#-------------------------------------------------------------------------------

health_data$subcategory <- as.factor(health_data$subcategory)
health_data$subcategory <- factor(health_data$subcategory, 
                                  levels = c("Optimal", "Normal", "High Normal", "Hypertension"))
#-------------------------------------------------------------------------------
# Create a new categorical variable for daytime (Morning, evening, afternoon)
health_data$hour <- format(health_data$zeit, "%H")
health_data$daytime <- with(health_data,  
                            ifelse(hour %in% c(8:11),
                                   "Morning",
                                   ifelse(hour %in% c(12:15),
                                          "Afternoon",
                                          "Evening")))
health_data$daytime <- as.factor(health_data$daytime)


#-------------------------------------------------------------------------------
# Create a new categorical variable for season (Spring, Summer, Fall)
health_data$month <- month(as.Date(health_data$zeit), label = TRUE, abbr = TRUE)
health_data$season <- with(health_data,  
                           ifelse(month %in% c("Mar", "Apr", "May"),
                                  "Spring",
                                  ifelse(month %in% c("Jun", "Jul", "Aug"),
                                         "Summer",
                                         ifelse(month %in% c("Sep", "Oct", "Nov"),
                                                "Fall",
                                                "Winter"))))
health_data$season <- as.factor(health_data$season)
health_data$season <- factor(health_data$season, levels = c("Spring", "Summer", "Fall"))
health_data$season <- relevel(health_data$season, ref = "Spring")


health_data$terminal <- with(health_data,  
                             ifelse(terminal == 1,
                                    "1",
                                    ifelse(terminal == 2,
                                           "2",      
                                           ifelse(terminal == 3 & month %in% c('Apr', 'May'),
                                                  "3a", "3b"))))

health_data$terminal <- as.factor(health_data$terminal)
health_data$terminal <- relevel(health_data$terminal, ref = "3a")

#--------------------------- Histograms ----------------------------------------
hist_plots <- list(
  ggplot(health_data, aes(x = messwert_bp_sys)) +
    geom_histogram(color = "black", fill = "blue", aes(y = ..density..)) +
    xlab("messwert_bp_sys (in mmHg)") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold")),
  
  ggplot(health_data, aes(x = messwert_bp_dia)) +
    geom_histogram(color = "black", fill = "blue", aes(y = ..density..)) +
    xlab("messwert_bp_dia (in mmHg)") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold")),
  
  ggplot(health_data, aes(x = schaetzwert_bp_sys)) +
    geom_histogram(color = "black", fill = "blue", aes(y = ..density..)) +
    xlab("schaetzwert_bp_sys (in mmHg)") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold")),
  
  ggplot(health_data, aes(x = schaetzwert_bp_dia)) +
    geom_histogram(color = "black", fill = "blue", aes(y = ..density..)) +
    xlab("schaetzwert_bp_dia (in mmHg)") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"))
)

grid.arrange(grobs = hist_plots, ncol = 2)

#-------------------------------------------------------------------------------
#----------------------------- Scatterplot -------------------------------------
#-------------------------------------------------------------------------------
scatter_plots <- list(
  ggplot(health_data, aes(x = schaetzwert_bp_sys, y = schaetzwert_bp_dia, color = geschlecht)) +
    geom_point(alpha = 0.5) +
    geom_abline() +
    xlim(0, 300) +
    ylim(0, 250) +
    xlab("schaetzwert_bp_sys (in mmHg)") +
    ylab("schaetzwert_bp_dia (in mmHg)") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold")),
  
  ggplot(health_data, aes(x = messwert_bp_sys, y = messwert_bp_dia, color = geschlecht)) +
    geom_point(alpha = 0.5) +
    geom_abline() +
    xlim(0, 300) +
    ylim(0, 250) +
    xlab("messwert_bp_sys (in mmHg)") +
    ylab("messwert_bp_dia (in mmHg)") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"))
)

grid.arrange(grobs = scatter_plots, ncol = 2)

#-------------------------------------------------------------------------------
#----------------------------- Box plot ----------------------------------------
#-------------------------------------------------------------------------------

# Boxplot: messwert_bp_sys vs geschlecht, colored by blutzucker_bekannt
box_sys_blutzucker_bekannt <- ggplot(data = health_data, aes(x = geschlecht, y = messwert_bp_sys)) + 
  geom_boxplot(alpha=0.6, size=0.7, aes(colour = blutzucker_bekannt)) +
  ylab("messwert_bp_sys (in mmHg)") +
  theme_minimal() +
  theme(axis.title.x = element_text(colour="Black", size=12, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

# Boxplot: messwert_bp_dia vs geschlecht, colored by blutzucker_bekannt
box_dia_blutzucker_bekannt <- ggplot(data = health_data, aes(x = geschlecht, y = messwert_bp_dia)) + 
  geom_boxplot(alpha=0.6, size=0.7, aes(colour = blutzucker_bekannt)) +
  ylab("messwert_bp_dia (in mmHg)") +
  theme_minimal() +
  theme(axis.title.x = element_text(colour="Black", size=12, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))

# Boxplot: messwert_bp_sys vs geschlecht, colored by in_behandlung
box_sys_in_behandlung <- ggplot(data = health_data, aes(x = geschlecht, y = messwert_bp_sys)) + 
  geom_boxplot(alpha=0.6, size=0.7, aes(colour = in_behandlung)) +
  ylab("messwert_bp_sys (in mmHg)") +
  theme_minimal() +
  theme(axis.title.x = element_text(colour="Black", size=12, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))

# Boxplot: messwert_bp_dia vs geschlecht, colored by in_behandlung
box_dia_in_behandlung <- ggplot(data = health_data, aes(x = geschlecht, y = messwert_bp_dia)) + 
  geom_boxplot(alpha=0.6, size=0.7, aes(colour = in_behandlung)) +
  ylab("messwert_bp_dia (in mmHg)") +
  theme_minimal() +
  theme(axis.title.x = element_text(colour="Black", size=12, face = "bold"),
        axis.title.y = element_text(colour="Black", size=12, face = "bold"),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))

# Boxplot: messwert_bp_sys vs month
give.n <- function(x){
  return(c(y = median(x), label = length(x)))
}

sys_box_month <- ggplot(data = health_data, aes(x = month, y = messwert_bp_sys)) +
  geom_boxplot(alpha=0.6, size=0.7, color = "darkturquoise") +
  ylab("messwert_bp_sys (in mmHg)") +
  xlab("Month") +
  stat_summary(fun.data = give.n, geom = "text", position = position_dodge(width = 1),
               vjust = -2.5, size = 5) +
  theme_minimal() +
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=12),
        legend.text=element_text(size=10), 
        strip.text = element_text(size=10),
        plot.title = element_text(size=12, face = "bold"))

# Plot the box plots
plot(box_sys_blutzucker_bekannt)
plot(box_dia_blutzucker_bekannt)
plot(box_sys_in_behandlung)
plot(box_dia_in_behandlung)
plot(sys_box_month)


#-------------------------------------------------------------------------------
#---------------------------- Time-date format --------------------------------- 
#-------------------------------------------------------------------------------
health_data$month <- format(health_data$zeit, "%m:%Y") 
health_data$time_hour <- format(health_data$zeit, "%H")

health_data$day_time <- with(health_data,  ifelse(time_hour %in% c(8:12), "Morning(08:00-12:00)",
                                                  ifelse(time_hour %in% c(12:16), "Afternoon(12:00-16:00)",
                                                         ifelse(time_hour %in% c(16:20), "Evening(16:00-20:00)", "Night(20:00-08:00)"))))
health_data$day_time <- as.factor(health_data$day_time)
health_data$day_time <- factor(health_data$day_time, 
                               levels = c("Morning(08:00-12:00)", "Afternoon(12:00-16:00)",
                                          "Evening(16:00-20:00)", "Night(20:00-08:00)"))

month <- month(as.Date(health_data$zeit), label = TRUE)
month_ <- as.factor(month) 
month_ <- factor(month_, exclude = c("January", "February", "December"))

# Create the week_time plot
week_time <- health_data %>% 
  mutate(wday = wday(health_data$zeit, label = TRUE, week_start = 1)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(wday), fill = geschlecht)) +
  xlab("Week (in days)") +
  theme_minimal() +
  theme(axis.title.x = element_text(colour = "Black", size = 12, face = "bold"),
        axis.title.y = element_text(colour = "Black", size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# Create the month_time plot
month_time <- ggplot(data = health_data) +
  geom_mosaic(aes(x = product(month_), fill = geschlecht)) +
  xlab("Months in year 2006") +
  theme_minimal() +
  theme(axis.title.x = element_text(colour = "Black", size = 12, face = "bold"),
        axis.title.y = element_text(colour = "Black", size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# Create the Day_time plot
Day_time <- ggplot(data = health_data) +
  geom_mosaic(aes(x = product(day_time), fill = geschlecht)) +
  xlab("Day duration (in hrs:mins)") +
  theme_minimal() +
  theme(axis.title.x = element_text(colour = "Black", size = 12, face = "bold"),
        axis.title.y = element_text(colour = "Black", size = 12, face = "bold"),
        axis.text.x = element_text(size = 10, angle = 15, vjust = 0.7),
        axis.text.y = element_text(size = 10))

# Arrange the plots in a grid
grid.arrange(week_time, month_time, Day_time, ncol = 1)

# Create the cat_geschlect plot
cat_geschlect <- ggplot(data = health_data) +
  geom_mosaic(aes(x = product(category), fill = geschlecht)) +
  xlab("Blood pressure categories") +
  theme_minimal() +
  theme(axis.title.x = element_text(colour = "Black", size = 12),
        axis.title.y = element_text(colour = "Black", size = 12),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 10))

# Create the cat_in_behandlung plot
cat_in_behandlung <- ggplot(data = health_data) +
  geom_mosaic(aes(x = product(category), fill = in_behandlung)) +
  xlab("Blood pressure categories") +
  theme_minimal() +
  theme(axis.title.x = element_text(colour = "Black", size = 12),
        axis.title.y = element_text(colour = "Black", size = 12),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 10))

# Arrange the plots in a grid
grid.arrange(cat_geschlect, cat_in_behandlung)

# Create the plot for raucher
ggplot(data = health_data) +
  geom_mosaic(aes(x = product(category), fill = raucher)) +
  xlab("Blood pressure categories") +
  theme_minimal() +
  theme(axis.title.x = element_text(colour = "Black", size = 12),
        axis.title.y = element_text(colour = "Black", size = 12),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 10))
#-------------------------------------------------------------------------------
#---------------------------correlation matrix----------------------------------
#-------------------------------------------------------------------------------


Styrian_data$age <- 2006 - Styrian_data$geburtsjahr
Styrian_data <- na.omit(Styrian_data)

ggcorrplot(cor(Styrian_data[c("age", "messwert_bp_sys", "messwert_bp_dia",
                              "befinden", "raucher", "blutzucker_bekannt", 
                              "cholesterin_bekannt","in_behandlung")]),
           hc.order = TRUE, lab = TRUE, pch.cex = 1.2, type = "lower") +
  theme(axis.text.x = element_text(size=10, face = "bold", angle = 90),
        axis.text.y = element_text(size=10, face = "bold"))


#--------------------------------map plots--------------------------------------
mean_sys_bundesland <- health_data %>%
  group_by(bundesland) %>%
  summarise_at(vars(messwert_bp_sys), list(name = mean))


mean_dia_bundesland <- health_data %>%
  group_by(bundesland) %>%
  summarise_at(vars(messwert_bp_dia), list(name = mean))

percent_cat <- tabyl(health_data, bundesland, category) %>%
  adorn_percentages("row") #%>%


Austria <- ne_states(country = "Austria", returnclass = "sf")

ymod_shape <- NULL
mod_shape <- Austria %>% 
  left_join(mean_sys_bundesland, by = c("name" = "bundesland"))

mod_shape <- mod_shape %>% 
  left_join(mean_dia_bundesland, by = c("name" = "bundesland"))

mod_shape <- mod_shape %>% 
  left_join(percent_cat, by = c("name" = "bundesland"))

sys <- ggplot(data = mod_shape) + 
  geom_sf(aes(fill = name.y)) +
  scale_fill_viridis(trans = "sqrt") +
  labs(fill = "Systolic bp (mmHg)") +
  theme_void() +
  geom_sf_label(aes(label = name),  label.padding = unit(0.5, "lines"),
                label.r = unit(0.20, "mm"),
                nudge_y = 0.07,
                label.size = 0.4)

plot(sys)

dia <- ggplot(data = mod_shape) + 
  geom_sf(aes(fill = name.y.y)) +
  scale_fill_viridis(trans = "sqrt") +
  labs(fill = "Diastolic bp (mmHg)") +
  theme_void() +
  geom_sf_label(aes(label = name),  label.padding = unit(0.5, "lines"),
                label.r = unit(0.10, "lines"),
                nudge_y = 0.07,
                label.size = 0.1)

grid.arrange(sys, dia)


#-------------------------------------------------------------------------------
#-------------------------------- Data Cleaning --------------------------------
#-------------------------------------------------------------------------------
health_data

#-------- 1478 records with age less than 15
(health_data[(health_data$age %in% c(0:14)),])
#-------- 21 records with age greater than 100
(health_data[(health_data$age %in% c(101:150)),])


df_health_data <- health_data[!(health_data$age %in% c(0:14, 101:150)),]
#-------------- 14887 records -------------
df_health_data

#---------------------------------- Date ---------------------------------------
tail(format(df_health_data$zeit, "%d %m"))

df_health_data$daydate <- as.Date(df_health_data$zeit)
#-------------------- 133 entries for months ----------------------------
df_health_data[(df_health_data$daydate < as.Date('2006-04-29')) | 
                 (df_health_data$daydate > as.Date('2006-10-29')),]

#---------------- 133 records removed ------------
df_health_data <- df_health_data[!(df_health_data$daydate < as.Date('2006-04-29')
                                   | df_health_data$daydate > as.Date('2006-10-29')),]

#-------------- 14754 records -------------
df_health_data

#---------------------------------- Time ---------------------------------------
tail(format(df_health_data$zeit, "%H:%M"))

df_health_data$hour <- format(df_health_data$zeit, "%H")
df_health_data[(df_health_data$hour %in% c(20:23)),]


#--------------------- 1 Entry from schaetzwert_bp_dia -------------------------
df_health_data <- df_health_data[!(df_health_data$schaetzwert_bp_dia %in% c(200:250)),]
#-------------- 14753 records -------------
df_health_data

#------------------- Counting number of NA values in the data -------------------
df1 <- na.omit(df_health_data)
nrow(df_health_data) - nrow(df1)

#-----------------------14699 final rows----------------------------------------
df_health_data <- df1

table(df_health_data$subcategory)

#-------------------------------------------------------------------------------
#----------------------------- Train and test  ---------------------------------
#-------------------------------------------------------------------------------
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df_health_data), 
                 replace = TRUE, prob = c(0.7, 0.3))
#------------------------------ train and test ---------------------------------
train <- df_health_data[sample, ]
test <- df_health_data[!sample, ]



#-------------------------------------------------------------------------------
#---------------------------- Assumptions --------------------------------------
#-------------------------------------------------------------------------------

#Linearity
#-------------------------------------------------------------------------------
#Logistic regression assumes that the relationship between the natural log of 
#these probabilities (when expressed as odds) and your predictor variable is linear.

#No Outliers
#-------------------------------------------------------------------------------
#Logistic Regression is sensitive to outliers, or data points that have unusually large or small values.

#Independence
#-------------------------------------------------------------------------------
#Each value of your variables doesn’t “depend” on any of the others.

#No Multicollinearity
#-------------------------------------------------------------------------------
#Multicollinearity refers to the scenario when two or more of the independent variables
#are substantially correlated amongst each other. When multicollinearity is present,
#the regression coefficients and statistical significance become unstable and less trustworthy.

#-------------------------------------------------------------------------------
#--------------------------------- MULTINOM ------------------------------------
#-------------------------------------------------------------------------------
train$subcategory <- relevel(train$subcategory, ref = "Optimal")

#Semi-Full model
df_health_data$subcategory <- relevel(train$subcategory, ref = "Optimal")


model <- multinom(subcategory ~ 
                    age + geschlecht + blutzucker_bekannt +
                    raucher + cholesterin_bekannt + in_behandlung + terminal +
                    bundesland + befinden + season + daytime,
                  data = df_health_data, maxit = 100)


slm_aic_semi_full <- step(model, direction = "both", k = 2)
slm_aic_semi_full_val <- slm_aic_semi_full$call
slm_aic_semi_full_val

#log(nrow(df_health_data))
slm_bic_semi_full <- step(model, direction = "both", k = 9.5955)
slm_bic_semi_full_val <- slm_bic_semi_full$call
slm_bic_semi_full_val


df_error <- data.frame()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
model.null <- multinom(subcategory ~ 1,
                       data = train, maxit = 100)

#-------------------------------------------------------------------------------
#Confusion matrix for training data
conf_train <- predict(model.null, train)
tab_train <- table(conf_train, train$subcategory)
#Misclassification error for training
train_error <- 1 - sum(diag(tab_train))/sum(tab_train)

#-------------------------------------------------------------------------------
#Confusion matrix for testing data
conf_test <- predict(model.null, test)
tab_test <- table(conf_test, test$subcategory)
#Misclassification error for testing
test_error <- 1 - sum(diag(tab_test))/sum(tab_test)
df_error <- rbind(df_error, c("Null", round(train_error, 5), round(test_error, 5)))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
model.semi <- multinom(subcategory ~ 
                         age + geschlecht + blutzucker_bekannt +
                         raucher + cholesterin_bekannt + in_behandlung + terminal +
                         bundesland + befinden + season + daytime,
                       data = train, maxit = 100)
#-------------------------------------------------------------------------------
#Confusion matrix for training data
conf_train <- predict(model.semi, train)
tab_train <- table(conf_train, train$subcategory)
#Misclassification error for training
train_error <- 1 - sum(diag(tab_train))/sum(tab_train)

#-------------------------------------------------------------------------------
#Confusion matrix for testing data
conf_test <- predict(model.semi, test)
tab_test <- table(conf_test, test$subcategory)
#Misclassification error for testing
test_error <- 1 - sum(diag(tab_test))/sum(tab_test)
df_error <- rbind(df_error, c("Semi Full", round(train_error, 3), round(test_error, 3)))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
model.aic <- multinom(subcategory ~ age + geschlecht + blutzucker_bekannt + 
                        in_behandlung + terminal + season + daytime,
                      data = train, maxit = 100)

#-------------------------------------------------------------------------------
#Confusion matrix for training data
conf_train <- predict(model.aic, train)
tab_train <- table(conf_train, train$subcategory)
#Misclassification error for training
train_error <- 1 - sum(diag(tab_train))/sum(tab_train)

#-------------------------------------------------------------------------------
#Confusion matrix for testing data
conf_test <- predict(model.aic, test)
tab_test <- table(conf_test, test$subcategory)
#Misclassification error for testing
test_error <- 1 - sum(diag(tab_test))/sum(tab_test)
df_error <- rbind(df_error, c("AIC",  round(train_error, 3), round(test_error, 3)))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
model.bic <- multinom(subcategory ~ age + geschlecht + blutzucker_bekannt + 
                        in_behandlung + terminal + season,
                      data = train, maxit = 100)
#-------------------------------------------------------------------------------
#Confusion matrix for training data
conf_train <- predict(model.bic, train)
tab_train <- table(conf_train, train$subcategory)
#Misclassification error for training
train_error <- 1 - sum(diag(tab_train))/sum(tab_train)

#-------------------------------------------------------------------------------
#Confusion matrix for testing data
conf_test <- predict(model.bic, test)
tab_test <- table(conf_test, test$subcategory) 
#Misclassification error for testing
test_error <- 1 - sum(diag(tab_test))/sum(tab_test)
df_error <- rbind(df_error, c("BIC",  round(train_error, 3), round(test_error, 3)))



colnames(df_error) <- c("Model", "Train Error", "Test Error")
df_error

summary(model.bic)
round(coef(model.bic),3)
round(exp(coef(model.bic)),3)

#---------------------------- 2-tailed z-test ----------------------------------
p_values <- (1 - pnorm(abs(summary(model.bic)$coefficients/summary(model.bic)$standard.errors), 0, 1))*2
p_values
