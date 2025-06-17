# Hannah Stouter
# HLS
# May 22, 2024

setwd("/Users/hstouter/Library/CloudStorage/Box-Box/NASA - Land Cover Land Use Change (2023 - 2026)/Data/LCLUC/RF_Datasets/")  
source("LCLUC_random_forest_functions.R")

library(sp)
library(raster)
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(randomForest)
library(caTools)
library(gridExtra)
library(compositions)
library(rpart)
library(ggplot2)
library(ggridges)
library(lattice)
library(caret)
library(data.table)



# Load raster data
S30 <- rast("test_may_2025//EVI_HLS/2022_test_small/HLS_composite/S30_2022_median.tif")
L30 <- rast("test_may_2025/EVI_HLS/2022_test_small/HLS_composite/L30_2022_median.tif")
DEM <- rast("test_may_2025/STRM_DEM/dem.tif")
GLCM_red <- rast("glcm/S30_2022_glcm_red.tif")
GLCM_nir <- rast("glcm/S30_2022_glcm_NIR_narrow.tif")
S1 <- rast("test_may_2025/Sentinel_1/s1_2022_test.tif")
labels <- vect("all_labels/all_labels.shp")
roi <- vect("/Users/hstouter/Desktop/NASA_LCLUC/RF_datasets/ynd_test_buffer.shp")
roi <- project(roi, "EPSG:4326")

# Subset labels to ROI

labels <- crop(labels, roi)

# Check labels
ggplot()+
  geom_point(aes( x = labels$long, y = labels$lat, color = labels$class))+
  theme_classic()

# Calculate EVI
EVI_S30 <- 2.5 * ((S30$NIR_narrow-S30$red) / (S30$NIR_narrow + 6 * S30$red - 7.5 * S30$blue + 1))
EVI_L30 <- 2.5 * ((L30$NIR_narrow-L30$red) / (L30$NIR_narrow + 6 * L30$red - 7.5 * L30$blue + 1))


EVI_S30 <- clamp(EVI_S30, lower = -1, upper = 1, values = FALSE)
EVI_L30 <- clamp(EVI_L30, lower = -1, upper = 1, values = FALSE)


names(EVI_S30) <- "EVI_S30"
names(EVI_S30) <- "EVI_L30"

# GLCM names 
GLCM_names <- c("GLCM_mean", "GLCM_variance", "GLCM_homogeneity", "GLCM_contrast", "GLCM_dissimilarity", "GLCM_entropy", "GLCM_second_moment", "GLCM_correlation")

names(GLCM_nir) <- paste0(GLCM_names, "_nir")
names(GLCM_red) <- paste0(GLCM_names,"_red")

# Update HLS names 
names(S30) <- paste0(names(S30), "_S30")
names(L30) <- paste0(names(L30), "_L30")


# stack all datasets 

RS_data <- list(L30, S30, EVI_L30, EVI_S30, S1, DEM, GLCM_red, GLCM_nir)

RS_data <- align_rasters(RS_data, roi)

rasters <- rast(RS_data)

plot(rasters)

#extract pixels values for each label 

all_label_data <- predictor_dataset(rasters, labels)

# Create data set with only forest and non-forest labels 

forest <- subset_labels(all_label_data, c("forest"))
forest_water_bare <- subset_labels(all_label_data, c("forest", "water", "bare"))

##### SUBSET DATA INTO TEST AND TRAINING ####

# Set validation scheme 
cross_val <- caret::trainControl(method = "LGOCV", p = 0.7, number = 5)

# Split data into test and training
forest_data <- split_data(forest)

##### RANDOM FOREST ##### 

# Forest and Non-forest, 500 trees

train_forest <- forest_data[[1]]

# Handel NA values 
train_forest_rmNA <- train_forest %>% select(-GLCM_correlation_nir, GLCM_contrast_red)
train_forest_rmNA <- na.roughfix(train_forest)
train_forest_rmNA$class <- as.factor(train_forest_rmNA$class)


RF_forest <- randomForest(class ~ ., data = train_forest_rmNA, trControl = cross_val, na.action = na.omit, ntrees = 500)
RF_forest
varImpPlot(RF_forest, main = "Forest/Non-forest (500 trees)")

test_forest_rmNA <- test_forest %>% select(-GLCM_correlation_nir, GLCM_contrast_red)

test_forest <- forest_data[[2]]
test_forest_rmNA <- na.roughfix(test_forest)

predict_RF_forest <- predict(RF_forest, newdata=test_forest_rmNA, type="class")     
summary(predict_RF_forest)

rasters_dat <- as.data.table(rasters)
rasters_dat_rmNA <- na.roughfix(rasters_dat)

predict_RF_forest <- predict(RF_forest, newdata=rasters_dat_rmNA, type="class")     
summary(predict_RF_forest)


get number of pixel
cell_indices <- 1:ncell(rasters)

# extract coordinates from each pixel 
rast_coords <- xyFromCell(rasters, cell_indices)

raster_stack_data <- as.data.frame(rasters)
raster_stack_data <- as.data.table(rasters)
head(raster_stack_data)

# combine coordinates with band values 
pixels_coords <- cbind(rast_coords, raster_stack_data)


# convert to matrix 
pixels_coords <- as.matrix(pixels_coords)


# create an empty row for forest to match test data set
class <- rep(NA, nrow(pixels_coords))

# rename columns to match test data set 
HLS_forest <- cbind(class, pixels_coords)
colnames(HLS_forest)[2] <- "X"
colnames(HLS_forest)[3] <- "Y"

head(HLS_forest)

template_raster <- rast(rasters)

# covert to dataframe & rename column 
forest_raster_dat <- as.data.frame(predict_RF_forest)
colnames(forest_raster_dat)[1] <- "class"

head(forest_raster_dat)

forest_raster_dat <- forest_raster_dat %>%
  mutate(forest = case_when(
    is.na(class)        ~ -1,     # missing
    class == "forest"   ~ 1,      # forest
    TRUE                ~ 0       # everything else (non-forest)
  ))

head(forest_raster_dat)

forest_raster_dat <- forest_raster_dat %>% select(-class)
colnames(forest_raster_dat)[1] <- "class"


forest_raster_dat <- cbind(rast_coords, forest_raster_dat)
head(forest_raster_dat)

# # convert to matrix to export as raster 
forest_raster_matrix <- as.matrix(forest_raster_dat)

forest_vect <- vect(forest_raster_dat, geom = c("x", "y"), crs = crs(template_raster))


# Export to raster

forest_raster <- rast(forest_raster_matrix)

# Set the extent, coordinate reference system and resolution to match original raster 
ext(forest_raster) <- ext(rasters)
crs(forest_raster) <- crs(rasters)
res(forest_raster) <- res(rasters)

# view raster 
plot(forest_raster, main = "500 trees")

# Save output raster as geoTIFF
writeRaster(forest_raster, filename = "forest_raster_test.tif", format = "GTiff")










# Forest, water, bare, non-forest, 500 trees
RF_forest_water_bare <- randomForest(train_forest_water_bare$forest_water_bare~., data = train_forest_water_bare)
RF_forest_water_bare
varImpPlot(RF_forest_water_bare, main = "Forest, Water, Bare (500 trees)")

# All-land use types, 500 trees
RF_all_label_data <- randomForest(train_all_label_data$class~., data = train_all_label_data)
RF_all_label_data 
varImpPlot(RF_all_label_data, main = "All land use types (500 trees)")


# 1000 trees 

# Forest and Non-forest, 1000 trees
RF_forest_1 <- randomForest(train_forest$forest~., data = train_forest, ntree = 1000)
RF_forest_1
varImpPlot(RF_forest_1)

# Forest, water, bare, non-forest, 1000 trees
RF_forest_water_bare_1 <- randomForest(train_forest_water_bare$forest_water_bare~., data = train_forest_water_bare, ntree = 1000)
RF_forest_water_bare_1
varImpPlot(RF_forest_water_bare)

# All-land use types, 1000 trees
RF_all_label_data_1 <- randomForest(train_all_label_data$class~., data = train_all_label_data, ntree = 1000)
RF_all_label_data_1 
varImpPlot(RF_all_label_data_1)


# 10000 trees

# Forest and Non-forest, 10000 trees
RF_forest_10 <- randomForest(train_forest$forest~., data = train_forest, ntree = 10000)
RF_forest_10
varImpPlot(RF_forest_10)

# Forest, water, bare, non-forest, 10000 trees
RF_forest_water_bare_10 <- randomForest(train_forest_water_bare$forest_water_bare~., data = train_forest_water_bare, ntree = 10000)
RF_forest_water_bare_10
varImpPlot(RF_forest_water_bare_10)

# All-land use types, 1000 trees
RF_all_label_data_10 <- randomForest(train_all_label_data$class~., data = train_all_label_data, ntree = 10000)
RF_all_label_data_10 
varImpPlot(RF_all_label_data_10)

# 10000 trees

# # Forest and Non-forest, 100,000 trees
# RF_forest_100 <- randomForest(train_forest$forest~., data = train_forest, ntree = 100000)
# RF_forest_100
# varImpPlot(RF_forest_100)
# 
# # Forest, water, bare, non-forest, 100,000 trees
# RF_forest_water_bare_100 <- randomForest(train_forest_water_bare$forest_water_bare~., data = train_forest_water_bare, ntree = 100000)
# RF_forest_water_bare_100
# varImpPlot(RF_forest_water_bare_100)
# 
# # All-land use types, 100,000 trees
# RF_all_label_data_100 <- randomForest(train_all_label_data$class~., data = train_all_label_data, ntree = 100000)
# RF_all_label_data_100
# varImpPlot(RF_all_label_data_100)


# Predict model with 500 trees onto test data and accuracy assessment 

# Forest 
 
predict_RF_forest <- predict(RF_forest, newdata=test_forest, type="class")     


# Determine kappa accuracy 
kappa_forest <- confusionMatrix(test_forest$forest, predict_RF_forest)
kappa_forest



kappa_forest_table <- kappa_forest$table
kappa_forest_table

kappa_forest_data <- as.data.frame(kappa_forest_table)


ggplot()+
  geom_tile(aes(x = kappa_forest_data$Reference, y = kappa_forest_data$Prediction, 
                fill = kappa_forest_data$Freq))+
  theme_classic()


# Builting table with total values 
total_cols <- colSums(kappa_forest_table)
total_rows <- rowSums(kappa_forest_table)
forest_accuracy <- rbind(kappa_forest_table, 'total' = colSums(kappa_forest_table))
forest_accuracy <- cbind(forest_accuracy, 'total' = rowSums(forest_accuracy))
forest_accuracy

forest_accuracy_data <- as.data.frame(forest_accuracy)

actual <- forest_accuracy_data %>% gather(forest_accuracy_data, forest:users)

forest_heatmap <- ggplot()+
  geom_heat()

# Producers accuracy 
producers_accuracy <- diag(kappa_forest_table) / colSums(kappa_forest_table)
forest_accuracy <- rbind(forest_accuracy, "producers" = producers_accuracy)
forest_accuracy

# Users accuracy 
users_accuracy <- diag(kappa_forest_table) / rowSums(kappa_forest_table)
forest_accuracy <- cbind(forest_accuracy, "users" = users_accuracy)

# Overall accuracy
forest_accuracy

# Forest, water, bare, non-forest 
predict_RF_forest_water_bare<- predict(RF_forest_water_bare, newdata=test_forest_water_bare, type="class") 

# Kappa accuracy 
kappa_forest_water_bare <- confusionMatrix(test_forest_water_bare$forest_water_bare, predict_RF_forest_water_bare)
kappa_forest_water_bare

kappa_forest_water_bare_table <- kappa_forest_water_bare$table
kappa_forest_water_bare_table

# Builting table with total values 
total_cols <- colSums(kappa_forest_water_bare_table)
total_rows <- rowSums(kappa_forest_water_bare_table)
forest_water_bare_accuracy <- rbind(kappa_forest_water_bare_table, 'total' = colSums(kappa_forest_water_bare_table))
forest_water_bare_accuracy <- cbind(forest_water_bare_accuracy, 'total' = rowSums(forest_water_bare_accuracy))
forest_water_bare_accuracy

# Producers accuracy 
producers_accuracy <- diag(kappa_forest_water_bare_table) / colSums(kappa_forest_water_bare_table)
producers_accuracy_round <- round(producers_accuracy, 3)
forest_water_bare_accuracy <- rbind(forest_water_bare_accuracy, "producers" = producers_accuracy_round)
forest_water_bare_accuracy

# Users accuracy 
users_accuracy <- diag(kappa_forest_water_bare_table) / rowSums(kappa_forest_water_bare_table)
users_accuracy_round <- round(users_accuracy, 3)
forest_water_bare_accuracy <- cbind(forest_water_bare_accuracy, "users" = users_accuracy_round)

forest_water_bare_accuracy

# All land use types 
predict_RF_all_label_data<- predict(RF_all_label_data, newdata=test_all_label_data, type="class")

# Kappa accuracy 
kappa_all_label_data <- confusionMatrix(test_all_label_data$class, predict_RF_all_label_data)
kappa_all_label_data

kappa_all_label_data_table <- kappa_all_label_data$table
kappa_all_label_data_table

all_label_data_data <- as.data.frame(kappa_all_label_data_table)

# Normalize to percentages
all_label_data_data <- all_label_data_data %>%
  mutate(Percentage = Freq / sum(Freq) * 100)

all_label_data_data$Percentage <- round(all_label_data_data$Percentage, digits = 0)

ggplot()+
  geom_tile(aes(x=all_label_data_data$Prediction, y = all_label_data_data$Reference, fill = all_label_data_data$Freq))+
  geom_text(aes(x=all_label_data_data$Prediction, y = all_label_data_data$Reference, label = all_label_data_data$Freq), color = "white")+
  labs(fill = "Percent accurate")+
  scale_fill_continuous(type = "gradient") +
  theme_classic()





# Builting table with total values 
total_cols <- colSums(kappa_all_label_data_table)
total_rows <- rowSums(kappa_all_label_data_table)
all_label_data_accuracy <- rbind(kappa_all_label_data_table, 'total' = colSums(kappa_all_label_data_table))
all_label_data_accuracy <- cbind(all_label_data_accuracy, 'total' = rowSums(all_label_data_accuracy))
all_label_data_accuracy

# Producers accuracy 
producers_accuracy <- diag(kappa_all_label_data_table) / colSums(kappa_all_label_data_table)
producers_accuracy_round <- round(producers_accuracy, 3)
all_label_data_accuracy <- rbind(all_label_data_accuracy, "producers" = producers_accuracy_round)
all_label_data_accuracy

# Users accuracy 
users_accuracy <- diag(kappa_all_label_data_table) / rowSums(kappa_all_label_data_table)
users_accuracy_round <- round(users_accuracy, 3)
all_label_data_accuracy <- cbind(all_label_data_accuracy, "users" = users_accuracy_round)

all_label_data_accuracy



# # Forest and Oil Palm
# predict_RF_opp<- predict(RF_opp_LU, newdata=test_opp_LU, type="class") 
# table(predict_RF_opp)
# table(test_opp_LU$forest_opp)

# Predict model with 1000 trees onto test data 

# Forest 
predict_RF_forest_1 <- predict(RF_forest_1, newdata=test_forest, type="class") 
table(predict_RF_forest_1)
table(test_forest$forest)

# Forest, water, bare, non-forest 
predict_RF_forest_water_bare_1<- predict(RF_forest_water_bare_1, newdata=test_forest_water_bare, type="class") 
table(predict_RF_forest_water_bare_1)
table(test_forest_water_bare$forest_water_bare)


# All land use types 
predict_RF_all_label_data_1<- predict(RF_all_label_data_1, newdata=test_all_label_data, type="class") 
table(predict_RF_all_label_data_1)
table(test_all_label_data$class)


# Predict model with 10000 trees onto test data 

# Forest 
predict_RF_forest_10 <- predict(RF_forest_10, newdata=test_forest, type="class") 
table(predict_RF_forest_10)
table(test_forest$forest)

# Forest, water, bare, non-forest 
predict_RF_forest_water_bare_10<- predict(RF_forest_water_bare_10, newdata=test_forest_water_bare, type="class") 
table(predict_RF_forest_water_bare_10)
table(test_forest_water_bare$forest_water_bare)


# All land use types 
predict_RF_all_label_data_10<- predict(RF_all_label_data_10, newdata=test_all_label_data, type="class") 
table(predict_RF_all_label_data_10)
table(test_all_label_data$class)

# Predict model with 100,000 trees onto test data 

# Forest 
predict_RF_forest_100 <- predict(RF_forest_100, newdata=test_forest, type="class") 
table(predict_RF_forest_100)
table(test_forest$forest)

# Forest, water, bare, non-forest 
predict_RF_forest_water_bare_100 <- predict(RF_forest_water_bare_100, newdata=test_forest_water_bare, type="class") 
table(predict_RF_forest_water_bare_100)
table(test_forest_water_bare$forest_water_bare)


# All land use types 
predict_RF_all_label_data_100<- predict(RF_all_label_data_100, newdata=test_all_label_data, type="class") 
table(predict_RF_all_label_data_100)
table(test_all_label_data$class)
# Memory error with 100,000 trees 


##### PREDICT LAND USE OVER ENTIRE AREA ####
#Extract coordinates for each cell in the HLS raster 

# get number of pixel
cell_indices <- 1:ncell(rasters)

# extract coordinates from each pixel 
rast_coords <- xyFromCell(rasters, cell_indices)

raster_stack_data <- as.data.frame(rasters)
raster_stack_data <- as.data.table(rasters)
head(raster_stack_data)

# combine coordinates with band values 
pixels_coords <- cbind(rast_coords, raster_stack_data)


# convert to matrix 
pixels_coords <- as.matrix(pixels_coords)


# create an empty row for forest to match test data set
class <- rep(NA, nrow(pixels_coords))

# rename columns to match test data set 
HLS_forest <- cbind(class, pixels_coords)
colnames(HLS_forest)[2] <- "X"
colnames(HLS_forest)[3] <- "Y"

head(HLS_forest)

#### PREDICT FOREST ####

# 500 trees
predict_forest <- predict(RF_forest, newdata=HLS_forest, type="class") 
table(predict_forest)

# covert to dataframe & rename column 
forest_raster_dat <- as.data.frame(predict_forest)
colnames(forest_raster_dat)[1] <- "class"

head(forest_raster_dat)

# convert columns to numeric 
# 1 = forest 
# 0 = nonforest 
# -1 = no data

forest_raster_dat <- forest_raster_dat %>%
  mutate(forest = case_when(
    is.na(class)        ~ -1,     # missing
    class == "forest"   ~ 1,      # forest
    TRUE                ~ 0       # everything else (non-forest)
  ))

# convert to matrix to export as raster 
forest_raster_dat <- as.matrix(forest_raster_dat)

# Export to raster
forest_raster <- rast(forest_raster_dat)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
ext(forest_raster) <- ext(rasters)
crs(forest_raster) <- crs(rasters)
res(forest_raster) <- res(rasters)

# view raster 
plot(forest_raster, main = "500 trees")

# Save output raster as geoTIFF
writeRaster(forest_raster, filename = "forest_raster_test.tif", format = "GTiff")


# 1000 trees
predict_RF_HLS_forest_1 <- predict(RF_forest_1, newdata=HLS_forest, type="class") 
table(predict_RF_HLS_forest_1)

# covert to dataframe & rename column 
forest_raster_dat_1 <- as.data.frame(predict_RF_HLS_forest_1)
colnames(forest_raster_dat_1)[1] <- "forest"

# convert columns to numeric 
forest_raster_dat_1$forest <- if_else(forest_raster_dat_1$forest == "forest", 1, 0)
# 1 = forest 
# 0 = nonforest 

#set no data value to -1
forest_raster_dat_1$forest <- if_else(is.na(forest_raster_dat_1$forest), -1, forest_raster_dat_1$forest)

# convert to matrix to export as raster 
forest_raster_dat_1 <- as.matrix(forest_raster_dat_1)

# Export to raster
forest_raster_1 <- raster(forest_raster_dat_1)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
extent(forest_raster_1) <- extent(rf_stack)
crs(forest_raster_1) <- crs(rf_stack)
res(forest_raster_1) <- res(rf_stack)

# view raster 
plot(forest_raster_1, main = "1000 Trees")

# Save output raster as geoTIFF
writeRaster(forest_raster_1, filename = "forest_raster_1_rf_stack.tif", format = "GTiff")



#### PREDICT FOREST, WATER, BARE ####

# Copy dataset and rename forest column to match test dataset for forest_water_bare
HLS_forest_water_bare <- HLS_forest 
colnames(HLS_forest_water_bare)[1] <- "class"

predict_RF_HLS_forest_water_bare <- predict(RF_forest_water_bare, newdata=HLS_forest_water_bare, type="class") 
table(predict_RF_HLS_forest_water_bare)

# covert to dataframe & rename column 
forest_water_bare_raster_dat <- as.data.frame(predict_RF_HLS_forest_water_bare)
colnames(forest_water_bare_raster_dat)[1] <- "class"


# Specify the mapping from factor levels to numeric values
factor_to_numeric <- c("forest" = 1, "water" = 2, "bare" = 3, "non-forest" = 0)

# Convert the factor column to a numeric column using the specified mapping
forest_water_bare_raster_dat <- forest_water_bare_raster_dat %>%
  mutate(class = as.numeric(factor_to_numeric[as.character(class)]))


# convert NA to -1 
forest_water_bare_raster_dat$class <- if_else(is.na(forest_water_bare_raster_dat$class), 
                                              -1, 
                                              forest_water_bare_raster_dat$class)


# 1 = forest 
# 2 = water
# 3 = bare 
# 0 = nonforest 
# -1 = no data value 



# convert to matrix to export as raster 
forest_water_bare_raster <- as.matrix(forest_water_bare_raster_dat)

# Export to raster
forest_water_bare_raster <- raster(forest_water_bare_raster)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
extent(forest_water_bare_raster) <- extent(rf_stack)
crs(forest_water_bare_raster) <- crs(rf_stack)
res(forest_water_bare_raster) <- res(rf_stack)

# view raster 
plot(forest_water_bare_raster , main = "Three land use classes (500 trees)")

# Save output raster as geoTIFF
writeRaster(forest_water_bare_raster, filename = "forest_water_bare_raster_rf_stack.tif", format = "GTiff")

### 10,000 TREES FOREST, WATER, BARE ###

# Copy dataset and rename forest column to match test dataset for forest_water_bare
HLS_forest_water_bare_10 <- HLS_forest 
colnames(HLS_forest_water_bare_10)[1] <- "class"

predict_RF_HLS_forest_water_bare_10 <- predict(RF_forest_water_bare_10, newdata=HLS_forest_water_bare, type="class") 
table(predict_RF_HLS_forest_water_bare_10)

# covert to dataframe & rename column 
forest_water_bare_raster_dat_10 <- as_data_frame(predict_RF_HLS_forest_water_bare_10)
colnames(forest_water_bare_raster_dat_10)[1] <- "class"

# Specify the mapping from factor levels to numeric values
factor_to_numeric <- c("forest" = 1, "water" = 2, "bare" = 3, "non-forest" = 0)

# Convert the factor column to a numeric column using the specified mapping
forest_water_bare_raster_dat_10 <- forest_water_bare_raster_dat_10 %>%
  mutate(class = as.numeric(factor_to_numeric[as.character(class)]))

# convert NA to -1 
forest_water_bare_raster_dat_10$class <- if_else(is.na(forest_water_bare_raster_dat_10$class), 
                                                 -1, 
                                                 forest_water_bare_raster_dat_10$class)


# 1 = forest 
# 2 = water
# 3 = bare 
# 0 = nonforest 
# -1 = no data value 



# convert to matrix to export as raster 
forest_water_bare_raster_10 <- as.matrix(forest_water_bare_raster_dat_10)

# Export to raster
forest_water_bare_raster_10 <- raster(forest_water_bare_raster_10)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
extent(forest_water_bare_raster_10) <- extent(rf_stack)
crs(forest_water_bare_raster_10) <- crs(rf_stack)
res(forest_water_bare_raster_10) <- res(rf_stack)


# view raster 
plot(forest_water_bare_raster_10, main = "Three land use classes (10000 trees)")

# Save output raster as geoTIFF
writeRaster(forest_water_bare_raster_10, filename = "forest_water_bare_raster_10_rf_stack.tif", format = "GTiff")



##### PREDICT ALL LAND USE TYPES #####

# 500 trees
# Copy dataset and rename forest column to match test dataset for forest_water_bare
HLS_all_label_data <- HLS_forest 
colnames(HLS_all_label_data)[1] <- "class"

predict_RF_HLS_all_label_data <- predict(RF_all_label_data, newdata=HLS_all_label_data, type="class") 
table(predict_RF_HLS_all_label_data)

# covert to dataframe & rename column 
HLS_all_label_data_dat <- as.data.frame(predict_RF_HLS_all_label_data)
colnames(HLS_all_label_data_dat)[1] <- "class"

# convert columns to numeric 
# Specify the mapping from factor levels to numeric values
factor_to_numeric_LU <- c("forest" = 1, "water" = 2, "bare" = 3, "agriculture" = 4, "built" = 5, "oil_palm_plantation" = 6,"non-forest" = 0)

# Convert the factor column to a numeric column using the specified mapping
HLS_all_label_data_dat <- HLS_all_label_data_dat %>%
  mutate(class = as.numeric(factor_to_numeric_LU[as.character(class)]))

HLS_all_label_data_dat$class <- if_else(is.na(HLS_all_label_data_dat$class), 
                                -1, 
                                HLS_all_label_data_dat$class)




# 1 = forest 
# 2 = water
# 3 = bare 
# 4 = agriculture
# 5 = built
# 6 = oil_palm_plantation


# -1 = no data value 



# convert to matrix to export as raster 
HLS_all_label_data_dat_raster <- as.matrix(HLS_all_label_data_dat)



# Export to raster
HLS_all_label_data_dat_raster <- raster(HLS_all_label_data_dat_raster)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
extent(HLS_all_label_data_dat_raster) <- extent(HLS)
crs(HLS_all_label_data_dat_raster) <- crs(HLS)
res(HLS_all_label_data_dat_raster) <- res(HLS)

# view raster 
plot(HLS_all_label_data_dat_raster, main = "All LU Classes (500 trees)")

# Save output raster as geoTIFF
writeRaster(HLS_all_label_data_dat_raster, filename = "HLS_all_label_data_dat_raster_rf_stack.tif", format = "GTiff")

all_label_data_Test <- all_label_data

all_label_data_Test$class <- as.factor(all_label_data_Test$class)


# 1000 trees 

# Copy dataset and rename forest column to match test dataset for forest_water_bare
HLS_all_label_data_1 <- HLS_forest 
colnames(HLS_all_label_data_1)[1] <- "class"

predict_RF_HLS_all_label_data_1 <- predict(RF_all_label_data_1, newdata=HLS_all_label_data, type="class") 
table(predict_RF_HLS_all_label_data_1)

# covert to dataframe & rename column 
HLS_all_label_data_dat_1 <- as_data_frame(predict_RF_HLS_all_label_data_1)
colnames(HLS_all_label_data_dat_1)[1] <- "class"

# convert columns to numeric 
# Specify the mapping from factor levels to numeric values
factor_to_numeric_LU <- c("forest" = 1, "water" = 2, "bare" = 3, "agriculture" = 4, "built" = 5, "oil_palm_plantation" = 6,"non-forest" = 0)

# Convert the factor column to a numeric column using the specified mapping
HLS_all_label_data_dat_1 <- HLS_all_label_data_dat_1 %>%
  mutate(class = as.numeric(factor_to_numeric_LU[as.character(class)]))

HLS_all_label_data_dat_1$class <- if_else(is.na(HLS_all_label_data_dat_1$class), 
                                  -1, 
                                  HLS_all_label_data_dat_1$class)



# 1 = forest 
# 2 = water
# 3 = bare 
# 4 = agriculture
# 5 = built
# 6 = oil_palm_plantation


# -1 = no data value 



# convert to matrix to export as raster 
HLS_all_label_data_dat_1_raster <- as.matrix(HLS_all_label_data_dat_1)

# Export to raster
HLS_all_label_data_dat_1_raster <- raster(HLS_all_label_data_dat_1_raster)


# Set the extent, coordinate reference system and resolution to match original HLS raster 
extent(HLS_all_label_data_dat_1_raster) <- extent(HLS)
crs(HLS_all_label_data_dat_1_raster) <- crs(HLS)
res(HLS_all_label_data_dat_1_raster) <- res(HLS)

# view raster 
plot(HLS_all_label_data_dat_1_raster, main = "All LU Classes (1000 trees)")

# Save output raster as geoTIFF
writeRaster(HLS_all_label_data_dat_1_raster, filename = "all_label_data_raster_1_rf_stack.tif", format = "GTiff")






