#Libraries
library(tidyverse)
library(janitor)

#Read All Text Files
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activity", "activity_label"))
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("Code", "feature"))

test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature, check.names = FALSE)
test_lab <- read.table("UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")

train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature, check.names = FALSE)
train_lab <- read.table("UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")

#Combine Test Data
test_merge <- bind_cols(test_subject, test_lab, test) %>% 
   mutate(set = "test") %>%    
   select(subject_id = V1...1, activity = V1...2, set, everything())
      

#Combine Train Data
train_merge <- bind_cols(train_subject, train_lab, train) %>% 
   mutate(set = "train") %>%    
   select(subject_id = V1...1, activity = V1...2, set, everything())
      
#Combine Test and Train.. 
#Select variables with MEAN and STD,
data.1 <- bind_rows(test_merge, train_merge) %>% 
      left_join(., activity_labels.2) %>% 
      select(subject_id, activity_label, contains("mean()"), contains("std()"))

#Clean Names
clean_names <- names(data.1)

clean_names <- str_replace(clean_names, '^t', 'Time domain signal: ')
clean_names <- str_replace(clean_names, '^f', 'Frequency domain signal: ')
clean_names <- str_replace(clean_names, '-', ', ')
clean_names <- str_replace(clean_names, 'mean\\(\\)', ' mean value ')
clean_names <- str_replace(clean_names, 'std\\(\\)', ' standard deviation ')
clean_names <- str_replace(clean_names, '-X', 'in X direction')
clean_names <- str_replace(clean_names, '-Y', 'in Y direction')
clean_names <- str_replace(clean_names, '-Z', 'in Z direction')
clean_names <- str_replace(clean_names, 'AccJerk', ' acceleration jerk')
clean_names <- str_replace(clean_names, 'Acc', ' acceleration')
clean_names <- str_replace(clean_names, 'GyroJerk', ' angular velocity jerk')
clean_names <- str_replace(clean_names, 'Gyro', ' angular velocity')
clean_names <- str_replace(clean_names, 'Mag', ' magnitude')
names(data.1) <- clean_names

#Create Tidy Data Set with mean values for all variables
tidy_set <- data.1 %>% 
   group_by(subject_id, activity_label) %>% 
   summarise_all(function(x) mean(x))

#Output: Tidy Set
write.table(tidy_set,file = 'tidy_set.txt',row.names = F)
