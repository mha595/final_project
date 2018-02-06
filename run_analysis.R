# Data cleaning, final project

# reading data..

# traning data set 
subject_train <- read.table("train/subject_train.txt")
X_train       <- read.table("train/X_train.txt")
Y_train <-   read.table("train/y_train.txt")

# reading test data..
subject_test <- read.table("test/subject_test.txt")
X_test       <- read.table("test/X_test.txt")
Y_test <-   read.table("test/y_test.txt")
# 
# Task-1 
# #combining 
# 
subject_comb <- rbind(subject_train,subject_test)     # subject id
X_comb       <- rbind(X_train,X_test)                 # measurements
Y_comb <-   rbind(Y_train,Y_test)                     # activity type


# extracting only the activites with mean and std in their names
# Task -2
activity_names <- read.table("features.txt")
meanstd_data <- X_comb[,grep("mean|std", activity_names$V2)]



# descriptive naming
# Taske 4
meanandstd_activities <-  activity_names[grep("mean|std", activity_names$V2),2]
names(meanstd_data) <-  meanandstd_activities

# creating final dataset
f_data <- data.frame(id = as.vector(subject_comb)[[1]], activity = as.vector(Y_comb)[[1]] )
f_data <- cbind(f_data, meanstd_data)




# changing the activity names to descriptive  
# Task -3
aid <- read.table("activity_labels.txt")
names(aid) <- c("activity", "name")
f_data$activity <- aid[match(f_data[['activity']], aid[['activity']] ), 'name']

# sorting by id 
f_data <- f_data[order(f_data$id),]


# averging measurments by activities by subject
split_data <- split(f_data, f_data$activity)
split_mean <- lapply(split_data, function(x) aggregate(x[,3:81],by=list(x$id), mean))

# adding the activity column to each data frame
b <- summary( split_data)
nms <- names(b[,1])

for (a in 1:6){
        split_mean[[a]]$activity <- nms[a]
        
}



f_means <- do.call("rbind", split_mean)    # final data

write.table(f_means, file = "final_data.txt", row.names = FALSE)

