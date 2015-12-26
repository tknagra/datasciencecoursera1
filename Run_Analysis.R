table1 <- read.table("X_train.txt")
table2 <- read.table("X_test.txt")
X <- rbind(table1, table2)

sub_table1 <- read.table("subject_train.txt")
sub_table2 <- read.table("subject_test.txt")
Sub_mer <- rbind(sub_table1, sub_table2)

y_table1 <- read.table("y_train.txt")
y_table2 <- read.table("y_test.txt")
Y <- rbind(y_table1, y_table2)

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

names(S) <- "subject"
cleaned <- cbind(Sub_mer, Y, X)
write.table(cleaned, "merged_clean_data.txt")

uniqueSubjects = unique(Sub_mer)[,1]
numSubjects = length(unique(Sub_mer)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = activities[a, 2]
        tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "data_set_with_the_averages.txt")
