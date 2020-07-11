####################
# Minor Assignment #
####################

# attach preloaded dataset mtcars
attach(mtcars)
View(mtcars)

# (1) Function to calculate rmse
rmse <- function(p,t) {
  e <- sqrt(mean(p-t)^2)
  return(e)
}

# linear regression model on mtcars data, 
# outcome variable is mpg and all other are predictor variables
lm <- lm(mpg~., mtcars)
# predicted value of mpg
y_hat <- predict(lm, mtcars)
# calling rmse function
rmse(mtcars$mpg, y_hat)

## (2) Function to calculate coefficient of variation

CV <- function(x){
  cvx <- (sd(x)/mean(x))*100
  return(cvx)
}
# function calling
CV(mtcars$mpg)

### (3) Function of standard deviation
std_dev <- function(x){
  mean_x <- mean(x)
  stdev <- sqrt(mean((x - mean(x))^2))
  return(stdev)
}
# call function
std_dev(mtcars$mpg)
# call R in built function to see the difference 
# between values computed by our function and in bulit function
sd(mtcars$mpg)

# Another function of standard deviation
standard_dev <- function(x){
  mean_x <- mean(x)
  diff <- (x - mean_x)
  sq_diff <- diff ^ 2
  mean_of_sq_diff <- mean(sq_diff)
  Stnd_Dev_mpg <- sqrt(mean_of_sq_diff)
  return(Stnd_Dev_mpg)
}
standard_dev(mtcars$mpg)

####################
# Major Assignment #
####################


# read as character
income_employment <-read.csv("income_employment_analysis.csv",stringsAsFactors=FALSE)

in_em_full_info <- read.csv("~/trunk/kumud/COURSES/Intermediate Analytics/week 4/income_employment_full_information.csv", stringsAsFactors=FALSE)


# Function to convert character into numeric variable and
# removing special characters by gsub function
make_numeric <- function(variable) {
  clean <- variable
  clean <- gsub(",", "", clean)
  clean <- gsub("*", "", clean)
  clean <- gsub("#", "", clean)
  return(as.numeric(clean))
}

# check the structure of data
str(in_em_full_info)

# summary
summary(in_em_full_info)

# apply make_numeric function 
in_em_full_info[c(7:14)] <- lapply(in_em_full_info[c(7:14)], make_numeric)
# check and deletion of missing
in_em_full_info <- na.omit(in_em_full_info)
# standardize variables
in_em_full_info[, c(7:14)] <- scale(in_em_full_info[, c(7:14)])

###### k-means Clustering ######

kmeans_distance <- rep(NA, 90) # Determine number of clusters
for (i in 10:100) {
  kmeans_distance[i-10] <- sum(kmeans(in_em_full_info[, c(7:14)], 
                                     centers=i, iter.max = 30)$withinss)
}
plot(10:100, kmeans_distance[10:100], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# fitting model to 30 cluster solution
fit_kmeans <- kmeans(in_em_full_info[, c(7:14)], 30, iter.max = 50) 

# append cluster assignment in the dataframe
in_em_full_info$Cluster <- fit_kmeans$cluster 

map_cluster_to_category <- function(in_em_full_info, category_column_id){
  unique_cluster_ids <- unique(in_em_full_info$Cluster)
  unique_categories <- unique(in_em_full_info[, category_column_id])
  mapping_df <- data.frame(matrix(0, ncol= length(unique_categories), 
                                  nrow = length(unique_cluster_ids)))
  colnames(mapping_df) <- unique_categories
  for (row in 1:nrow(in_em_full_info)) {
    cluster_id <- in_em_full_info[row,]$Cluster
    name_id  <- in_em_full_info[row, category_column_id]
    mapping_df[cluster_id, name_id] <- mapping_df[cluster_id, name_id]+1
  }
  return (mapping_df)
}

cluster_to_area_name_map <- map_cluster_to_category(in_em_full_info, 3)
heatmap(as.matrix(cluster_to_area_name_map))

# classification 

library(glmnet)

num_observations = nrow(in_em_full_info)
proportion_split = 0.80 # using 80% data for training and 20% for test 
train_index = sample(1:num_observations, round(num_observations * proportion_split))
y = as.matrix(in_em_full_info$Short_name)
X_train = as.matrix(in_em_full_info[train_index, c(7:14)])
X_test = as.matrix(in_em_full_info[-train_index, c(7:14)])

compute_accuracy <- function(true_label, pred_label){
  return ((sum(true_label==pred_label)) / length(pred_label))
}

logistic_model = glmnet(X_train,  y[train_index], lambda=0,  family="multinomial")
y_predicted_logistic = predict(logistic_model, X_test, type = "class")
compute_accuracy(in_em_full_info[-train_index, ]$Short_name,y_predicted_logistic)


data("iris")
head(iris)
attach(iris)
n_obs <- nrow(iris)
prop_split <- .90
training_index <- sample(1:n_obs, round(n_obs * prop_split))
y_true <- as.matrix(iris$Species)
x_train <- as.matrix(iris[training_index, c(1:4)])
x_test <- as.matrix(iris[-training_index, c(1:4)])

glm_fit <- glmnet(x_train , y_true[training_index], lambda = 0, family = "multinomial")
y_hat <- predict(glm_fit, x_test, type = "class")
compute_accuracy(iris[-training_index, ]$Species, y_hat)