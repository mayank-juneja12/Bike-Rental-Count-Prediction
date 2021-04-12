####Setting Working directory
# setwd("D:\\Data Science\\edwisor\\Project Stage\\Project3")

###Load the dataset#33333333333
dset <- read.csv('day.csv')

str(dset)
summary(dset)

library(dplyr)

####Dropping instant and dteday####
dset <- dset %>%
        select(-c('dteday', 'instant'))

######Setting relevant feature into factor####
for(i in names(dset)[1:7]){
        dset[[i]] = factor(dset[[i]])
} 


sum(is.na(dset))

######Getting numeric features data
numeric_index = sapply(dset, is.numeric)
numeric_data <- dset[, numeric_index]

#####Getting categorical features data
factor_index = sapply(dset, is.factor)
cat_data <- dset[, factor_index]


cnames <- colnames(numeric_data)

library(ggplot2)
library(grid)
library(gridExtra)

#######Plotting distributions of numeric features
dist <- function(var_name){
        ggplot(data = dset, aes_string(x = var_name)) +
                geom_density() + xlab(var_name)
}

dist_list = list()
dist_list = lapply(cnames, dist)

grid.arrange(grobs = dist_list)

#######Boxplots########
box_plot <- function(var_name){
        ggplot(data = dset, aes_string(y = var_name)) +
                geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                             outlier.size=1, notch=FALSE) +
                stat_boxplot(geom = 'errorbar', width = 0.5) + ylab(var_name)
}


plot_list = list()
plot_list = lapply(cnames, box_plot)


grid.arrange(grobs = plot_list)

# install.packages('GGally')
library(GGally)

########Scatter Plots for all pairs####### 
ggpairs(numeric_data)


g1 <- ggplot(data = dset, aes(x = casual, y = cnt)) +
        geom_point(aes(color = yr)) +
        geom_smooth(method = lm)

######Outlier Analysis######
out_cas = boxplot.stats(numeric_data$casual)$out
min(out_cas) ##2282

g1 + facet_grid(yr ~ mnth)


out_hum = boxplot.stats(numeric_data$hum)$out
out_wds = boxplot.stats(numeric_data$windspeed)$out

dset$hum[dset$hum %in% out_hum] <- NA
dset$windspeed[dset$windspeed %in% out_wds] <- NA

sum(is.na(dset))
summary(dset)

####Knn Inmputation########
library(DMwR)

dat <- dset

str(dat)
dat[123, 10] ##0.697083
dat[123, 10] = NA


set.seed(123)

data_knn = knnImputation(data = dat, k = 5)

dset <- data_knn
rm(dat)
rm(data_knn)

sum(is.na(dset))

#########Categorical Data Analysis##########3
#######Barplots
barplot <- function(var_name){
        ggplot(data = dset, aes_string(x = var_name, y = 'cnt')) +
                geom_bar(stat = "identity")
}

cat_names <- colnames(cat_data)

bar_list = list()
bar_list = lapply(cat_names, barplot)

g2 <- ggplot(data = dset, aes(x = mnth, y = cnt)) + geom_col()
g2 + facet_grid( ~ yr)
g2 + facet_grid( ~ season)

#######Creating a new variable 'mnth_fe' from 'mnth'#####
new_month <- function(mnth){
        mnth = as.numeric(as.character(mnth))
        mnth[mnth %in% c(1, 2)] = 1
        mnth[mnth %in% c(3, 4, 11, 12)] = 2
        mnth[mnth %in% c(5, 6, 7, 8, 9, 10)] = 3
        mth <- factor(mnth)
}

dset$mnth_fe = new_month(dset$mnth)

dset <- dset %>%
        select(-('mnth'))

dset$mnth_fe = factor(dset$mnth_fe, labels = c('Low', 'Medium', 'High'))

#########Getting numerical and categorical data######
numeric_data <- dset %>%
        select_if(is.numeric)

cat_data <- dset %>%
        select_if(is.factor)


#######Correlation Analysis#########
cor_mat <- cor(numeric_data)


library(corrplot)

####Correlation Plot#########33
cor_test <- corrplot(cor_mat, method = "color", type = "upper")

#####Checking variation inflation factor to remove correlated features#######
library(usdm)
vif(numeric_data)
vif(numeric_data[, -c(5:7)])
vif(numeric_data[, -c(2, 5:7)])


#####Dropping features#####3
dset_fs <- dset[, -c(8, 11, 12)]


########ANOVA test analysis#########

factor_data_aov <- data.frame(Count = dset$cnt, cat_data)
aov_data <- data.frame(Variable_Name = names(cat_data))
p_value <- {}

for(i in names(factor_data_aov)[-c(1)]){
        aov_test <- aov(Count ~ cat_data[[i]], data = factor_data_aov)
        p_value <- c(p_value, summary(aov_test)[[1]][1, 5])
}

aov_data$p_value <- p_value

aov_data$Result <- aov_data$p_value <= 0.05

rm(factor_data_aov)
rm(aov_data)
rm(aov_test)

######Dropping features#######33
dset_fs <- dset_fs[, -c(3, 4, 5)]

#####Transformin target variable 'cnt' by taking log
dset_fs$cnt_log = log(dset_fs$cnt)

#######Making dummy variables#########
library(caret)

dummy <- dummyVars("~.", data = dset_fs)
dummy_data <- data.frame(predict(dummy, newdata = dset_fs))
final_data <- data.frame(dummy_data)

#######Splitting data into train and test###########3
set.seed(123456)
train_index = createDataPartition(final_data$cnt, p = 0.8, list = FALSE)
train <- final_data[train_index,]
test <- final_data[-train_index,]

train_cnt <- train$cnt
test_cnt <- test$cnt

train = train[, -c(13)]
test = test[, -c(13)]

###############Supervised Machine Learning##########


###Linear Regression Model##########
set.seed(789)
lr_model <- lm(cnt_log ~ ., data = train)
pred_train <- predict(lr_model, train[, -c(16)])
pred_vd <- predict(lr_model, test[, -c(16)])

LR_error <- postResample(exp(pred_train), obs = train_cnt) ###RMSE:950.216, MAE:692.744, R2: 0.774
LR_error_vd <- postResample(exp(pred_vd), obs = test_cnt) ####RMSE:846.044, MAE:607.675, R2: 0.829
print(AIC(lr_model))  ###69.76856

########Random Forest Model########
library(randomForest)
set.seed(678)
rf_model <- randomForest(cnt_log ~ ., data = train, ntree = 500, importance = TRUE)
pred_train_rf <- predict(rf_model, train)
pred_vd_rf <- predict(rf_model, test)

RF_error <- postResample(exp(pred_train_rf), obs = train_cnt) ###RMSE:392.431, MAE:300.302, R2:0.961
RF_error_vd <- postResample(exp(pred_vd_rf), obs = test_cnt) ####RMSE:701.384, MAE:490.127, R2:0.883 

####Tuning model using RandomSearchCV########
set.seed(678)
best_rf <- tuneRF(train[, -c(16)], train$cnt_log, ntreeTry = 500, stepFactor = 1.5)
set.seed(678)
rf_model <- randomForest(cnt_log ~ ., data = train, ntree = 500, importance = TRUE, mtry = 7)
pred_train_rf <- predict(rf_model, train)
pred_vd_rf <- predict(rf_model, test)

RF_error <- postResample(exp(pred_train_rf), obs = train_cnt) ###RMSE:333.921, MAE:253.193, R2:0.971
RF_error_vd <- postResample(exp(pred_vd_rf), obs = test_cnt) ####RMSE:710.123, MAE:488.627, R2:0.879 

########XGBoost Model###########
set.seed(4567)
library(xgboost)

dtrain_label <- train$cnt_log
dtrain <- xgb.DMatrix(data = data.matrix(train[, -c(16)]), label = dtrain_label)
xg_model <- xgboost(dtrain, nrounds = 200, print_every_n = 10)

pred_train_xg <- predict(xg_model, data.matrix(train[, -c(16)]))
pred_vd_xg <- predict(xg_model, data.matrix(test[, -c(16)]))

XG_error <- postResample(exp(pred_train_xg), obs = train_cnt)####RMSE:9.51, MAE:6.18, R2:1
XG_error_vd <- postResample(exp(pred_vd_xg), obs = test_cnt)####RMSE:715.584, MAE:532.212, R2:0.876

###########Tuning Hyperparameters########

###Setting parameters for base model usind grid#### 
grid_default <- expand.grid(
        nrounds = 200,
        max_depth = 6,
        eta = 0.3,
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1
)

####Setting train control to state whether to use cross validation or training the model#####
train_control <- trainControl(
        method = 'none', ####Used for training
        verboseIter = FALSE,
        allowParallel = TRUE
)

#####Converting train and test into matrix form#######
input_x <- as.matrix(train[, -c(16)])
input_y <- train$cnt_log

test_x <- as.matrix(test[, -c(16)])
test_y <- test$cnt_log

#######Training the  base model##########
set.seed(4567)

xgb_base <- caret::train(
        x = input_x,
        y = input_y,
        trControl = train_control,
        tuneGrid = grid_default,
        method = 'xgbTree',
        verbose = TRUE
)

hy_pred_train_xg <- predict(xgb_base, input_x)
hy_pred_test_xg <- predict(xgb_base, test_x)
        
hy_XG_error <- postResample(exp(hy_pred_train_xg), obs = train_cnt)####RMSE:10.65, MAE:6.84, R2:1
hy_XG_error_vd <- postResample(exp(hy_pred_test_xg), obs = test_cnt)####RMSE:715.341, MAE:532.02, R2:0.876

###################Setting hyperparameters range to tune#############
nrounds = 1000

#######Deciding the learning rate, max_depth parameters#########
tune_grid <- expand.grid(
        nrounds = seq(from = 200, to = nrounds, by = 50),
        eta = c(0.025, 0.05, 0.1, 0.3),
        max_depth = c(2, 3, 4, 5, 6),
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1
)

tune_control <- trainControl(
        method = 'cv',
        number = 3,
        verboseIter = FALSE,
        allowParallel = TRUE
)


xgb_tune <- train(
        x = input_x,
        y = input_y,
        trControl = tune_control,
        tuneGrid = tune_grid,
        method = 'xgbTree',
        verbose = TRUE
)

tuneplot <- ggplot(xgb_tune)
tuneplot
xgb_tune$bestTune

########Setting the best parameters eta and max_depth and tune the min_child_weight########3
tune_grid <- expand.grid(
        nrounds = seq(from = 50, to = nrounds, by = 50),
        eta = xgb_tune$bestTune$eta,
        max_depth = xgb_tune$bestTune$max_depth,
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = c(1, 2, 3),
        subsample = 1
)

############Train the model to find best parameter using Grid Search CV##########
xgb_tune <- train(
        x = input_x,
        y = input_y,
        trControl = tune_control,
        tuneGrid = tune_grid,
        method = 'xgbTree',
        verbose = TRUE
)

tuneplot <- ggplot(xgb_tune)
tuneplot
xgb_tune$bestTune

##########Training the final model using best hyperparameters#######
final_grid <- expand.grid(
        nrounds = xgb_tune$bestTune$nrounds,
        eta = xgb_tune$bestTune$eta,
        max_depth = xgb_tune$bestTune$max_depth,
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = xgb_tune$bestTune$min_child_weight,
        subsample = 1
)

xgb_final <- caret::train(
        x = input_x,
        y = input_y,
        trControl = train_control,
        tuneGrid = final_grid,
        method = 'xgbTree',
        verbose = TRUE
        )

hy_pred_train_xg <- predict(xgb_final, input_x)
hy_pred_test_xg <- predict(xgb_final, test_x)

hy_XG_error <- postResample(exp(hy_pred_train_xg), obs = train_cnt)####RMSE:574.631, MAE:430.022, R2:0.912
hy_XG_error_vd <- postResample(exp(hy_pred_test_xg), obs = test_cnt)####RMSE:644.469, MAE:488.194, R2:0.899

 