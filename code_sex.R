library(ggplot2)
library(readxl)
library(data.table)
library(tidyverse)

g <- glimpse
h <- head
s <- summary


####################################### Importing Data #########################################

sc <- fread("suicide-lghc-indicator-21.csv")
dic <- read_excel("suicide-lghc-21-data-dictionary.xlsx")


####################################### Data Cleaning #########################################


names(sc) <- c("Indicator",
               "Geography",
               "Year",
               "Strata",
               "Strata_Name",
               "Numerator",
               "Denominator",
               "Rate",
               "Age_Adjusted_Rate")

# sc_s: subsetted data for sex 
sc_s <- subset(sc, 
               Geography != "CALIFORNIA" & Strata_Name != "Total" & Strata == "Sex") %>%
        mutate(Strata_Name = factor(Strata_Name, levels = c("Female", "Male")))



raw_dst_plot <- function(df, tit) {
        ggplot(df, 
               aes(x = Age_Adjusted_Rate,
                   fill = Strata_Name,
                   color = Strata_Name)) +
                geom_density(alpha = 0.5) + 
                theme_bw() + 
                xlab("Age-adjusted Rate (per 1,000 People)") +
                ylab("Density") + 
                ggtitle(tit)
}


raw_dst_plot(sc_s, "Suicide by Sex")
####################################### Modeling #########################################

library(caret)

fm <- as.formula(Age_Adjusted_Rate ~ Strata_Name + Geography) 

set.seed(1275)
myFolds <- createFolds(sc_s$Age_Adjusted_Rate, k = 10)

summary(sc_s$Age_Adjusted_Rate)
summary(sc_s$Age_Adjusted_Rate[myFolds$Fold01])
summary(sc_s$Age_Adjusted_Rate[myFolds$Fold02])

myControl <- trainControl(method = "cv",
                          number = 10,
                          savePredictions = TRUE,
                          verboseIter = TRUE,
                          index = myFolds)

gGrid <- expand.grid(alpha = 0:1,
                     lambda = seq(0.0001, 0.2, length = 100))


rGrid <- expand.grid(mtry = 10:20,
                     splitrule = c("variance",
                                   "extratrees"),
                     min.node.size = 6:8)


gModel <- train(fm,
                sc_s,
                method = "glmnet",
                tuneGrid = gGrid, 
                trControl = myControl)



set.seed(680)
rModel <- train(fm,
                sc_s,
                method = "ranger",
                tuneLength = 10,
                tuneGrid = rGrid, 
                trControl = myControl)

plot(gModel, main = "glmnet")
plot(rModel, main = "random forest")



####################################### Comparing Models #########################################

models <- list(glmnet = gModel,
               RandomForest = rModel)

# rsam: resamples object 
rsam <- resamples(models)

dotplot(rsam, main = "Evaluation: glmnet vs Random Forests")

xyplot(rsam, metric = "RMSE", col = "red")
xyplot(rsam, metric = "Rsquared")
xyplot(rsam, metric = "MAE")

# rsam_tb: a table for evaluation metrics in every fold
rsam_tb <- rsam$values

# data cleaning for plotting
new_name <- colnames(rsam_tb) %>%
        str_replace("~", "_") %>%
        str_replace("Resample", "Fold")

names(rsam_tb) <- new_name

rsam_tb1 <- rsam_tb %>%
        mutate(SD = sd(sc_s$Age_Adjusted_Rate)) %>%
        gather(RMSE_Model, RMSE, c(glmnet_RMSE, RandomForest_RMSE, SD))

rsam_tb2 <- rsam_tb %>% 
        gather(MAE_Model, MAE, c(glmnet_MAE, RandomForest_MAE))

rsam_tb3 <- rsam_tb %>%
        gather(Rsquared_Model, Rsquared, c(glmnet_Rsquared, RandomForest_Rsquared))





####################################### Prediction #########################################

# extracting predicted values and residuals
sc_s <- sc_s %>% 
        mutate(Gpred = predict(gModel, sc_s),
               Rpred = predict(rModel, sc_s),
               Gresid = Gpred - Age_Adjusted_Rate,
               Rresid = Rpred - Age_Adjusted_Rate)

# sc_sp: prediction vs Age_Adjusted_Rate plot

sc_sp <- sc_s %>% 
        gather(Model, Predicted_Rate, c(Gpred, Rpred)) %>%
        ggplot(aes(x = Predicted_Rate, 
                   y = Age_Adjusted_Rate, 
                   color = Model)) + 
        geom_point(alpha = 0.3) + 
        geom_smooth(method = "lm", se = F) + 
        theme_bw() + 
        ggtitle("Actual Rate vs Predicted Rate (Sex)") + 
        xlab("Predicted Age-adjusted Rate (per 1,000 People)") + 
        ylab("Actual Age-adjusted Rate (per 1,000 People)")

cor(sc_s$Gpred, sc_s$Age_Adjusted_Rate)
cor(sc_s$Rpred, sc_s$Age_Adjusted_Rate)

# sc_sr: a gathered table for plotting residuals

resid_fn <- function(df, xcol, ycol, c, tit) {
        ggplot(df, 
               aes(x = xcol,
                   y = ycol)) + 
                geom_point(alpha = 0.5, color = c) + 
                geom_smooth(method = "lm", se = F) + 
                theme_bw() + 
                xlab("Prediction") + 
                ylab("Residual") + 
                ggtitle(tit) 
}

sc_srg <- resid_fn(sc_s,
                   sc_s$Gpred,
                   sc_s$Gresid,
                   "#FF9999",
                   "Residuals in glmnet Model (Sex)")

sc_srr <- resid_fn(sc_s,
                   sc_s$Rpred,
                   sc_s$Rresid,
                   "#009933",
                   "Residuals in Random Forests Model (Sex)")



grid.arrange(sc_srg, sc_srr, nrow = 1)


