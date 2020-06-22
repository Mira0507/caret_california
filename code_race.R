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


# sc_s: subsetted data for race 
sc_r <- subset(sc, 
               Geography != "CALIFORNIA" & Strata_Name != "Total" & Strata == "Race/Ethnicity") 




####################################### Modeling #########################################


fmr <- as.formula(Age_Adjusted_Rate ~ Strata_Name + Geography)

set.seed(711)
myFoldr <- createFolds(sc_r$Age_Adjusted_Rate, k = 10)

summary(sc_r$Age_Adjusted_Rate)
summary(sc_r$Age_Adjusted_Rate[myFoldr$Fold01])
summary(sc_r$Age_Adjusted_Rate[myFoldr$Fold02])

myControl <- trainControl(method = "cv",
                          number = 10,
                          savePredictions = TRUE,
                          verboseIter = TRUE,
                          index = myFoldr)

grGrid <- expand.grid(alpha = 0:1,
                      lambda = seq(0.001, 2, length = 100))

rrGrid <- expand.grid(mtry = 5:15,
                      splitrule = c("variance",
                                    "extratrees"),
                      min.node.size = 8)

grModel <- train(fmr,
                sc_r,
                method = "glmnet",
                tuneGrid = grGrid,
                trControl = myControl)


set.seed(680)
rrModel <- train(fmr,
                sc_r,
                method = "ranger",
                tuneLength = 10,
                tuneGrid = rrGrid,
                trControl = myControl)

plot(grModel, main = "glmnet")
plot(rrModel, main = "random forest")


####################################### Comparing Models #########################################


AllModels <- list(glmnet_sex = gModel,
               RandomForest_sex = rModel,
               glmnet_race = grModel,
               RandomForest_race = rrModel)

# rsp: resamples object 
rsp <- resamples(AllModels)

dotplot(rsp, main = "Evaluation: glmnet vs Random Forests")

xyplot(rsp, metric = "RMSE", col = "red")
xyplot(rsp, metric = "Rsquared")
xyplot(rsp, metric = "MAE")

# rsp_tb: a table for extracted results of metrics 
rsp_tb <- rsp$values

# data cleaning for plotting
new_name <- colnames(rsp_tb) %>%
        str_replace("~", "_") %>%
        str_replace("Resample", "Fold")

names(rsp_tb) <- new_name

rsp_tb_RMSE <- gather(rsp_tb,
                      RMSE_Model,
                      Value,
                      c(glmnet_sex_RMSE, 
                        RandomForest_sex_RMSE, 
                        glmnet_race_RMSE,
                        RandomForest_race_RMSE))

rsp_tb_MAE <- gather(rsp_tb,
                     MAE_Model,
                     Value,
                     c(glmnet_sex_MAE,
                       RandomForest_sex_MAE,
                       glmnet_race_MAE,
                       RandomForest_race_MAE))

rsp_tb_Rsquared <- gather(rsp_tb,
                          Rsquared_Model,
                          Value,
                          c(glmnet_sex_Rsquared, 
                            RandomForest_sex_Rsquared,
                            glmnet_race_Rsquared,
                            RandomForest_race_Rsquared))


# plots
compare_plot <- function(df, Model, Value, tit) {
        ggplot(df,
               aes(x = Model,
                   y = Value,
                   fill = Model)) + 
                geom_boxplot() +
                theme_bw() +
                xlab("Model") +
                ggtitle(tit) + 
                theme(axis.text.x = element_blank())
}



rmse_plot <- compare_plot(rsp_tb_RMSE, 
                         rsp_tb_RMSE$RMSE_Model,
                         rsp_tb_RMSE$Value,
                         "RMSE")



mae_plot <- compare_plot(rsp_tb_MAE, 
                         rsp_tb_MAE$MAE_Model,
                         rsp_tb_MAE$Value,
                          "MAE")

sqrd_plot <- compare_plot(rsp_tb_Rsquared, 
                          rsp_tb_Rsquared$Rsquared_Model,
                          rsp_tb_Rsquared$Value,
                          "R Squared")

grid.arrange(rmse_plot,
             mae_plot,
             sqrd_plot,
             ncol = 1)


####################################### Prediction #########################################

# extracting predicted values and residuals
sc_r <- sc_r %>% 
        mutate(Gpred = predict(grModel, sc_r),
               Rpred = predict(rrModel, sc_r),
               Gresid = Gpred - Age_Adjusted_Rate,
               Rresid = Rpred - Age_Adjusted_Rate)

# sc_rp: prediction vs Age_Adjusted_Rate plot
sc_rp <- sc_r %>% 
        gather(Model, Predicted_Rate, c(Gpred, Rpred)) %>%
        ggplot(aes(x = Predicted_Rate, 
                   y = Age_Adjusted_Rate, 
                   color = Model)) + 
        geom_point(alpha = 0.3) + 
        geom_smooth(method = "lm", se = F) + 
        theme_bw() + 
        ggtitle("Actual Rate vs Predicted Rate (Race)") + 
        xlab("Predicted Age-adjusted Rate (per 1,000 People)") + 
        ylab("Actual Age-adjusted Rate (per 1,000 People)")

cor(sc_r$Gpred, sc_r$Age_Adjusted_Rate)
cor(sc_r$Rpred, sc_r$Age_Adjusted_Rate)


grid.arrange(sc_sp, sc_rp, ncol = 1)

# sc_rr: a gathered table for plotting residuals

sc_rrg <- resid_fn(sc_r,
                   sc_r$Gpred,
                   sc_r$Gresid,
                   "#FF9999",
                   "Residuals in glmnet Model (Race)")

sc_rrr <- resid_fn(sc_r,
                   sc_r$Rpred,
                   sc_r$Rresid,
                   "#009933",
                   "Residuals in Random Forests Model (Race)")

grid.arrange(sc_srg, sc_srr,
             sc_rrg, sc_rrr, nrow = 2)


