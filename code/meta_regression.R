#multiple meta regresssion 
library(PerformanceAnalytics)
library(metafor)
#check for correlation between variables to see check for multicollinearity
  #if correlation occurs (>=.08), you can remove redundant variables
  
  MVRegressionData[,c("reputation", "quality", "pubyear")] %>% cor()
  library(PerformanceAnalytics)
  
  MVRegressionData[,c("reputation", "quality", "pubyear")] %>% 
    chart.Correlation()

#use step-wise fitting process or multi-model inference to select variables
  multimodel.inference(TE = "yi", 
                       seTE = "sei",
                       data = MVRegressionData,
                       predictors = c("pubyear", "quality", 
                                      "reputation", "continent"),
                       interaction = FALSE)
  
  # or use anova to compare between full and reduced models
  levels(MVRegressionData$continent) = c("Europe", "North America")
  
  qual <- rma(yi = yi,
              sei = sei,
              data = MVRegressionData,
              method = "ML",
              mods = ~ quality,
              test = "knha")
  
  m.qual
  
  ##steps
  #verify all variables are complete 
  #change words to numbers, then add factor level labels
  #run multimodel inference to test which variables should stay
  #plot relationships of interest 
  #clean for Samantha
  #what should be done with feedback and ROI variables