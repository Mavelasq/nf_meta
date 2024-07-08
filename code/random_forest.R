# random forrest analysis

#load metaforest
library(metaforest)
library(caret)

# create dataset with no NAs and all variables that will be included
nf_first_forest <- filter(nf_first_short, !is.na(SMD_first_mean_last))

#change labels in variables
nf_first_forest$device <- ifelse(nf_first_forest$device == 0, "EEG", "fMRI")
nf_first_forest$blinding <- ifelse(nf_first_forest$blinding == 0, "No", "Yes")
nf_first_forest$instruction_yn <- ifelse(nf_first_forest$instruction_yn == 0, "No", "Yes")
nf_first_forest$rehearsal <- ifelse(nf_first_forest$rehearsal == 0, "No", "Yes")
nf_first_forest$func.loc <- ifelse(nf_first_forest$func.loc == 0, "No", "Yes")
nf_first_forest <- nf_first_forest %>%
  mutate(roi_cat_code = case_when(
    roi_cat_code == 0 ~ "Central",
    roi_cat_code == 1 ~ "Frontal",
    roi_cat_code == 2 ~ "Posterior/Occipital",
    TRUE ~ as.character(roi_cat_code)  # keep original value if no match
  ))


to_exclude_all <- c(1,2,3,4,5,9,10,13,15,16,17,19,20,21:28, 30, 37, 38,40,42, 45:76)
to_exclude_eeg <- c(14,31,32,33,34, 35, 36, 37,41)

nf_first_forest_all <- nf_first_forest[,c(-to_exclude_all, -to_exclude_eeg)]
nf_first_forest_eeg <- nf_first_forest[,-to_exclude_all]

nf_first_forest_all[14:15, 8] <- "Central"

nf_first_forest_eeg <- filter(nf_first_forest_eeg, device == 0)

#log transform
nf_first_forest_all$training.dur.min <- log(nf_first_forest_all$training.dur.min)
#set seed so analyses can be replicated 
set.seed(62)

nf_first_forest_all$id_exp <- as.numeric(1:nrow(nf_first_forest_all))

check_conv <-MetaForest(SMD_first_mean_last~.,
                        data =nf_first_forest_all,
                        vi = "SMD_first_var_last",
                        study ="id_exp",
                        whichweights ="random",
                        num.trees =20000)
# Plot convergence trajectory
plot(check_conv)

mf_rep <-MetaForest(SMD_first_mean_last~.,
                    data =nf_first_forest_all,
                    vi = "SMD_first_var_last",
                    study ="id_exp",
                    whichweights ="random",
                    num.trees =6000)
# Recursive preselection
preselected <-preselect(mf_rep,
                        replications =100,
                        algorithm ="recursive")
# Plot results
plot(preselected)
VarImpPlot(mf_rep)

grouped_cv <- trainControl(method = "cv",
                           index = groupKFold(nf_first_forest_all$id_exp, k = 10))

tuning_grid <- expand.grid(whichweights = c("random", "fixed", "unif"),
                           mtry = 2:6,
                           min.node.size = 2:6)


#retained vars
first_retained <- nf_first_forest_all[, c(1,2,4,6,7,8,9,10)]

X <- nf_first_forest_all[,c(1:8, 10,11)]

colnames(X)[9] <- "vi"

mf_cv <- train(y = nf_first_forest_all$SMD_first_mean_last,
               x = X,
               study = "id_exp", # Name of the clustering variable
               method = ModelInfo_mf(),
               trControl = grouped_cv,
               tuneGrid = tuning_grid,
               num.trees = 6000)

r2_cv <- mf_cv$results$Rsquared[which.min(mf_cv$results$RMSE)]

# Extract final model
final <- mf_cv$finalModel
# Extract R^2_oob from the final model
r2_oob <- final$forest$r.squared
# Plot convergence
plot(final)

# Plot variable importance
VarImpPlot(final)
# Sort the variable names by importance
ordered_vars <- names(final$forest$variable.importance)[
  order(final$forest$variable.importance, decreasing = TRUE)]

# Plot partial dependence
PartialDependence(final, vars = ordered_vars,
                  rawdata = TRUE, pi = .95)




m.gen_forest <- metagen(TE = SMD_first_mean_last,
                 seTE = SMD_first_var_last,
                 studlab = author_n,
                 data = nf_first_forest_all,
                 sm = "Hedge's g",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK",
                 prediction = TRUE,
                 title = "Neurofeedback")
summary(m.gen_forest)

sub.feedback <- update(m.gen_forest, 
                     subgroup = feedback_cat, 
                     tau.common = FALSE)
summary(sub.feedback)


sub.feedback
pdf(file='results/device subgroup.pdf', width=10, height=15)
png(filename='results/feedback subgroup.png', width=1450, height=1600, res=150)
forest(sub.feedback, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm', 
       label.e = "EEG", label.c = "fMRI")
dev.off()



