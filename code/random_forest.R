# random forrest analysis

#load metaforest
library(metaforest)

# create dataset with no NAs and all variables that will be included
nf_first_forest <- filter(nf_first_short, !is.na(SMD_first_mean_last))

to_exclude_all <- c(2,3,4,5,9,13,16,17,19,20,21:28, 30, 37, 38,40,42, 45:76)
to_exclude_eeg <- c(14,31,32,33,34, 35, 36, 37,41)

nf_first_forest_all <- nf_first_forest[,c(-to_exclude_all, -to_exclude_eeg)]
nf_first_forest_eeg <- nf_first_forest[,-to_exclude_all]

nf_first_forest_all[14:15, 11] <- 0

nf_first_forest_eeg <- filter(nf_first_forest_eeg, device == 0)



#set seed so analyses can be replicated 
set.seed(62)

check_conv <-MetaForest(SMD_first_mean_last~.,
                        data =nf_first_forest_all,
                        vi = "SMD_first_var_last",
                        #study ="author_n",
                        whichweights ="random",
                        num.trees =20000)
# Plot convergence trajectory
plot(check_conv)

mf_rep <-MetaForest(SMD_first_mean_last~.,
                    data =nf_first_forest_all,
                    vi = "SMD_first_var_last",
                    whichweights ="random",
                    num.trees =4500)
# Recursive preselection
preselected <-preselect(mf_rep,
                        replications =100,
                        algorithm ="recursive")
# Plot results
plot(preselected)
VarImpPlot(mf_rep)

ordered_vars <- names(mf_rep$forest$variable.importance)[
  order(mf_rep$forest$variable.importance, decreasing = TRUE)]

PartialDependence(mf_rep, vars = ordered_vars,
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

