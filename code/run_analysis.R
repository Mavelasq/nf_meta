library(meta)
library(metafor)

library(tidyverse)

#create author year variable
data_psc_final$AuthorYear <- as.character(paste(data_psc_final$author_n, data_psc_final$year, sep=" "))

data_psc_final$PSC_1    <- ifelse(!is.na(data_psc_final$pscBase_mean_last), data_psc_final$pscBase_mean_last, data_psc_final$pscFirst_mean_last)
data_psc_final$PSC_1_se <- ifelse(!is.na(data_psc_final$pscBase_var_last), data_psc_final$pscBase_var_last, data_psc_final$pscFirst_var_last)

data_psc_final$PSC_2    <- ifelse(!is.na(data_psc_final$pscBase_mean_post), data_psc_final$pscBase_mean_post, data_psc_final$pscFirst_mean_post)
data_psc_final$PSC_2_se <- ifelse(!is.na(data_psc_final$pscBase_var_post), data_psc_final$pscBase_var_post, data_psc_final$pscFirst_var_post)

data_psc_final$PSC      <- ifelse(!is.na(data_psc_final$PSC_2), data_psc_final$PSC_2, data_psc_final$PSC_1)
data_psc_final$PSC_se   <- ifelse(!is.na(data_psc_final$PSC_2_se), data_psc_final$PSC_2_se, data_psc_final$PSC_1_se)

#filter by available
pscData_f <- filter(data_psc_final, !is.na(PSC))


#overall meta-analysis####

overall<-metagen(PSC, PSC_se, method.tau="DL", studlab=AuthorYear, data=pscData_f)
overall
forest(overall)

pdf(file='results/nf_plot.pdf', width=10, height=12)
forest(overall, sortvar=author, fixed=FALSE, random=TRUE, lty.random=2, layout="meta", leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("PSC [95% CI]"), print.tau2=FALSE, label.left = "Negative PSC", 
       label.right = "Positive PSC", bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue")
dev.off()

#subgroup####
#device
pscData_f$device <- ifelse(pscData_f$device== "", 3, pscData_f$device)

sub1<-metagen(pscData_f$PSC, pscData_f$PSC_se,  subgroup=pscData_f$device, 
              method.tau="DL", studlab=pscData_f$AuthorYear, data=pscData_f)
sub1
pdf(file='results/device subgroup.pdf', width=10, height=15)
forest(sub1, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("PSC [95% CI]"), print.tau2=FALSE, label.left = "Negative PSC", 
       label.right = "Positive PSC", bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
dev.off()

pscData_f$instruction <- ifelse(is.na(pscData_f$instruction), 1, pscData_f$instruction)

sub2<-metagen(pscData_f$PSC, pscData_f$PSC_se,  subgroup=pscData_f$instruction, 
              method.tau="DL", studlab=pscData_f$AuthorYear, data=pscData_f)
sub2
pdf(file='results/instruction subgroup', width=10, height=115)
forest(sub2, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("PSC [95% CI]"), print.tau2=FALSE, label.left = "Negative PSC", 
       label.right = "Positive PSC", bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
dev.off()


pscData_f$motivation <- ifelse(is.na(pscData_f$motivation), 1, pscData_f$motivation)

sub3<-metagen(pscData_f$PSC, pscData_f$PSC_se,  subgroup=pscData_f$motivation, 
              method.tau="DL", studlab=pscData_f$AuthorYear, data=pscData_f)
sub3
pdf(file='results/motivation instruction.pdf', width=10, height=15)
forest(sub3, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("PSC [95% CI]"), print.tau2=FALSE, label.left = "Negative PSC", 
       label.right = "Positive PSC", bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
dev.off()

pscData_f$func.loc <- ifelse(is.na(pscData_f$func.loc), 1, pscData_f$func.loc)

sub4<-metagen(pscData_f$PSC, pscData_f$PSC_se,  subgroup=pscData_f$func.loc, 
              method.tau="DL", studlab=pscData_f$AuthorYear, data=pscData_f)
sub4
pdf(file='results/funcLoc instruction.pdf', width=10, height=15)
forest(sub4, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("PSC [95% CI]"), print.tau2=FALSE, label.left = "Negative PSC", 
       label.right = "Positive PSC", bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
dev.off()


sub5<-metagen(pscData_f$PSC, pscData_f$PSC_se,  subgroup=pscData_f$direction, 
              method.tau="DL", studlab=pscData_f$AuthorYear, data=pscData_f)
sub5
pdf(file='results/direction instruction.pdf', width=10, height=15)
forest(sub5, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("PSC [95% CI]"), print.tau2=FALSE, label.left = "Negative PSC", 
       label.right = "Positive PSC", bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
dev.off()

#overall meta-analysis  - PSC from First training only####
#filter by available
FirstData_f <- filter(data_psc_final, !is.na(pscFirst_mean_last))

overall<-metagen(pscFirst_mean_last, pscFirst_var_last, method.tau="DL", studlab=AuthorYear, data=FirstData_f)
overall
forest(overall)

pdf(file='nf_plot.pdf', width=10, height=12)
forest(overall, sortvar=author, fixed=FALSE, random=TRUE, lty.random=2, layout="meta", leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("PSC [95% CI]"), print.tau2=FALSE, label.left = "Negative PSC", 
       label.right = "Positive PSC", bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue")
dev.off()


overall_cont <- metacont(PSC, PSC_se, method.tau="DL", studlab=AuthorYear, data=pscData_f)

c<-metareg(~n, x)
bubble(c)



#Subgroup analysis, forest plots, and meta-regression


a<-metagen(pscData_test$PSC, pscData_test$PSC_se,  subgroup=pscData_test$direction, 
           method.tau="DL", studlab=pscData_test$AuthorYear, data=pscData_test)

forest(a, fixed=TRUE, random=FALSE, test.subgroup=TRUE, 
       test.effect.subgroup.fixed = TRUE, resid.hetstat=FALSE, 
       col.square = "blue", col.diamond.fixed = "lightblue", 
       at=c(-0.25, 0.25,  0.75, 1.25), colgap.forest.left='2.5cm')

forest(a, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, 
       col.square = "blue", col.diamond.fixed = "lightblue", 
       at=c(-0.25, 0.25,  0.75, 1.25), colgap.forest.left='2.5cm')

pscData_test <- filter(pscData_f, !is.na(n))

c <- metareg(formula=~pscData_test$n, x=a, method.tau="DL")
bubble(c)