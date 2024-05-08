install.packages('meta')

#Set working directory to relevant folder

setwd('D:/Documents/Projects/NF_Meta Analysis/analysis/nf_meta/data')
#Use the meta package
library(meta)

#Read in dataset
nf<-read.table('nf_input.txt', header=T, sep='\t')

#create author year variable
nf$AuthorYear<-as.character(paste(nf$author, nf$year, sep=" "))

#overall meta-analysis
overall<-metagen(PSC, SE_psc, method.tau="DL", studlab=AuthorYear, data=nf)
overall
forest(overall)

pdf(file='nf_plot.pdf', width=10, height=12)
forest(overall, sortvar=author, fixed=FALSE, random=TRUE, lty.random=2, layout="meta", leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("PSC [95% CI]"), print.tau2=FALSE, label.left = "Negative PSC", 
       label.right = "Positive PSC", bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue")
dev.off()

#subgroup
#device
sub1<-metagen(nf$PSC, nf$SE_psc,  subgroup=nf$device, 
           method.tau="DL", studlab=nf$AuthorYear, data=nf)
sub1
pdf(file='device subgroup.pdf', width=10, height=12)
forest(sub1, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("PSC [95% CI]"), print.tau2=FALSE, label.left = "Negative PSC", 
       label.right = "Positive PSC", bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
dev.off()

#instruction
sub2<-metagen(nf$PSC, nf$SE_psc,  subgroup=nf$instruction, 
              method.tau="DL", studlab=nf$AuthorYear, data=nf)
sub2
pdf(file='instruction subgroup.pdf', width=10, height=12)
forest(sub2, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("PSC [95% CI]"), print.tau2=FALSE, label.left = "Negative PSC", 
       label.right = "Positive PSC", bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
dev.off()

#motivation
sub3<-metagen(nf$PSC, nf$SE_psc,  subgroup=nf$motivation, 
              method.tau="DL", studlab=nf$AuthorYear, data=nf)
sub3
pdf(file='motivation subgroup.pdf', width=10, height=12)
forest(sub3, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("PSC [95% CI]"), print.tau2=FALSE, label.left = "Negative PSC", 
       label.right = "Positive PSC", bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
dev.off()

