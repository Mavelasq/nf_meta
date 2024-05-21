## SUBGROUP ANALYSIS ##

#blinding
  # double and triple blinding studies were aggregated since there were too few triple blinding studies
  sub.blind <- update(m.gen_outX, 
                   subgroup = blinding, 
                   tau.common = FALSE)
  summary(sub.blind)
  
  sub.blind
  pdf(file='results/blind subgroup.pdf', width=10, height=15)
  forest(sub.blind, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm',
         bylab  = c("No blinding", "Blinding"))
  dev.off()

#device
 sub.device <- update(m.gen_outX, 
                  subgroup = device, 
                  tau.common = FALSE)
  summary(sub.device)
  
  sub.device
  pdf(file='results/device subgroup.pdf', width=10, height=15)
  forest(sub.device, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()

#instruction
  sub.instruction <- update(m.gen_outX, 
                        subgroup = instruction_yn, 
                        tau.common = FALSE)
  sub.instruction
  pdf(file='results/instruction subgroup.pdf', width=10, height=15)
  forest(sub.instruction, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()

#motivation
  sub.motivation <- update(m.gen_outX, 
                        subgroup = motivation_yn, 
                        tau.common = FALSE)
  sub.motivation
  pdf(file='results/motivation subgroup.pdf', width=10, height=15)
  forest(sub.motivation, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()
  
#rehearsal
  sub.rehearsal <- update(m.gen_outX, 
                       subgroup = rehearsal, 
                       tau.common = FALSE)
  sub.rehearsal
  pdf(file='results/rehearsal subgroup.pdf', width=10, height=15)
  forest(sub.rehearsal, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()
  
#functional localizer
  func.loc <- update(m.gen_outX, 
                       subgroup = func.loc, 
                       tau.common = FALSE)
  func.loc
  pdf(file='results/func.loc subgroup.pdf', width=10, height=15)
  forest(func.loc, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()
  
  
# update timing (continuous or intermittent)
  sub.update.timing <- update(m.gen_outX, 
                     subgroup = update.timing, 
                     tau.common = FALSE)
  
  summary(m.gen_outX)
  sub.update.timing
  pdf(file='results/update.timing subgroup.pdf', width=10, height=15)
  forest(sub.update.timing, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()
  
# additional analyses
  #feedback - check per study
  #eeg freq
  #ratio (filter by EEG papers)
  #roi (aggregate regions?)
  #bias
  #from baseline studies
  #test sig difference between pre-post sd to see if Glass' delta is necessary
  #convert PSC-only studies to Z score?

  # what are variables that have more than 10 cases?
    #make count
  #fillout empty cases with data from papers
  #
  

