## SUBGROUP ANALYSIS ##
library(meta)

#blinding
  # double and triple blinding studies were aggregated since there were too few triple blinding studies
  sub.blind <- update(m.gen, 
                   subgroup = blinding, 
                   tau.common = FALSE)
  summary(sub.blind)
  
  
  sub.blind_base <- update(m.gen_base, 
                      subgroup = blinding, 
                      tau.common = FALSE)
  summary(sub.blind_base)
  
  sub.blind
  svg(file='results/base_blind subgroup.svg', width=10, height=16)
  forest(sub.blind_base, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm',
         smlab = "Standardized Mean \n Difference")
  dev.off()

#device
  sub.device <- update(m.gen, 
                  subgroup = device, 
                  tau.common = FALSE)
  summary(sub.device)
  
  sub.device_base <- update(m.gen_base, 
                       subgroup = device, 
                       tau.common = FALSE)
  summary(sub.device_base)
  
  
  sub.device
  pdf(file='results/device subgroup.pdf', width=10, height=15)
  svg(filename='results/device subgroup.svg', width=10, height=16)
  forest(sub.device, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "#5b8bff", col.diamond.random = "lightblue", colgap.forest.left='5.5cm', 
         smlab = "Standardized Mean \n Difference")
  dev.off()
  
#instruction
  sub.instruction <- update(m.gen, 
                        subgroup = instruction_yn, 
                        tau.common = FALSE)
  summary(sub.instruction)
  
  sub.instruction_base <- update(m.gen_base, 
                            subgroup = instruction_yn, 
                            tau.common = FALSE)
  summary(sub.instruction_base)
  sub.instruction
  
  svg(file='results/base_instruction subgroup.svg', width=10, height=15)
  forest(sub.instruction_base, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm',
         smlab = "Standardized Mean \n Difference")
  dev.off()

#motivation
  sub.motivation <- update(m.gen, 
                        subgroup = motivation_yn, 
                        tau.common = FALSE)
  summary(sub.motivation)
  
  sub.motivation_base <- update(m.gen_base, 
                           subgroup = motivation_yn, 
                           tau.common = FALSE)
  summary(sub.motivation_base)
  
  sub.motivation
  svg(file='results/first_motivation subgroup.svg', width=10, height=15)
  forest(sub.motivation, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm',
         smlab = "Standardized Mean \n Difference")
  dev.off()
  
#rehearsal
  sub.rehearsal <- update(m.gen, 
                       subgroup = rehearsal, 
                       tau.common = FALSE)
  summary(sub.rehearsal)
  
  sub.rehearsal_base <- update(m.gen_base, 
                          subgroup = rehearsal, 
                          tau.common = FALSE)
  summary(sub.rehearsal_base)
  
  sub.rehearsal
  svg(file='results/rehearsal subgroup.svg', width=10, height=15)
  forest(sub.rehearsal, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm',
         smlab = "Standardized Mean \n Difference")
  dev.off()
  
#functional localizer
  func.loc <- update(m.gen, 
                       subgroup = func.loc, 
                       tau.common = FALSE)
  summary(func.loc)
  
  func.loc_base <- update(m.gen_base, 
                     subgroup = func.loc, 
                     tau.common = FALSE)
  summary(func.loc_base)
  
  func.loc
  svg(file='results/base_func.loc subgroup.svg', width=10, height=15)
  forest(m.gen_base, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm',
         smlab = "Standardized Mean \n Difference")
  dev.off()
  
  
# update timing (continuous or intermittent)
  sub.update.timing <- update(m.gen, 
                     subgroup = update.timing, 
                     tau.common = FALSE)
  summary(sub.update.timing)
  
  sub.update.timing_base <- update(m.gen_base, 
                              subgroup = update.timing, 
                              tau.common = FALSE)
  summary(sub.update.timing_base)
  
  sub.update.timing
  svg(file='results/update.timing subgroup.svg', width=10, height=15)
  forest(sub.update.timing, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm',
         smlab = "Standardized Mean \n Difference")
  dev.off()
  
  
  # direction
  sub.direction <- update(m.gen, 
                              subgroup = direction, 
                              tau.common = FALSE)
  summary(sub.direction)
  
  sub.direction_base <- update(m.gen_base, 
                          subgroup = direction, 
                          tau.common = FALSE)
  summary(sub.direction_base)
  
  
  svg(file='results/base_direction subgroup.svg', width=10, height=15)
  forest(sub.direction_base, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm',
         smlab = "Standardized Mean \n Difference")
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
  #risk of bias

  # what are variables that have more than 10 cases?
    #make count
  #fillout empty cases with data from papers
  #
  

