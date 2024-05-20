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
 device <- update(m.gen_outX, 
                  subgroup = device, 
                  tau.common = FALSE)
  summary(device)
  
  device
  pdf(file='results/device subgroup.pdf', width=10, height=15)
  forest(device, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()

#instruction
  instruction <- update(overall, 
                        subgroup = instruction, 
                        tau.common = FALSE)
  instruction
  pdf(file='results/instruction subgroup.pdf', width=10, height=15)
  forest(instruction, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()

#motivation
  motivation <- update(overall, 
                        subgroup = motivation, 
                        tau.common = FALSE)
  motivation
  pdf(file='results/motivation subgroup.pdf', width=10, height=15)
  forest(motivation, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()
  
#rehearsal
  rehearsal <- update(overall, 
                       subgroup = rehearsal, 
                       tau.common = FALSE)
  rehearsal
  pdf(file='results/rehearsal subgroup.pdf', width=10, height=15)
  forest(rehearsal, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()
  
#functional localizer
  func.loc <- update(overall, 
                       subgroup = func.loc, 
                       tau.common = FALSE)
  func.loc
  pdf(file='results/func.loc subgroup.pdf', width=10, height=15)
  forest(func.loc, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()
  
  
#functional localizer
  update.timing <- update(m.gen_outX, 
                     subgroup = update.timing, 
                     tau.common = FALSE)
  
  summary(m.gen_outX)
  update.timing
  pdf(file='results/update.timing subgroup.pdf', width=10, height=15)
  forest(update.timing, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()
  
#feedback - how?
#per study
#what are numbers
#direction (check studies with both up/down)
#cont vs interval?
#eeg freq
#study quality
#ratio
#roi (aggregate)

  
  #instruction
  #motivation
  #feedback - how?
  #per study
  #what are numbers
  #direction (check studies with both up/down)
  #cont vs interval?
  #rehearsal
  #functional localizer
  #eeg freq
  #study quality
  #ratio
  #roi (aggregate)
  #do power analyses per subgroup analysis
  # what are variables that have more than 10 cases?
  
  
  # to do
  # report sensitivity analysis  
  # check studies with no SE
  # run gosh for outliers
  # test sig difference between pre-post sd to see if Glass' delta is necessary
  # Clean code for samantha
  # Convert PSC-only studies to Z score?
  # bias 
  