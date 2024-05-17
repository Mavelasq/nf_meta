# filter out NAs (this is done for plotting)
  smd_nft_fromFirst$author <- nf_data_filtered$author_n
  smd_nft_fromFirst$blinding <- nf_data_filtered$blinding
  smd_nft_fromFirst$device <- nf_data_filtered$device
  smd_nft_fromFirst$instruction <- nf_data_filtered$instruction_yn
  smd_nft_fromFirst$motivation <- nf_data_filtered$motivation_yn
  smd_nft_fromFirst$direction <- nf_data_filtered$direction
  smd_nft_fromFirst$rehearsal <- nf_data_filtered$rehearsal
  smd_nft_fromFirst$func_loc <- nf_data_filtered$func.loc
  
  smd_nft_fromFirst_f      <- filter(smd_nft_fromFirst, !is.na(SMD_first_mean_last))

# overall random effects ####
  overall <- metagen(TE = SMD_first_mean_last,
                     seTE = SMD_first_var_last,
                     studlab = author,
                     data = smd_nft_fromFirst_f,
                     sm = "Hedge's g",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     prediction = TRUE,
                     title = "Neurofeedback")
  summary(m.gen)

# Outlier rejection due to high heterogeneity (I^2 > 50%)
find.outliers(m.gen) 
m.gen.inf <- InfluenceAnalysis(m.gen, random = TRUE)
plot(m.gen.inf, "baujat")
plot(m.gen.inf, "influence")
plot(m.gen.inf, "es")
plot(m.gen.inf, "i2")
m.rma <- rma(yi = m.gen$TE,
             sei = m.gen$seTE,
             method = m.gen$method.tau,
             test = "knha")
res.gosh <- gosh(m.rma)


# Forrest plots
m.gen
forest(m.gen)

pdf(file='results/nf_plot.pdf', width=10, height=12)
forest(m.gen, sortvar=author, fixed=FALSE, random=TRUE, lty.random=2, layout="meta", leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("g [95% CI]"), print.tau2=FALSE, 
       bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue")
dev.off()


# Subgroup analyses ####

#blinding
#aggregate all blinding? 
#do power analysis
#change name of variable
  device <- update(overall, 
                   subgroup = device, 
                   tau.common = FALSE)
  summary(device)
  
  blinding
  pdf(file='results/blind subgroup.pdf', width=10, height=15)
  forest(blinding, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
         test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
         rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm')
  dev.off()

#device
 device <- update(overall, 
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
  
#feedback - how?
#per study
#what are numbers
#direction (check studies with both up/down)
#cont vs interval?
#eeg freq
#study quality
#ratio
#roi (aggregate)
