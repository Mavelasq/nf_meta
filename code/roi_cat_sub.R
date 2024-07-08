nf_data_first_short <- read.csv("data/nf_data(from_first)_short.csv")
nf_data_base_short  <- read.csv("data/nf_data(from_base)_short.csv")


nf_data_first_short <- filter(nf_data_first_short, !is.na(SMD_first_mean_last))
nf_data_base_short  <- filter(nf_data_base_short, !is.na(SMD_base_mean_post))


m.gen_f_short <- metagen(TE = SMD_first_mean_last,
                 seTE = SMD_first_var_last,
                 studlab = author_n,
                 data = nf_data_first_short,
                 sm = "g",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK",
                 prediction = TRUE,
                 title = "Neurofeedback")
summary(m.gen_f_short)

m.gen_b_short <- metagen(TE = SMD_base_mean_post,
                         seTE = SMD_base_var_post,
                         studlab = author_n,
                         data = nf_data_base_short,
                         sm = "g",
                         fixed = FALSE,
                         random = TRUE,
                         method.tau = "REML",
                         method.random.ci = "HK",
                         prediction = TRUE,
                         title = "Neurofeedback")
summary(m.gen_b_short)

m.gen_f_short_outX <- update(m.gen_f_short, exclude = c(35)) #update model without outlier

#FEEDBACK
sub.feed <- update(m.gen_f_short_outX, 
                    subgroup = feedback_cat, 
                    tau.common = FALSE)
summary(sub.feed)


sub.feed_base <- update(m.gen_b_short, 
                         subgroup = feedback_cat, 
                         tau.common = FALSE)
summary(sub.feed_base)

sub.blind
svg(file='results/base_feed subgroup.svg', width=10, height=16)
forest(sub.feed_base, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm',
       smlab = "Standardized Mean \n Difference")
dev.off()


#rois
sub.feed <- update(m.gen_f_short_outX, 
                   subgroup = roi, 
                   tau.common = FALSE)
summary(sub.feed)


sub.feed_base <- update(m.gen_b_short, 
                        subgroup = feedback_cat, 
                        tau.common = FALSE)
summary(sub.feed_base)

sub.blind
svg(file='results/base_feed subgroup.svg', width=10, height=16)
forest(sub.feed_base, fixed=FALSE,random=TRUE, test.subgroup=TRUE,  
       test.effect.subgroup.random = TRUE, resid.hetstat=FALSE, leftcols=c("studlab"), leftlab=c("Study Author"), 
       rightcols=c("effect.ci"), rightlab=c("[g% CI]"), print.tau2=FALSE, bottom.lr = TRUE, col.square = "blue", col.diamond.random = "lightblue", colgap.forest.left='5.5cm',
       smlab = "Standardized Mean \n Difference")
dev.off()