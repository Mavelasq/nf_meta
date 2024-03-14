setwd("D:/Documents/Projects/NF_Meta Analysis/analysis/nf_meta")

merge1 <- read.csv("data/nf_data.csv")
merge2 <- read.csv("data/sam_edit_030124.csv")
merge3 <- read.csv("data/detailed_outcomes2merge.csv")

colnames(merge2)[2] <- "author_n"

m1 <- merge(merge1[,1:4], merge2[,2:ncol(merge2)], by = c("author_n", "year"), all = TRUE)
m2 <- merge(m1, merge3, by = c("author", "year"), all = TRUE)


write.csv(m2, file = "data/data_template.csv")


data_temp <- read.csv("data/data_template.csv")

data_psc <- data_temp

#first for pre intervention psc calculations
#list ids that have pre intervention values
  
pre.ids <- which(!is.na(data_psc$mean_pre)) #get indices of participants with mean_pre values
idx_mean <- which(grepl("mean", colnames(data_psc)[67:91]))+66  #get indices of columns with "mean" in name

for (p in pre.ids){ #loop through participants
  
  idxs <- idx_mean[which(!is.na(data_psc[p,idx_mean]))] #get indexes of columns with values in row p
  
  for (i in idxs){ #loop through means/columns
    data_psc[p, i+26] <- (as.numeric(data_psc[p, i]) - data_psc$mean_pre[p])/data_psc$mean_pre[p] #calculate percent signal change and add value to pscBase
  }
}


#calculate for first nft values
pre.ids <- which(!is.na(data_psc$mean_first)) #get indexes of participants with mean_pre values
idx_mean <- which(grepl("mean", colnames(data_psc)[69:91]))+68  #get indexes of columns with "mean" in name

for (p in pre.ids){ #loop through participants
  
  idxs <- idx_mean[which(!is.na(data_psc[p,idx_mean]))] #get indexes of columns with values in row p
  
  for (i in idxs){ #loop through means/columns
    data_psc[p, i+52] <- (as.numeric(data_psc[p, i]) - data_psc$mean_first[p])/data_psc$mean_first[p] #calculate percent signal change and add value to pscFirst
  }
}


write.csv(data_psc, file = "data/data_psc.csv")
