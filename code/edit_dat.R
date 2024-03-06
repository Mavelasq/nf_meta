setwd("D:/Documents/Projects/NF_Meta Analysis/analysis/nf_meta")

merge1 <- read.csv("data/nf_data.csv")
merge2 <- read.csv("data/sam_edit_030124.csv")
merge3 <- read.csv("data/detailed_outcomes2merge.csv")

colnames(merge2)[2] <- "author_n"

m1 <- merge(merge1[,1:4], merge2[,2:ncol(merge2)], by = c("author_n", "year"), all = TRUE)
m2 <- merge(m1, merge3, by = c("author", "year"), all = TRUE)


write.csv(m2, file = "data/data_template.csv")


data_temp <- read.csv("data/data_template.csv")

if (is.na(data_temp[11,65])){
  
}
  
if (is.na(data_temp[11,67]))
  
idxs <- which(is.na(data_temp[11,69:91]))+68

for (i in idxs){
data_temp$psc_mean_last[11] <- (data_temp$mean_last[11] - data_temp$mean_first[11])/data_temp$mean_first[11]
}

data_temp$psc_mean_last[11] <- (data_temp$mean_last[11] - data_temp$mean_first[11])/data_temp$mean_first[11]
data_temp$psc_mean_last[11] <- (data_temp$mean_last[11] - data_temp$mean_first[11])/data_temp$mean_first[11]
data_temp$psc_mean_last[11] <- (data_temp$mean_last[11] - data_temp$mean_first[11])/data_temp$mean_first[11]
data_temp$psc_mean_last[11] <- (data_temp$mean_last[11] - data_temp$mean_first[11])/data_temp$mean_first[11]
