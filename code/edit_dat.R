##calculate PSC##
library(tidyverse)
library(readxl)

# set working directory
setwd("D:/Documents/Projects/NF_Meta Analysis/analysis/nf_meta")

# open data set
nf_data   <- read_excel("data/nf_data.xlsx", sheet = 1)

# filter out rejected papers
nf_data_f <- nf_data %>%
  filter(reject ==0)

# convert SE to SD
nf_data_f <- calc_sd(nf_data_f)


# initialize output dataframes
smd_nft_fromBase   <- initialize_dfOut(nf_data_f, "base", "nft", "SMD")
smd_nft_fromFirst  <- initialize_dfOut(nf_data_f, "first", "nft", "SMD")
smd_rest_fromBase  <- initialize_dfOut(nf_data_f, "base", "rest", "SMD")
smd_rest_fromFirst <- initialize_dfOut(nf_data_f, "first", "rest", "SMD")


#calculate SMD or PSC

# function calculates SMD (with sd1 or pooled) or PSC from baseline or first trial, for rest and NFT trials

outcomeCalc <- function(nf_data, outputCalc, out_data, firstValue, nftOrBase){
  #indices for start and end of training or rest data
    idx_start_tr <- which(colnames(nf_data) == "mean_pre")
    idx_end_tr   <- which(colnames(nf_data) == "var_ses_15")
    
    idx_start_bl <- which(colnames(nf_data) == "mean_ses_bl_2")
    idx_end_bl   <- which(colnames(nf_data) == "var_ses_bl_10")
    idx_end      <- ncol(nf_data)
  
  # convert df columns to numeric
    nf_data[,idx_start_tr:idx_end] <- as.data.frame(lapply(nf_data[, idx_start:idx_end], as.numeric))
    nf_data$n <- as.numeric(nf_data$n)
    
    
    pre.ids    <- which(!is.na(nf_data$mean_pre)) #get indices of papers with mean_pre values
    first.ids  <- which(!is.na(nf_data$mean_first)) #get indices of papers with mean_first values
    
    #training sessions indices
    idx_mean_tr   <- which(grepl("mean", colnames(nf_data)[idx_start_tr:idx_end_tr]))+(idx_start_tr-1)  #get indices of columns with "mean" in name
    idx_sd_tr     <- which(grepl("var", colnames(nf_data)[idx_start_tr:idx_end_tr]))+(idx_start_tr-1)   #get indices of columns with "var" in name
  
    #rest sessions indices
    idx_mean_bl   <- which(grepl("mean", colnames(nf_data)[idx_start_bl:idx_end_bl]))+(idx_start_bl-1)  #get indices of columns with "mean" in name
    idx_sd_bl     <- which(grepl("var", colnames(nf_data)[idx_start_bl:idx_end_bl]))+(idx_start_bl-1)   #get indices of columns with "var" in name
    
    #if outputCalc == "smd" run the following:
    #calculate smd
    for (p in pre.ids){ #loop through participants. Change to pre.ids if firstValue is "base" or to first.ids if firstValues is "first"
      
      idxs_mean <- idx_mean[which(!is.na(nf_data[p,idx_mean]))] #get indices of columns (mean) with values in row p
      idxs_sd   <- idx_sd[which(!is.na(nf_data[p,idx_sd]))] #get indexes of columns (sd) with values in row p
      
      if (method == "smd"){
        
        for (s in idxs_sd){ #loop through vars/columns
          sd_pooled <- sqrt((nf_data$var_pre[p]^2)+(nf_data[p,s]^2)/2) #calculate percent signal change and add value to pscBase
        }
        
        for (i in idxs_mean){ #loop through means/columns
          out_data[p, (i+1)-i] <- ((as.numeric(nf_data[p, i]) - as.numeric(nf_data$mean_pre[p])))/sd_pooled #calculate percent signal change and add value to pscBase
          
        }
      }
    }
  
  #if outputCalc == "psc" run the following:  
  # calculate psc  
  for (p in pre.ids){ #loop through participants
    
    idxs_mean <- idx_mean_tr[which(!is.na(nf_data[p,idx_mean_tr]))] #get indices of columns (mean) with values in row p
    idxs_sd   <- idx_sd_tr[which(!is.na(nf_data[p,idx_sd_tr]))] #get indexes of columns (sd) with values in row p
    
    if (method == "psc"){
      for (i in idxs_mean){ #loop through means/columns
        smd_nft_fromBase[p, (i+1)-i] <- (as.numeric(nf_data[p, i]) - as.numeric(nf_data$mean_pre[p]))/as.numeric(nf_data$mean_pre[p]) #calculate percent signal change and add value to pscBase
      }
      
      for (s in idxs_sd){ #loop through vars/columns
        nf_data[p, s+66] <- sqrt((nf_data$var_pre[p]^2)+(nf_data[p,s]^2)-(2*0.5*nf_data$var_pre[p]*nf_data[p,s])) #calculate percent signal change and add value to pscBase
      }
    }
  }
}

}
  

#calculate for first nft values
pre.ids <- which(!is.na(data_psc_f_sd$mean_first)) #get indexes of participants with mean_pre values
idx_mean <- which(grepl("mean", colnames(data_psc_f_sd)[69:99]))+68  #get indexes of columns with "mean" in name
idx_sd <- which(grepl("var", colnames(data_psc_f_sd)[69:99]))+68   #get indices of columns with "mean" in name

for (p in pre.ids){ #loop through participants
  
  idxs_mean <- idx_mean[which(!is.na(data_psc_f_sd[p,idx_mean]))] #get indexes of columns with values in row p
  idxs_sd <- idx_sd[which(!is.na(data_psc_f_sd[p,idx_sd]))] #get indexes of columns with values in row p
  
  for (i in idxs_mean){ #loop through means/columns
    data_psc_f_sd[p, i+32] <- (as.numeric(data_psc_f_sd[p, i]) - data_psc_f_sd$mean_first[p])/data_psc_f_sd$mean_first[p] #calculate percent signal change and add value to pscFirst
  }
  
  for (s in idxs_sd){ #loop through vars/columns
    data_psc_f_sd[p, s+32] <- sqrt((data_psc_f_sd$var_first[p]^2)+(data_psc_f_sd[p,s]^2)-(2*0.5*data_psc_f_sd$var_first[p]*data_psc_f_sd[p,s])) #calculate percent signal change and add value to pscBase
  }
}




#functions####

# function: convert se to sd 
calc_sd <- function(nf_data){
  
  #initialize start and end indices for outcome columns
  idx_start <- which(colnames(nf_data)=="mean_pre")
  idx_end   <- ncol(nf_data)
  
  # convert df columns to numeric
  nf_data[,idx_start:idx_end] <- as.data.frame(lapply(nf_data[, idx_start:idx_end], as.numeric))
  nf_data$n <- as.numeric(nf_data$n)
  
  se_idxs <- which(nf_data$var_measure == "SE") #indices of papers with SE
  idx_var <- which(grepl("var", colnames(nf_data)[idx_start:idx_end]))+(idx_start-1)   #get indices of columns with "var" in name
  
  for (p in se_idxs){ #loop through papers
    for (s in idx_var){ #vars/columns
      if (is.na(nf_data[p,s])){
        nf_data[p,s] <- NA
      } else {
        nf_data[p,s] <- nf_data[p,s]*(sqrt(nf_data$n[p]))
        nf_data$var_measure[p] <- "SD"
      }
    }
  }
  return(nf_data)
} 

# function: initialize outcome data frame for SMD or psc from first NFT or baseline, or baseline or NFT training-only trials
# run as initialize_dfOut(dataFrame, "first" or "base", "nft" or "rest", "smd" or "psc")

initialize_dfOut <- function(nf_data, firstValue, nftOrBase, outcome) {
  #indices
  #indices for training values
  idx_end_tr   <- which(colnames(nf_data) == "var_ses_15")
  
  idx_start_bl <- which(colnames(nf_data) == "mean_ses_bl_2")
  idx_end_bl   <- which(colnames(nf_data) == "var_ses_bl_10")
  
  if (nftOrBase == "nft"){
    if (firstValue == "base"){
      idx_start_tr <- which(colnames(nf_data) == "mean_first")
    } else {idx_start_tr <- which(colnames(nf_data) == "mean_last")}
    out <- nf_data[idx_start_tr:idx_end_tr]
    colnames(out) <- paste(outcome, firstValue, colnames(out), sep = "_")
  }else {    
    out <- nf_data[idx_start_bl:idx_end_bl]
    colnames(out) <- paste(outcome, firstValue, colnames(out), sep = "_")
  }
  out[,] <- NA
  return(out)
}    



####DOUBLE CHECK
#convert SD to SEM
data_psc_final <- data_psc_f_sd

sd_idxs <- which(data_psc_final$var_measure == "SD") #indices of papers with SEM
idx_sd <- which(grepl("var", colnames(data_psc_final)[101:166]))+100   #get indices of columns with "mean" in name

for (p in se_idxs){ #loop through papers
  for (s in idx_sd){ #vars/columns
    data_psc_final[p, s] <- as.numeric(data_psc_f_sd[p, s])/(sqrt(as.numeric(data_psc_f_sd$n[p])))
  }
}


write.csv(data_psc, file = "data/data_psc.csv")



#functions####

# function: convert se to sd 
calc_sd <- function(nf_data){
  
  #initialize start and end indices for outcome columns
  idx_start <- which(colnames(nf_data)=="mean_pre")
  idx_end   <- ncol(nf_data)
  
  # convert df columns to numeric
  nf_data[,idx_start:idx_end] <- as.data.frame(lapply(nf_data[, idx_start:idx_end], as.numeric))
  nf_data$n <- as.numeric(nf_data$n)
  
  se_idxs <- which(nf_data$var_measure == "SE") #indices of papers with SE
  idx_var <- which(grepl("var", colnames(nf_data)[idx_start:idx_end]))+(idx_start-1)   #get indices of columns with "var" in name
  
  for (p in se_idxs){ #loop through papers
    for (s in idx_var){ #vars/columns
      if (is.na(nf_data[p,s])){
        nf_data[p,s] <- NA
      } else {
        nf_data[p,s] <- nf_data[p,s]*(sqrt(nf_data$n[p]))
        nf_data$var_measure[p] <- "SD"
      }
    }
  }
  return(nf_data)
} 

# function: initialize outcome data frame for SMD or psc from first NFT or baseline, or baseline or NFT training-only trials
# run as initialize_dfOut(dataFrame, "first" or "base", "nft" or "rest", "smd" or "psc")

initialize_dfOut <- function(nf_data, firstValue, nftOrBase, outcome) {
  #indices
  #indices for training values
  idx_end_tr   <- which(colnames(nf_data) == "var_ses_15")
  
  idx_start_bl <- which(colnames(nf_data) == "mean_ses_bl_2")
  idx_end_bl   <- which(colnames(nf_data) == "var_ses_bl_10")
  
  if (nftOrBase == "nft"){
    if (firstValue == "base"){
      idx_start_tr <- which(colnames(nf_data) == "mean_first")
    } else {idx_start_tr <- which(colnames(nf_data) == "mean_last")}
    out <- nf_data[idx_start_tr:idx_end_tr]
    colnames(out) <- paste(outcome, firstValue, colnames(out), sep = "_")
  }else {    
    out <- nf_data[idx_start_bl:idx_end_bl]
    colnames(out) <- paste(outcome, firstValue, colnames(out), sep = "_")
  }
  out[,] <- NA
  return(out)
}    

#open and merge if needed####
merge1 <- read.csv("data/nf_data.csv")
merge2 <- read.csv("data/sam_edit_030124.csv")
merge3 <- read.csv("data/detailed_outcomes2merge.csv")

colnames(merge2)[2] <- "author_n"

m1 <- merge(merge1[,1:4], merge2[,2:ncol(merge2)], by = c("author_n", "year"), all = TRUE)
m2 <- merge(m1, merge3, by = c("author", "year"), all = TRUE)

write.csv(m2, file = "data/data_template.csv")


