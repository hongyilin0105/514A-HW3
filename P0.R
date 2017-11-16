library(data.table)
library(BBmisc)
library(stringr)
library(reshape2)
#install.packages("RWeka")
#install.packages("rJava")
library(RWeka)
library(rJava)
library(RWekajars)
library(partykit)
library(ggplot2)

#Data Pre-processing
## For AD data 
raw_case = read.table("case.gex", sep = "")
raw_case_dt = data.table(t(raw_case))
colnames(raw_case_dt) = as.character(raw_case_dt[1,])
raw_case_dt = raw_case_dt[-1,]
colnames(raw_case_dt) = str_replace(colnames(raw_case_dt),"-","_")
raw_case_dt$Classes = as.factor(raw_case_dt$Classes)
raw_case_dt[, 3:8562] = lapply(raw_case_dt[,3:8562], as.numeric)

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
case_impute_dt = raw_case_dt[, lapply(.SD, impute.mean)]
case_impute_dt$Classes = as.factor(case_impute_dt$Classes)
normalized_ad = cbind(case_impute_dt[,1:2],t(normalize(t(case_impute_dt[,3:8562]), method = "range", margin = 2L, range = c(-1, 1), on.constant = "quiet")))
summary(normalized_ad[,3:10])


## For control data 
raw_ctrl = read.table("ctrl.gex", sep = "")
raw_ctrl_dt = data.table(t(raw_ctrl))
colnames(raw_ctrl_dt) = as.character(raw_ctrl_dt[1,])
raw_ctrl_dt = raw_ctrl_dt[-1,]
colnames(raw_ctrl_dt) = str_replace(colnames(raw_ctrl_dt),"-","_")
raw_ctrl_dt$Classes = as.factor(raw_ctrl_dt$Classes)
raw_ctrl_dt[, 3:8562] = lapply(raw_ctrl_dt[,3:8562], as.numeric)

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
ctrl_impute_dt = raw_ctrl_dt[, lapply(.SD, impute.mean)]
ctrl_impute_dt$Classes = as.factor(ctrl_impute_dt$Classes)
#normalized_ctrl = cbind(ctrl_impute_dt[,1:2],normalize(ctrl_impute_dt[,3:8562], method = "range", range = c(-1, 1), on.constant = "quiet"))
normalized_ctrl = cbind(ctrl_impute_dt[,1:2],t(normalize(t(ctrl_impute_dt[,3:8562]), method = "range", range = c(-1, 1), margin = 2L, on.constant = "quiet")))
#par(mfrow = c(1,3))
#hist(as.numeric(ctrl_impute_dt[1,3:8562]), xlim = c(-2,2))
#hist(as.numeric(normalized_ctrl[1,3:8562]), xlim = c(-2,2), new = F)
#hist(as.numeric(normalized_ctrl2[1,3:8562]), xlim = c(-2,2), new = F)
summary(normalized_ctrl[,1:5])


#release some space
rm(case_impute_dt)
rm(ctrl_impute_dt)

#write out prepared data
write.csv(normalized_ad, "norm_ad.csv", row.names = F)
write.csv(normalized_ctrl, "norm_ctrl.csv", row.names = F)
