## Set dir

setwd("~/Work/Ds/GettingCleaningData/")
#load(".RData")
rm(list=ls()) # clean everything

## Load

DataTrain = read.table(file="X_train.txt")
DataTest = read.table(file="X_test.txt")

## rbind

Data = rbind(DataTrain, DataTest)
rm(DataTrain); rm(DataTest)

## name col and select col

NamCol = read.table(file="features.txt", as.is=T)[,2]

IdxMeanCol = grep("-mean()", NamCol, fixed=T)
IdxStdCol = grep("-std()", NamCol, fixed=T)

NamColSel = NamCol[c(IdxMeanCol, IdxStdCol)]
DataCol = Data[c(IdxMeanCol, IdxStdCol)]

## shorten names of col

NamColSelSub = gsub("()", "", NamColSel, fixed=T)
NamColSelSub = gsub("-mean", "-m", NamColSelSub, fixed=T)

names(DataCol) = NamColSelSub

## load activity

ActivityTrain = read.table(file="y_train.txt")$V1
ActivityTest = read.table(file="y_test.txt")$V1

Activity = c(ActivityTrain, ActivityTest)

## label activity

LevelActivity = read.table(file="activity_labels.txt", as.is=T)[,2]

LabelActivity = rep(NA, length(Activity))
for (i in seq_along(LevelActivity)) {
  LabelActivity = replace(LabelActivity, Activity == i, LevelActivity[i])
}

## add subjects

SubjTrain = read.table(file="subject_train.txt")$V1
SubjTest = read.table(file="subject_test.txt")$V1

Subj = c(SubjTrain, SubjTest)

## merge col

DataFinal = data.frame(Subject = Subj, Activity = LabelActivity, DataCol)

## new tidy data set

SubjTidy = c()
ActivityTidy = c()
MeanTidy = matrix(nrow=0, ncol=ncol(DataFinal)-2)
for (subj in unique(DataFinal$Subject)) {
  SelSubj = DataFinal$Subject == subj
  DataSel = DataFinal[SelSubj, c(-1, -2)] # data sel for a specific subject, remove subj and activity
  ActivitySel = DataFinal[SelSubj, "Activity"]
  Mean = sapply(DataSel, function(x) { tapply(x, ActivitySel, mean) })
  RepSubj = rep(subj, length(LevelActivity))
  LevActivity = row.names(Mean)
  SubjTidy = c(SubjTidy, RepSubj)
  ActivityTidy = c(ActivityTidy, LevActivity)
  MeanTidy = rbind(MeanTidy, Mean)
}
rownames(MeanTidy) = NULL
Tidy = data.frame(Subject = SubjTidy, Activity = ActivityTidy, MeanTidy)

## export tidy

write.table(Tidy, file="Tidy.txt", row.name=F)





