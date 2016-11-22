#Convert int to date
library("zoo")
install.packages("zoo")
as.Date(13474)

#Install and call Package #
#==========================================================================================================================================
#install.packages("sqldf")
#install.packages("data.table")
library(sqldf)
library(data.table)

#Read File#
#==========================================================================================================================================
patientResultComorbidity <- fread(paste('D:/BIG DATA/Prof Yehh/DOC/Result/3.patientResultComorbidity.csv', sep=""), header = T, sep = ',', verbose=F, colClasses = "character")
TATOL_PATIENT <- nrow(patientResultComorbidity)
colnames(patientResultComorbidity)

UnQualifiedData = (patientResultComorbidity[as.integer(patientResultComorbidity$NumberVisitHospital) < 8 | is.na(as.integer(patientResultComorbidity$NumberVisitHospital))])
QualifiedData = (patientResultComorbidity[as.integer(patientResultComorbidity$NumberVisitHospital) >= 8])


#Finally, save result Qualified Data#
#==========================================================================================================================================
write.table(QualifiedData, file = paste('D:/BIG DATA/Prof Yehh/DOC/Result/','QualifiedData.csv', sep=""), sep = ",", quote = FALSE, col.names = TRUE, row.names=FALSE, append=FALSE)

#Finally, save result UnQualified Data#
#==========================================================================================================================================
write.table(UnQualifiedData, file = paste('D:/BIG DATA/Prof Yehh/DOC/Result/','UnqualifiedData.csv', sep=""), sep = ",", quote = FALSE, col.names = TRUE, row.names=FALSE, append=FALSE)
