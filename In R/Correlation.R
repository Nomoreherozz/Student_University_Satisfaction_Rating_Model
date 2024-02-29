####Create subset for chosen variables 
data_cor=subset(data, select = -c(Sex,Year, Age))
####Calculate correlation
data_cor$Overall <- as.numeric(data_cor$Overall)
data_cor$Faculties <- as.numeric(data_cor$Faculties)
data_cor$Opportunities <- as.numeric(data_cor$Opportunities)
data_cor$Environment <- as.numeric(data_cor$Environment)
data_cor$Club <- as.numeric(data_cor$Club)
data_cor$Facilities <- as.numeric(data_cor$Facilities)
data_cor$Program <- as.numeric(data_cor$Program)

cor(data_cor)
####Create correlation table
corrplot(cor(data_cor), method="number", title = "Correlation")