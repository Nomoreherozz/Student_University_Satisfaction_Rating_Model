##### Read the data
data = read.csv("E:/Data analysis/data/Rating_data_2.csv")
data

##### Group data into smaller class types
data[data == "Male"] <- 1
data[data == "Female"] <- 0
data[data == "Prefer not to say"] <- -1

data["Program"][data["Program"] == 1 | data["Program"] == 2] = 0
data["Program"][data["Program"] == 3 | data["Program"] == 4] = 1
data["Program"][data["Program"] == 5 | data["Program"] == 6] = 2

data["Facilities"][data["Facilities"] == 1 | data["Facilities"] == 2] = 0
data["Facilities"][data["Facilities"] == 3 | data["Facilities"] == 4] = 1
data["Facilities"][data["Facilities"] == 5 | data["Facilities"] == 6] = 2

data["Club"][data["Club"] == 1 | data["Club"] == 2] = 0
data["Club"][data["Club"] == 3 | data["Club"] == 4] = 1
data["Club"][data["Club"] == 5 | data["Club"] == 6] = 2

data["Environment"][data["Environment"] == 1 | data["Environment"] == 2] = 0
data["Environment"][data["Environment"] == 3 | data["Environment"] == 4] = 1
data["Environment"][data["Environment"] == 5 | data["Environment"] == 6] = 2

data["Opportunities"][data["Opportunities"] == 1 | data["Opportunities"] == 2] = 0
data["Opportunities"][data["Opportunities"] == 3 | data["Opportunities"] == 4] = 1
data["Opportunities"][data["Opportunities"] == 5 | data["Opportunities"] == 6] = 2

data["Faculties"][data["Faculties"] == 1 | data["Faculties"] == 2] = 0
data["Faculties"][data["Faculties"] == 3 | data["Faculties"] == 4] = 1
data["Faculties"][data["Faculties"] == 5 | data["Faculties"] == 6] = 2

##### Change Overall data to binary
data["Overall"][data["Overall"] == 1 | data["Overall"] == 2 | data["Overall"] == 3] = 0
data["Overall"][data["Overall"] == 4 | data["Overall"] == 5 | data["Overall"] == 6] = 1


##### Factorize
### DO NOT FACTORIZE THE VARIABLE IF ONLY DRAWING CORRELATION MATRIX
data$Sex <- as.factor(data$Sex)
data$Overall <- as.factor(data$Overall)
data$Faculties <- as.factor(data$Faculties)
data$Opportunities <- as.factor(data$Opportunities)
data$Environment <- as.factor(data$Environment)
data$Club <- as.factor(data$Club)
data$Facilities <- as.factor(data$Facilities)
data$Program <- as.factor(data$Program)

##### Summary the data types in data
data
str(data)

