#load libraries
adult_data <- read_csv("adult.data.txt")
adult_test <- read_csv("adult.test.txt")

#data transformation
new_adult_data <- adult_data %>%
  mutate(workclass = replace(workclass, 
                             workclass %in% c("Never-worked", 
                                              "Without-pay"), 
                             "No-work"),
         marital.status= replace(marital.status,
                                 marital.status %in% c("Married-civ-spouse",
                                                       "Married-AF-spouse",
                                                       "Married-spouse-absent"),
                                 "Married"),
         occupation = replace(occupation,
                              occupation %in% c("Armed-Forces",
                                                "Other-service",
                                                "Priv-house-serv"),
                              "Other-service"),
         native.country = replace(native.country,
                                  !(native.country %in% c("United-States",
                                                          "Mexico")),
                                  "Other-countries"),
         gain = as.character(
           cut(capital.gain, 
               c(-1,0.1,99999),
               labels = c("No", "Yes"))),
         loss = as.character(
           cut(capital.loss, 
               c(-1,0.1,5000), 
               labels = c("No", "Yes")))) %>% 
  select(c(1:3,5:10,13:17))

new_adult_test <- adult_test %>%
  mutate(workclass = replace(workclass, 
                             workclass %in% c("Never-worked", 
                                              "Without-pay"), 
                             "No-work"),
         marital.status= replace(marital.status,
                                 marital.status %in% c("Married-civ-spouse",
                                                       "Married-AF-spouse",
                                                       "Married-spouse-absent"),
                                 "Married"),
         occupation = replace(occupation,
                              occupation %in% c("Armed-Forces",
                                                "Other-service",
                                                "Priv-house-serv"),
                              "Other-service"),
         native.country = replace(native.country,
                                  !(native.country %in% c("United-States",
                                                          "Mexico")),
                                  "Other-countries"),
         gain = as.character(
           cut(capital.gain, 
               c(-1,0.1,99999),
               labels = c("No", "Yes"))),
         loss = as.character(
           cut(capital.loss, 
               c(-1,0.1,5000), 
               labels = c("No", "Yes")))) %>% 
  select(c(1:3,5:10,13:17))

nadata <- new_adult_data
nadata[nadata == "?"] <- NA
natest <- new_adult_test
natest[natest == "?"] <- NA
fact <- lapply(select_if(nadata, is.character), factor)
dati <- cbind(fact, select_if(nadata, is.integer))
dataimp <- kNN(dati)
dataimp <- dataimp[,1:14]
dataimp[, 1:10] <- lapply(dataimp[, 1:10], as.character)
lapply(select_if(dataimp, is.factor),is.character)
fact2 <- lapply(select_if(natest, is.character), factor)
dati2 <- cbind(fact2, select_if(natest, is.integer))
testimp <- kNN(dati2)
testimp <- testimp[,1:14]
testimp[, 1:10] <- lapply(testimp[, 1:10], as.character)