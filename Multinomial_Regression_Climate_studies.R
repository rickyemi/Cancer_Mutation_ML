

############################################################################################################

library(dplyr)

# Step I : read data /import data 
data <- read.csv("C:/Users/rodeyemi/Downloads/pgdb_public_combined_inves (1).csv", stringsAsFactors=FALSE)

#Step II: Exploratory Data Analysis
#         identify the features/variables
#         Check for missing values/outliers 
#         Check for data types and statistical summary 
#         Determine if there's need for feature rescaling or reengineering


names(data)

sum(is.na.data.frame(data)) # or anyNA(data)

table(data$type) 

# feature reengineering /recoding 
data$type[data$type=='R1']<- 1
data$type[data$type=='R2']<-2
data$type[data$type=='R3']<-3
data$type[data$type=='other'] <- 4

utils::View(data$type)

####### SELECT FOR CLIMATE RELATED KEYWORDS 
library(sqldf)

data_climate <- sqldf("SELECT*
      FROM data
      WHERE abstract LIKE '%climate%'") 


data_globalwarming <- sqldf("SELECT*
                            FROM data
                            WHERE abstract LIKE '%global warming%'")

data_drought <- sqldf("SELECT*
                            FROM data
                            WHERE abstract LIKE '%drought%'")

data<-rbind(data_climate,data_drought,data_globalwarming)


# check for duplicates 

data[duplicated(data$award_id),]


utils::View(data)

library(sqldf)
##### select and check an example '1002649' for duplicates######


sqldf("SELECT award_id,Amount
      FROM data
      WHERE award_id == ' 1002649' ")

# number of duplicates 
dim(data[duplicated(data$award_id),])[1]


data<-data[!duplicated(data),]

###### reengineer for year from effective_date
data$eff_year<-substr(data$effective_date,start = 7,stop =10)


data_by_year_2013<-sqldf("SELECT *
      FROM data
      WHERE eff_year =='2013' ")

data_by_2014<-sqldf("SELECT *
      FROM data
      WHERE eff_year =='2014' ")

data_by_2015 <- sqldf("SELECT *
      FROM data
      WHERE eff_year =='2015' ")

data_by_year_2016<-sqldf("SELECT *
      FROM data
      WHERE eff_year =='2016' ")

data_by_year_2017<-sqldf("SELECT *
      FROM data
      WHERE eff_year =='2017' ")


data<- rbind(data_by_year_2013,data_by_2014,data_by_2015,
             data_by_year_2016,data_by_year_2017)

# check for duplicates 
sum(duplicated(data$award_id))

utils::View(data)


utils::View(data_climate)
             
             
# User defined function to remove digits from the abstracts

knife<-function(x){
  return(sub(pattern = '[[:digit:]]+','',x))
}

data$abstract<-sapply(data$abstract,knife)

# User defined function to convert all uppercase terms to lower case from the abstracts
sword<-function(y){
  return(sapply(strsplit(y, " "), length))
  
}
data$abstract<-sapply(data$abstract,tolower)
#pgdb$Word_count<-sapply(tolower(pgdb$abstract),sword)

#####################################################
# text mining /preprocessing 
library(tm)
text_knife <- function(document){
  documents <- Corpus(VectorSource(document)) # convert to corpus
  documents = tm_map(documents, content_transformer(tolower))
  documents = tm_map(documents, removePunctuation)
  documents = tm_map(documents, removeWords, stopwords("english"))
  documents = tm_map(documents, removeNumbers) 
  documents <- tm_map(documents, stripWhitespace)
  return(documents)
}


library(ngram)

text_dagger<-function(x){
  return(wordcount(sapply(x,text_knife)[[1]][1]))
}

data$abstract_word_count<-sapply(data[,8],text_dagger)

glimpse(data)




data$amount <- as.numeric(data$amount) # convert to numeric data. type
# discretize the amount class to 0-50K, 50K-500K and 500K+

data$amount_class <- cut(data$amount, breaks = c(-1,50000,500000,5000000,Inf),labels = c('3','2','1','0'))
utils::View(data)

data$effective_date <- as.Date(data$effective_date,format="%m/%d/%Y")
data$expiration_date <- as.Date(data$expiration_date,format="%m/%d/%Y")
data$num_investigators <- as.numeric(data$num_investigators)


write.csv(pgdb,'pgdb.csv')
names(pgdb)

data$type<-as.factor(data$type)

data$award_id <- as.character(data$award_id)



data_final <- data[,c(3,5,15,16)]

utils::View(data_final)

utils::View(data_final)


data_final$type<- as.factor(data_final$type)

glimpse(data_final)
utils::View(pgdb_final)


####################################################################################
# step III : Modeling 
#logsitic regression
#filter for T1 and T3

data_new<-filter(data_final,amount_class == 1|amount_class == 3)



# Binary reengineering T1 to 0 and T3 to 1 for logistic regression 

#data_new$amount_class[data_new$amount_class == 1] <- 0

#data_new$amount_class[data_new$amount_class == 3] <- 1

#data_new$amount_class[data_new$amount_class == 2] <- NULL

#data_new$amount_class[data_new$amount_class == 3] <- NULL

table(data_new$amount_class)

barplot(table(data_new$amount_class))

data_new$type_ref <- relevel(data_new$type,ref = '1')

summary(glm(amount_class~num_investigators + abstract_word_count + type_ref, family = 'binomial',data =data_new))



library(ggplot2)

ggplot(pgdb, aes(factor(type), fill = amount_class)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

##################################################################################

#MULTINOMIAL LOGISTIC REGRESSION

install.packages('vGAM')

library(VGAM)

table(data_final$amount_class) # spread of the amount Tiers (T0- T3)

table(data_final$type) # spread of uinversity classfication based on carnegie 

data_final$type_ref <- relevel(data_final$type,ref = '1') # other research organizations as reference
data_final$amount_class_ref <- relevel(data_final$amount_class,ref = '3') # Tier 3 as reference

model <- vglm(amount_class_ref~num_investigators+ abstract_word_count,
              family = 'multinomial', parallel = T,data=data_final)

names(data_final)




pgdb_final_log_set <-sqldf("SELECT*
     FROM 
     pgdb_final
     WHERE amount_class == 1 OR amount_class == 3")

library(dplyr)

pgdb_final_2 <- as.data.frame(filter(pgdb_final,amount_class == 1 |amount_class == 3))
table(pgdb_final_2$amount_class)
pgdb_final_2$amount_class_ref <- NULL
pgdb_final_2$type_ref <- NULL
pgdb_final_2$amount_class[pgdb_final_2$amount_class == 1] <- 0

pgdb_final_2$amount_class[pgdb_final_2$amount_class == 3] <- 1
#pgdb_final_log_set$amount_class[pgdb_final_log_set$amount_class== 1] <- 0

#pgdb_final_log_set$amount_class[pgdb_final_log_set$amount_class== 3] <- 1

glm(amount_class~num_investigators + Word_count + type, data = pgdb_final_2)

utils::View(pgdb_final_log_set)

pgdb_final_log_set$amount_class_ref<- NULL
pgdb_final_log_set$type_ref <- NULL

names(pgdb_final_log_set)

table(pgdb_final_log_set$amount_class)

table(pgdb_final_log_set$amount_class)

# check for amount less than $1B
pgdb_demo <-sqldf("SELECT amount
      FROM data
      WHERE amount < 10000000")
