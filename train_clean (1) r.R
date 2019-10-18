#install the required packages

setwd("C:\\Users\\User\\ALY 6040\\dataset")

# Libraries
library(stringr)
library(tm)

Needed <- c("stringr","tm","RWeka","corpus","knitr","fastDummies","mlr")
install.packages(Needed, dependencies = TRUE)


#import the dataset from the csv file 
movie_data_train<-read.csv("train.csv")
movie_data_test<-read.csv("test.csv")

#combine test and train to perform data cleaning
movie_data_test$Source<-"Test"
movie_data_train$Source<-"Train"

#add a column revenue to test data set and fill it with NA
#test data set would be required to test the final model to predict the revenue and thus revenue column is missing
#movie_data_test$revenue<-NA

comb_data<-movie_data_train
str(comb_data)


################################################################################################


# Genres
comb_data$genres <- as.character(comb_data$genres)

#1. Removing the id and name tags. Also, filtering out any punctuation or number.
#Adding the newly generated genres in newcolumn : P_genres

for(i in 1:nrow(comb_data)) {
  comb_data$P_genres[i] <- gsub(']',"",gsub("+(id)+","",gsub("[[{'':0-9},]|+(name)+", "", comb_data$genres[i])))
}

#2. Adding the text data to a corpus

new <- VCorpus(VectorSource((comb_data$P_genres)))
mt <- as.matrix(DocumentTermMatrix(new))

#3. Adding the list of genres to the main dataset :

comb_data <- cbind(comb_data,mt)


#PART B 
# Spoken Languages
# removing punctuation

corpus <- VCorpus(VectorSource(comb_data$spoken_languages))
corpus <- tm_map(corpus,removePunctuation)

# Creating a list of tags to remove
code <- c("en","es","ko","ar","ru","sv","sr","de","fr","la","it","nl","cn","ja","hi","cs","pt","de","am")
code_new <- NULL
for(i in 1:length(code)) {
  code_new[i] <- paste("+", "(", code[i], ")", "+",sep="")
}
code_new <- data.frame(code_new)

# Using gsub to filter tags “ iso__,  “ iso6391” and “name”

for(i in 1:length(corpus)) {
  corpus[[i]] <- gsub("iso__ ","",corpus[[i]])
  corpus[[i]] <- gsub("iso6391 ","",corpus[[i]])
  corpus[[i]] <- gsub("+(name)+","",corpus[[i]])
  
  # replacing language code
  for(j in 1:nrow(code_new)){
    corpus[[i]] <- gsub(code_new[j,],"",corpus[[i]])
  }
}

##################
corpus_1 <- tm_map(corpus, PlainTextDocument)
dtm3 <- DocumentTermMatrix(corpus_1)

nn <- as.matrix(dtm3)

# FInding most frequent spoken languages 
freq <- colSums(nn)
head(sort(freq,decreasing = TRUE),20)
colnames(nn)

# removing garbage ( Lot of garbage values picked up by document term matrix )

# Final data stored the comb_data

comb_data <- cbind(comb_data,nn)


###########################################################################################

#fill budget==0 with mean budget value
summary(comb_data$budget)

for (i in 1:nrow(comb_data)){
  if(comb_data$budget[i]==0){
    comb_data$budget[i]=mean(comb_data$budget)
  }
}



list_na <- colnames(comb_data)[ apply(comb_data, 2, anyNA) ]
list_na

#fill missing values as well as where runtime=0 with mean runtime
na_col <- as.matrix(comb_data[,which(colnames(comb_data) %in% list_na)])

mean <- mean(na_col[which(!is.na(na_col))])

mean_missing <- apply(comb_data[,which(colnames(comb_data) %in% list_na),drop=F],
                      2,
                      mean,na.rm=TRUE)
mean_missing



mean_runtime<-mean_missing[1]

for (i in 1:nrow(comb_data)){
  if(is.na(comb_data$runtime[i]) | comb_data$runtime[i]==0 ){
    comb_data$runtime[i]= mean_runtime
  }
}


#fill missing dates
#convert date into month, year and day

comb_data$release_date<-as.character(comb_data$release_date)
comb_data$release_date<-as.Date(comb_data$release_date, "%m/%d/%Y")

#extract month and create a month column
comb_data$month<-format(comb_data$release_date,format="%b")

#extract year and create new column year
comb_data$year<-format(comb_data$release_date,format="%Y")

#extract day and create another coumn day
comb_data$day<-format(comb_data$release_date,format="%d")

#get the weekday for each date
comb_data$weekday<-format(comb_data$release_date,format="%a")

#find mode for month, year, day, weekday
#create a function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Calculate the mode for weekday, day and month using the user function and impute the result in place of missing values.
result_weekday <- getmode(comb_data$weekday)
result_day<-getmode(comb_data$day)
result_month<-getmode(comb_data$month)

comb_data$year <- as.numeric(comb_data$year)
#calculate median for year and impute
result_year<-median(comb_data$year, na.rm = TRUE)

#impute missing weekday 
for (i in 1:nrow(comb_data)){
  if(is.na(comb_data$weekday[i])){
    comb_data$weekday[i]= result_weekday
  }
}

#impute missing months
for (i in 1:nrow(comb_data)){
  if(is.na(comb_data$month[i])){
    comb_data$month[i]= result_month
  }
}

#impute missing day
for (i in 1:nrow(comb_data)){
  if(is.na(comb_data$day[i])){
    comb_data$day[i]= result_day
  }
}

#impute missing year
for (i in 1:nrow(comb_data)){
  if(is.na(comb_data$year[i])){
    comb_data$year[i]= result_year
  }
}

#summary of the variables budget, runtime, weekday, day, month
summary(comb_data$budget)
summary(comb_data$runtime)

sort(table(comb_data$weekday), decreasing = TRUE)
sort(table(comb_data$day), decreasing = TRUE)
sort(table(comb_data$month), decreasing = TRUE)

#count the number of keywords for each movie

#extract each keyword from the list of characters and remove all the white spaces and special characters

comb_data$Keywords<-as.character(comb_data$Keywords)
for(i in 1:nrow(comb_data)) {
  comb_data$keyword_count[i] <- gsub(']',"",gsub("+(id)+","",gsub("[[{'':0-9},]|+(name)+", "", comb_data$Keywords[i])))
}

#count keywords
for(i in 1:nrow(comb_data)) {
  comb_data$keyword_count[i] <- str_count(comb_data$keyword_count[i], '\\w+')
}

#convert it into numeric
comb_data$keyword_count<-as.numeric(comb_data$keyword_count)

#impute mean number of keywords where count=0
for(i in 1:nrow(comb_data)) {
  if(comb_data$keyword_count[i]==0){
    comb_data$keyword_count[i] <- ceiling(mean(comb_data$keyword_count))
  } 
  
}

##########################################################################################################

#Production companies 

comb_data$production_companies<-as.character(comb_data$production_companies)

for(i in 1:nrow(comb_data)) {
  comb_data$production_companies[i] <- gsub(']', "", gsub('"', "", strsplit(substring(
    gsub("[[{'':0-9},] | +(name)+", "", comb_data$production_companies[i]), 10, 60
  ), "id")))
  
}

for(i in 1:nrow(comb_data)) {
  comb_data$production_companies[i] <- 
    gsub(",.*", "", comb_data$production_companies[i])} 

for(i in 1:nrow(comb_data)) {
  comb_data$production_companies[i] <- 
    str_sub(comb_data$production_companies[i],3,-3)} 

for(i in 1:nrow(comb_data)) {
  comb_data$production_companies[i] <- 
    str_replace(comb_data$production_companies[i],"aracter","NA,")}

for(i in 1:nrow(comb_data)) {
  comb_data$production_companies[i] <- 
    gsub(",.*", "", comb_data$production_companies[i])} 



#Production countries 
comb_data$production_countries<-as.character(comb_data$production_countries)

comb_data$production_countries <- as.character(comb_data$production_countries)

for(i in 1:nrow(comb_data)) {
  comb_data$production_countries[i] <- gsub(']', "", gsub('"', "", strsplit(substring(
    gsub("[[{'':0-9},] | +(name)+", "", comb_data$production_countries[i]), 26, 60
  ), "id")))
  
  
}
for(i in 1:nrow(comb_data)) {
  comb_data$production_countries[i] <- 
    gsub("\'.*", "", comb_data$production_countries[i])} 

for(i in 1:nrow(comb_data)) {
  comb_data$production_countries[i] <- 
    str_replace(comb_data$production_countries[i],"character","NA,")}

for(i in 1:nrow(comb_data)) {
  comb_data$production_countries[i] <- 
    gsub(",.*", "", comb_data$production_countries[i])} 


#generate derived variables
#create dummy variables for month and weekday
library(mlr)

dummy_month<-createDummyFeatures(comb_data$month)
comb_data<-cbind(comb_data,dummy_month)

dummy_weekday<-createDummyFeatures(comb_data$weekday)
comb_data<-cbind(comb_data,dummy_weekday)

#create a column for director name 
comb_data$crew1 <- gsub("Director of Photography","vv",comb_data$crew)



for(i in 1:nrow(comb_data)){
  pos <- regexpr("Director", comb_data$crew1[i])
  comb_data$p_crew[i] <- gsub("[',]","",substr(comb_data$crew1[i],(pos + 19),(pos + 38)))
}


comb_data$p_crew <- gsub("^pro","",comb_data$p_crew)
comb_data$p_crew <- gsub(" pr","",comb_data$p_crew)
comb_data$p_crew <- gsub(" p","",comb_data$p_crew)

for(i in 1:nrow(comb_data)){
  comb_data$p1_crew[i] <- gsub(" ","_",comb_data$p_crew[i])
}  

library("tm")
c <- VCorpus(VectorSource(comb_data$p1_crew))
d <- DocumentTermMatrix(c)
mt <- as.matrix(d)
freq <- colSums(mt)


comb_data <- cbind(comb_data,mt)



####################################################################3
#create a column for total cast in each movie (cast_count)
comb_data$cast<- as.character(comb_data$cast)

for(i in 1:nrow(comb_data)) {
  comb_data$cast_count[i] <- str_count(comb_data$cast[i],"name")
}

#replace cast_count=0 with mean cast_count
for(i in 1:nrow(comb_data)){
  if(comb_data$cast_count[i] == 0){
    comb_data$cast_count[i]<- ceiling(mean(comb_data$cast_count))
  }
}

#derive a column Budget by cast ratio
for(i in 1:nrow(comb_data)) {
  comb_data$budget_cast_ratio[i] <- comb_data$budget[i] / comb_data$cast_count[i]
  
}


#derive a column budget by runtime ratio
for(i in 1:nrow(comb_data)) {
  comb_data$budget_runtime_ratio[i] <- comb_data$budget[i] / comb_data$runtime[i]
  
}

# create a column crew count to get the number of crew for each movie
comb_data$crew <- as.character(comb_data$crew)

for (i in 1:nrow(comb_data)) {
  comb_data$crew_count[i] <- str_count(comb_data$crew[i], "name")
  
}

#replace crew_count=0 with mean crew count

for (i in 1:nrow(comb_data)) {
  if (comb_data$crew_count[i] == 0) {
    comb_data$crew_count[i] <- ceiling(mean(comb_data$crew_count))
  }
}

#derive a variable budget by crew ratio
comb_data$budget_crew_ratio <-
  (comb_data$budget / comb_data$crew_count)




#drop unwanted or derived columns 



up_comb_data<-comb_data[ , -which(names(comb_data) %in% c("ï..id","belongs_to_collection","homepage","imdb_id","original_title",
                                                          "overview","popularity", "poster_path","release_date","genres",
                                                          "spoken_languages","status","tagline","title","Keywords","cast",
                                                          "crew","P_genres","month","weekday","crew1","p_crew","p1_crew"))]

#split the comb data again into train and test
updated_train<-up_comb_data[up_comb_data$Source=="Train",]
#updated_test<-up_comb_data[up_comb_data$Source=="Test",]

#drop source from test and train
#updated_test<-updated_test[ , -which(names(updated_test) %in% c("Source"))]
updated_train<-updated_train[ , -which(names(updated_train) %in% c("Source"))]

#drop revenue from test
#updated_test<-updated_test[ , -which(names(updated_test) %in% c("revenue"))]

#write.csv(updated_test, file = "modified_test.csv")
write.csv(updated_train, file = "modified_train.csv")


#create visualization
#vis_genre<-as.data.frame(colSums(up_comb_data[,11:31]))
#colnames(vis_genre)<-c("Frequency")
#vis_genre$Genre<-rownames(vis_genre)

#par(mar=c(2,6,2,0.5)+.1)
#barplot(vis_genre$Frequency, horiz=TRUE,las=1,
#        names.arg=rownames(vis_genre))
#title("Genre Preference",adj=0)


#write.csv(up_comb_data, file = "Comb_data.csv")


#build linear regression model


#create dummy variable for production company


# To change
dummy_prod_company<-createDummyFeatures(updated_train$production_companies)

names(dummy_prod_company)
library(mlr)
#dummy_prod_country<-createDummyFeatures(pranav_train$production_countries)

#drop unwanted dummy variables
#############################
dummy_prod_company$V1<-NULL
dummy_prod_company$`???`<-NULL
dummy_prod_company$`????` <- NULL
dummy_prod_company$`??????` <- NULL
dummy_prod_company$`????????` <- NULL
dummy_prod_company$`???????????? �Lunapark�`<- NULL
dummy_prod_company$`?????????????`<- NULL
dummy_prod_company$`�ama??rhane`<- NULL
#############################

updated_train$production_companies<-NULL
updated_train$production_countries<-NULL

updated_train$month<-NULL
updated_train$weekday<-NULL

write.csv(updated_train,"test.csv")

#add dummy variables to the existing dataset
updated_pranav_train<-cbind(updated_train,dummy_prod_company)


#create file

write.csv(updated_pranav_train, file = "final_data_set.csv")


# Model 1

saturated_model<-lm(revenue~.,data = updated_pranav_train)
summary(saturated_model)


#create a table for p-value
pvalue<-as.data.frame(summary(saturated_model)$coefficient[,4])
att_name<-row.names(pvalue)
colnames(pvalue)<-c("Value")
pvalue<-cbind(pvalue,att_name)

library(randomForest)


#remove rows having value < 0.05
pvalue<- pvalue[pvalue$Value<=0.05,]

#remove special character
pvalue$up_att_name<- gsub("`", "", pvalue$att_name)

pvalue$up_att_name

#reduce the mnodel using backward selection method
#step <- step(saturated_model, direction="backward")
#step$anova # display results

#build a reduced model
#saturated_model<-lm(revenue~ budget + adventure + animation + drama + family + fiction + 
 #                     history + horror + romance + thriller + war + deutsch + latin + 
  #                    slovina + steven_spielberg_ + ridley_scotto + martin_scorsese_ + 
   #                   tim_burtonofi + crew_count + month + day + weekday + keyword_count + 
  #                    budget_cast_ratio + budget_runtime_ratio + cast_count,data = updated_pranav_train)
#summary(saturated_model)



#create a reduced model based on the attribues which have less than 0.05 P-value
reduced_pranav_train<- updated_pranav_train[ , names(updated_pranav_train) %in% pvalue$up_att_name]
dim#reduced_pranav_train<-cbind(reduced_pranav_train,x)

x<-as.data.frame(updated_pranav_train$revenue)

colnames(x)<-c("revenue")

reduced_pranav_train <- cbind(x,reduced_pranav_train)
#x$reduced_pranav_train$`updated_pranav_train$revenue`<-NULL

write.csv(reduced_pranav_train,"newmodel.csv")
newmodel <- read.csv(file.choose())

reduced_model<-lm(revenue~., data = newmodel)
summary(newmodel)

p_chart <- data.frame(summary(reduced_model)$coefficient[,4])
write.csv(p_chart,"p_value.csv")

coff_chart <- data.frame(summary(reduced_model)$coefficient[,1])
write.csv(coff_chart,"coff.csv")


Folds <- createFolds(reduced_pranav_train$revenue,k=10)


RMSE <- lapply(Folds, function(x) {
  train <- reduced_pranav_train[-x, ]
  test <- reduced_pranav_train[x, ]
  
  model <- lm(revenue ~ ., data = train)
  
  prediction <- predict(model, test)
  
  result <- RMSE(test$revenue,prediction)
  return(result)
}
)

mt <- data.matrix(RMSE)

mt[1,]

