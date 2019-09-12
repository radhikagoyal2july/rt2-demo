list.of.packages <- c("dplyr", "plyr", "data.table",
                      "ggplot2", "foreign", "lubridate", "readr")
new.packages <- list.of.packages[!(list.of.packages
                                   %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)})

help(tidyverse)

library(lubridate)
###############################1-FirstPackage to install#####################################
install.packages("tidyverse")
library(tidyverse)
##############################2-SecondPackage to install#####################################
install.packages("remotes")
remotes::install_github("wilkox/deidentifyr")
library (deidentifyr)
##############################3-Third Package to install#####################################

install.packages("digest")
library (digest)
##############################4-Fourth Package to install#####################################

install.packages("devtools")

if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("paulhendricks/anonymizer")
library(anonymizer)


###################################################################################################
#########################################ANONYMISER###########################################################
#                                                                                                   #
#                                                                                                   #
#                                                                                                   #
#######################Example of fake data with PII With Anomymiser##########################################


install.packages("generator") 
library(generator)
n <- 300
set.seed(1)

DatasetfakePII0<- 
  data.frame(name = r_full_names(n), 
             snn = r_national_identification_numbers(n), 
             dob = r_date_of_births(n), 
             email = r_email_addresses(n), 
             ip = r_ipv4_addresses(n), 
             phone = r_phone_numbers(n), 
             credit_card = r_credit_card_numbers(n), 
             lat = r_latitudes(n), 
             lon = r_longitudes(n), 
             stringsAsFactors = FALSE)
knitr::kable(DatasetfakePII0, format = "markdown")
View(DatasetfakePII0)
write.csv(DatasetfakePII0,file="C:/Users/wangsonne/Dropbox/BITSSDEIDENTIFICATION/DataFakePID/Datasetfake.csv")


######################################################################################################################
#################################################Detect data containing PII####################################################################
install.packages("detector")
library(detector)

install.packages("purrr")
library(purrr)

DatasetfakePII1<-DatasetfakePII0 %>% 
  detect %>% 
  knitr::kable(format = "markdown")


#################################################Removing Variables with PII not necessary for the analysis####################################
DatasetfakePII0
View(DatasetfakePII0)

str(DatasetfakePII0)
is.data.frame(DatasetfakePII0)

DatasetfakePIImethod1<-DatasetfakePII0
DatasetfakePIImethod1$name<-NULL


view (DatasetfakePIImethod1)
str(DatasetfakePIImethod1$dob)

###############################Extract the Year of Birth in the dob#########################
lubridate::year(DatasetfakePIImethod1$dob)
DatasetfakePIImethod1$yearbirth<-lubridate::year(DatasetfakePIImethod1$dob)
view(DatasetfakePIImethod1)
str(DatasetfakePIImethod1)
#########################################Masking Variables###############################
DatasetfakePIImethod1$agebirthcategory <- cut(DatasetfakePIImethod1$yearbirth,
                     breaks=c(-Inf, 1900, 1990, 2000, 2010, Inf),
                     labels=c("Very old","Old","Teenagers", "Young", "Very Young"))
View(DatasetfakePIImethod1)

 
###################Round a continuous Variable in R#########################################
#Hint: formula to compute the current age with the Lubridate Package
today <- as.Date(Sys.Date(), format='%d/%m/%y')
today


library(dplyr)


library(lubridate)

### Make use of the dplyr package and the mutate command in R################
DatasetfakePIImethod2 <- mutate(DatasetfakePIImethod1, datatoday =as.Date(Sys.Date(), format='%d/%m/%y'))
DatasetfakePIImethod3 <- mutate(DatasetfakePIImethod2, currentage=interval(dob,datatoday) / years(1))

View(DatasetfakePIImethod3)

summary(DatasetfakePIImethod3$currentage)

######################Round a continous Variable######################################
DatasetfakePIImethod4 <- mutate(DatasetfakePIImethod3, roundcurrentage=round(currentage, digits=0))
View(DatasetfakePIImethod4)
summary(DatasetfakePIImethod4$roundcurrentage)

DatasetfakePIImethod4$agecurrentroundcategory <- cut(DatasetfakePIImethod4$roundcurrentage,
                                              breaks=c(0, 5, 10,15,20, 25, 30,35,40,45,50,55,60,65,70,75,80,120),right=FALSE)
View(DatasetfakePIImethod4)

summary(DatasetfakePIImethod4$agecurrentroundcategory)
ftable(DatasetfakePIImethod4$agecurrentroundcategory)

###########Excercises: Try Top coding############################

library(generator)
n <- 300
set.seed(1)
fakePII0<- 
  data.frame(name = r_full_names(n), 
             snn = r_national_identification_numbers(n), 
             dob = r_date_of_births(n), 
             email = r_email_addresses(n), 
             ip = r_ipv4_addresses(n), 
             phone = r_phone_numbers(n), 
             credit_card = r_credit_card_numbers(n), 
             lat = r_latitudes(n), 
             lon = r_longitudes(n), 
             stringsAsFactors = FALSE)

fakePII0
fakePII0[] <- lapply(fakePII0,hash, .algo = "crc32")
fakePII0

library(anonymizer)
DatasetfakePII0 [] <- lapply(DatasetfakePII0 , anonymize, .algo = "crc32")
DatasetfakePIIid<-DatasetfakePII0  %>% 
  knitr::kable(format = "markdown")
DatasetfakePIIid
View(DatasetfakePII0)

####################tO better Understand the difference#################################
library(dplyr, warn.conflicts = FALSE)
library(anonymizer)
letters %>% head

letters %>% head %>% salt(.seed = 1)

letters %>% head %>% salt(.seed = 1) %>% unsalt(.seed = 1)

letters %>% head %>% hash(.algo = "crc32")

letters %>% head %>% salt(.seed = 1) %>% hash(.algo = "crc32")

letters %>% head %>% anonymize(.algo = "crc32", .seed = 1)



##########################################deidentyir####################################
library(deidentifyr)
library(generator)
n <- 300
set.seed(1)

NewDatasetfakePII0<- 
  data.frame(name = r_full_names(n), 
             snn = r_national_identification_numbers(n), 
             dob = r_date_of_births(n), 
             email = r_email_addresses(n), 
             ip = r_ipv4_addresses(n), 
             phone = r_phone_numbers(n), 
             credit_card = r_credit_card_numbers(n), 
             lat = r_latitudes(n), 
             lon = r_longitudes(n), 
             stringsAsFactors = FALSE)

NewDatasetfakePII0d <- deidentify(NewDatasetfakePII0, snn, dob, name,ip, phone)
View(NewDatasetfakePII0)
View(NewDatasetfakePII0d)






#########################################HOMEWORk###############################################################
#LEARN HOW TO WRITE YOUR OWN FUNCTION TO ANONYMISE YOUR DATASET INCLUDING THE DIGEST PACKAGE###################################




######################################OTHER 2011 August 23,2011, By richierocks######################################################
## A data.frame containing sensitive information
pacman0 <- data.frame(
  id                = LETTERS[c(1, 2, 2, 2, 3, 4, 5, 6)],
  first_name        = c("Steve", rep.int("Tony", 3), "Natasha", "Clint", "Bruce", "Thor"),
  last_name         = c("Rogers", rep.int("Stark", 3), "Romanoff", "Barton", "Banner", NA),
  alias             = c("Captain America", rep.int("Iron Man", 3), "Black Widow", 
                        "Hawkeye", "The Hulk", "Thor"),
  gender            = rep(c("Male", "Female", "Male"), times = c(4, 1, 3)),
  pacman_score      = c(round(rlnorm(7, 9, 3), -1), 3333360),
  stringsAsFactors  = FALSE
)

pacman<-pacman0

pacman
cols_to_anon <- c("first_name", "last_name", "alias")

pacmananom<-within(pacman0,
       {
         first_name <- NULL
         last_name <- NULL
         alias <- NULL
       })

pacmananom

simple_id <- function(data, cols_to_anon)
{
  to_anon <- subset(data, select = cols_to_anon)
  ids <- unname(apply(to_anon, 1, paste, collapse = ""))
  as.integer(factor(ids))
}
pacman0$method2_id <- simple_id(pacman, cols_to_anon)  

pacman0


anonymise <- function(data, cols_to_anon, algo = "sha256")
{
  if(!require(digest)) stop("digest package is required") 
  to_anon <- subset(data, select = cols_to_anon)
  unname(apply(to_anon, 1, digest, algo = algo))
}

pacman0$method3_id <- anonymise(pacman0, cols_to_anon)

pacman0

generate_salt <- function(data, cols_to_anon, n_chars = 20)
{                                                                
  index <- simple_id(data, cols_to_anon)
  n_indicies <- length(unique(index))   
  chars <- rawToChar(as.raw(32:126), multiple = TRUE)
  x <- replicate(n_indicies, paste(sample(chars, n_chars, replace = TRUE), collapse = ""))
  x[index]
}

pacman0$salt <- generate_salt(pacman0, cols_to_anon)
pacman0$method4_id <- anonymise(pacman0, c(cols_to_anon, "salt")) 
pacman0


##############Othersuggestions######################################
## A function to anonymise columns in 'colIDs' 
##    colIDs can be either column names or integer indices
anonymiseColumns <- function(df, colIDs) {
  id <- if(is.character(colIDs)) match(colIDs, names(df)) else colIDs
  for(id in colIDs) {
    prefix <- sample(LETTERS, 1)
    suffix <- as.character(as.numeric(as.factor(df[[id]])))
    df[[id]] <- paste(prefix, suffix, sep="")
  }
  names(df)[id] <- paste("V", id, sep="")
  df
}

## A data.frame containing sensitive information
df <- data.frame(
  name = rep(readLines(file.path(R.home("doc"), "AUTHORS"))[9:13], each=2),
  hiscore = runif(10, 99, 100),
  passwd = replicate(10, paste(sample(c(LETTERS, letters), 9), collapse="")))

df

coldeid<-c("name", "passwd")
df2 <- anonymiseColumns(df, c(1,3))
df
head(df, 3)
head(df2, 3)

pacman

df3 <- anonymiseColumns(pacman, c(2,3,4))
df3
head(df3, 3)
head(pacman, 3)




