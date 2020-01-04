rm(list = ls())
library(dplyr)
library(stringr)
library(validate)
library(lubridate)
library(generator)
library(skimr)

#---------------------------------------------------
# Functions
#---------------------------------------------------

isUpperCase <- function(names_lst){
  # Function to evaluate if a given list of values are uppercase
  # Args : list of strings (e.g. names)
  # Returns : list of TRUE/FALSE/NA corresponding to each string 
  result_lst <- c()
  for (i in 1:length(names_lst)){
     #i = 1
    # Remove punctuations
    names_lst[i] <- gsub('[[:punct:] ]+','', names_lst[i])
    # Check if all characters are upper case
    result_lst[i]<- ifelse(regexpr('^[[:upper:]]+$', names_lst[i])[1] == 1, T,F)
  }
  return(result_lst)
}

isValidEmail <- function(email_lst) {
  # Function to check if email addresses in a given list of emails has the 
  #   correct format
  # Args : list of email addresses
  # returns : list of TRUE/FALSE/NA corresponding to each email 
  result_lst <- c()
  for (i in 1:length(email_lst)){
    result_lst[i] <- ifelse(is.na(email_lst[i]), NA, 
                            grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>"
                                  , as.character(email_lst[i]), ignore.case=TRUE
                                  ))
  }
  return(result_lst)
}


isDuplicated<- function(str_lst){
  # Function to check if each value in a list contain a duplicate value
  # Args : list of values
  # Returns : list of TRUE/FALSE/NA corresponding to each vaue on if it contains
  #   duplicates or not
  str_dup <- str_lst[duplicated(str_lst)]
  result_lst<- c()
  for(i in 1:length(str_lst)){
    result_lst[i]<- case_when(is.na(str_lst[i]) ~ NA,
                              (str_lst[i] %in% str_dup) ~F,
                              TRUE ~ T)
  }
  return(result_lst)
}


isValidOver18 <- function(dateofbirth.lst, over18.lst){
  # Function to assess data in a list of over18 is correct given the 
  #   corresponding date of birth
  # Args : lists of date of birth and corresponding over18 
  # returns : list of TRUE/FALSE/NA corresponding to each vaue of the over18 
  result_lst <- c()
  for (i in 1:length(over18_lst)){
    # Check if the cauculation is correct
    result_1 <- case_when(is.na(trunc(time_length(
                            interval(ymd(dateofbirth_lst[i]),Sys.Date()), 
                                                  "year")))== T ~ NA_character_,
                          trunc(time_length(
                            interval(ymd(dateofbirth_lst[i]),Sys.Date()),
                                                  "year")) >= 18 ~ "1",
                          TRUE ~ "0" ) 
    result_lst[i] <- case_when(is.na(result_1) & is.na(over18_lst[i]) ~ NA,
                               is.na(result.1) & !is.na(over18_lst[i]) ~ F,
                               !is.na(result.1) & is.na(over18_lst[i]) ~ F,
                               result_1 == over18.lst[i] ~ T,
                               TRUE ~ F )
    
  }
  
  
  return(result.lst)
}

#----------------------------------------------------
# Data Preparation
# POI data (id, firstname, lastname, dateofbirth, email, phone, gender)
#   are generated using 'generator' package
# Then data in each field is altered to include null values and erronous data 
#----------------------------------------------------

id <- c(r_national_identification_numbers(950), rep(NA,50)) %>% 
  str_replace_all("-","") 
firstname <- r_full_names(1000) %>% word(1)%>% toupper()
lastname <- r_full_names(1000) %>% word(-1) %>% toupper()
dateofbirth <- r_date_of_births(1000, start = as.Date("1995-01-01"), 
                                end = Sys.Date()) 
email <- r_email_addresses(1000)
phone <- r_phone_numbers(1000, use_hyphens = FALSE, use_parentheses = FALSE,
                         use_spaces = T)
gender <- c(rep("M", 750), rep("F", 250))


dat <- cbind.data.frame(id, firstname, lastname, dateofbirth, email, phone, 
                        gender, stringsAsFactors = F) 
# ---------------------------------------------
# Data Alteration for testing
#-----------------------------------------------
# id
#---------------------
# Replace the 1st character of ID of the first 10 records with "A"  
dat$id[1:10] <-sub("[[:digit:]]", "A", dat$id[1:10])
# Replace 1st character of next 10 records with "." (punctuation) 
dat$id[11:20] <-sub("[[:digit:]]", ".", dat$id[11:20])
# Delete the first digit of the next 10 rows 
dat$id[21:30] <-sub("[[:digit:]]", "", dat$id[21:30])
# Add an additional digits at the starts of the id of the next 10 rows 
#     (make them 10 digit ids)
dat$id[31:40] <-sub("[[:digit:]]", "00", dat$id[31:40])
# Create Duplicated IDs in the next 10 rows
dat$id[41:50] <- dat$id[51:60]
#-----------------------
# first name
#-------------
# Replace the 1st character of the 1st ten records with a digit
dat$firstname[1:10] <-sub("[[:alpha:]]", "0", dat$firstname[1:10])
# Replace the 1st character of the next ten recordswith lowercase
dat <- dat %>% mutate_at(vars(firstname),funs(ifelse(between(row_number(),11,20)
                                                     , tolower(.), .)))
# Make the first name Null for nest 10 rows
dat <- dat %>% mutate_at(vars(firstname),funs(ifelse(between(row_number(),21,30)
                                                     , NA, .)))
head(dat)
#----------------
# Date of Birth
#----------------
# Replace the "-" with "/" of the 1st ten records with a letter
dat <- dat %>%mutate_at(vars(dateofbirth),
                        funs(ifelse(between(row_number(),1,10), 
                                    str_replace_all(as.character(.),"-","/"),
                                    as.character(.))))
# Replace the 1st character with "" in the next 10 rows
dat <- dat %>% mutate_at(vars(dateofbirth),
                         funs(ifelse(between(row_number(),11,20),
                                     str_replace(.,str_sub(.,1,1),""), .)))
# Replace the 1st character with "A" in the next 10 rows
dat <- dat %>% mutate_at(vars(dateofbirth), 
                         funs(ifelse(between(row_number(),21,30), 
                                     str_replace(.,str_sub(.,1,1),"A"), .)))
# Make the next 10 records of Date the Birthdate  NA
dat <- dat %>% mutate_at(vars(dateofbirth), 
                         funs(ifelse(between(row_number(),31,40), NA, .)))
# Make the birth year of next 10 rows = 2005 so that they are definelty below 18
dat <- dat %>% mutate_at(vars(dateofbirth), 
                         funs(ifelse(between(row_number(),41,50), 
                                     str_replace(.,str_sub(.,1,4),"2005"), .)))
# Create Duplicate DateofBirth in next 10 rows
dat$dateofbirth[51:60] <- dat$dateofbirth[61:70]
#-----------------
# Email
#-----------------
# Make first 10 rows NA
dat <- dat %>% mutate_at(vars(email), 
                         funs(ifelse(between(row_number(),1,10), NA,.)))
# Email should have "@". Replace '@' of next 10 rows with ""
dat <- dat %>% mutate_at(vars(email), 
                         funs(ifelse(between(row_number(),11,20), 
                                     str_replace(.,"@",""), .)))
# An email should have only 1 "@" if there are 2 "@"s that is an invalid email
dat <- dat %>% mutate_at(vars(email), 
                         funs(ifelse(between(row_number(),21,30), 
                                     str_replace(.,"\\.","@"), .)))
# email should have "." - replace "." with "" in the next 10 rows
dat <- dat %>% mutate_at(vars(email), 
                         funs(ifelse(between(row_number(),31,40), 
                                     str_replace(.,"\\.",""), .)))
# email should not have a space. add space before @ in next 10 rows
dat <- dat %>% mutate_at(vars(email), 
                         funs(ifelse(between(row_number(),41,50), 
                                     str_replace(.,"@"," @"), .)))
# email should be unique. Create duplicated emails in the next 10 rows
dat$email[51:60] <- dat$email[61:70]
#--------------------
# phone
#--------------------
# Make first 10 rows NA
dat <- dat %>% mutate_at(vars(phone), 
                         funs(ifelse(between(row_number(),1,10), NA, .)))
# Replace 1st digit with a character in the next 10 rows
dat <- dat %>% mutate_at(vars(phone), 
                         funs(ifelse(between(row_number(),11,20), 
                                     str_replace(.,str_sub(.,1,1),"A"), .)))
# Add an extra digit to next 10 rows
dat <- dat %>% mutate_at(vars(phone), 
                         funs(ifelse(
                           between(row_number(),21,30), paste0('1',.), .)))
# Delete one digit in next 10 rows
dat <- dat %>% mutate_at(vars(phone), 
                         funs(ifelse(
                           between(row_number(),31,40), str_sub(.,2,nchar(.)),.)
                           ))
# Add '-' instead of " " beween numbers
dat <- dat %>% mutate_at(vars(phone),
                         funs(ifelse(
                           between(row_number(),41,50),str_replace(.," ","-"),.)
                           ))
# phone should be unique. Create duplicated emails in the next 10 rows
dat$phone[51:60] <- dat$phone[61:70]
#---------------------
# Gender
#----------------------
# Make first 10 rows NA
dat <- dat %>% mutate_at(vars(gender), 
                         funs(ifelse(between(row_number(),1,10),NA, .)))
# Gender should be M or F , replace M with M in the next 10 rows instead
dat <- dat %>% mutate_at(vars(gender),
                         funs(ifelse(between(row_number(),11,20), 
                                     str_replace(.,"M", "Male"), .)))
#------------------------------------------
# Calculate over 18 and credit card number
#------------------------------------------
dat <- dat %>% mutate(over18 = case_when(
  is.na(trunc(time_length(interval(ymd(dateofbirth),Sys.Date()), "year")))== T 
                                                                ~ NA_character_,
  trunc(time_length(interval(ymd(dateofbirth),Sys.Date()), "year")) >= 18 ~ "1",
                                                                    TRUE ~ "0"))

#------------------------------------------
# Alter over 18 for testing
#------------------------------------------
# Make the 1st 10 rows NA regardless the birthdate is valid or not
dat <- dat %>% mutate_at(vars(over18), 
                         funs(ifelse(between(row_number(),1,10), NA, .)))
# Over18 is a logical column where only NA, 0 or 1 are possible.
# Introduce 2 and "A" to fail this rule
dat <- dat %>% mutate_at(vars(over18), 
                         funs(ifelse(between(row_number(),11,20), "2", .)))
dat <- dat %>% mutate_at(vars(over18),
                         funs(ifelse(between(row_number(),21,30), "A", .)))
# Introduce "1" to the next 10 rows regardless of the birthday to make the 
#   calculation incorrect, the 10 rows whose birthdate is NA is skipped
dat <- dat %>% mutate_at(vars(over18), 
                         funs(ifelse(between(row_number(),41,50), "1", .)))

#----------------------------------------------------------------
# Data Validation
#----------------------------------------------------------------
labels.lst <-c(#"No empty rows"
  #, "Column names are correct"
    "id - consists only 9 digits"
  , "id - unique"
  # Check for columns that should contain only letters
  , "firstname - contains no digits"
  , "lastname - contains no digits"
  , "firstname - Uppercase"
  , "lastname - Uppercase"
  , "birthdate - in YYYY-mm-dd format"
  , "email - in valid format"
  , "email - is unique"
  , "phone - in correct format (XXX XXX XXXX)"
  , "phone - is unique"
  , "gender - either M or F"
  , "over18 - valid values 1,0, NA"
  , "over18 - calculation correct"

) 


rules.lst <- c(
  # id
  "ifelse(!is.na(dat$id),(nchar(dat$id)== 9 & grepl('[0-9]{9}', dat$id)),NA)== T"
  , "isDuplicated(dat$id)==T"
  # firstname
  , "ifelse(!is.na(dat$firstname), grepl('[^0-9]', dat$firstname), NA) == T"
  , "isUpperCase(dat$firstname)==T"
  # lastname
  , "ifelse(!is.na(dat$lastname), grepl('[^0-9]', dat$lastname), NA) == T"
  , "isUpperCase(dat$lastname)==T"
  # Birthdate
  , "ifelse(!is.na(dat$dateofbirth), 
            nchar(dat$dateofbirth)==10 & grepl('[0-9]{4}[-][0-9]{2}[-][0-9]{2}',
            dat$dateofbirth), NA)==T"
  # Email
  , "isValidEmail(dat$email)==T"
  , "isDuplicated(dat$email)==T"
  # Phone Number
  , "ifelse(!is.na(dat$phone), 
            (grepl('[0-9]{3}[ ][0-9]{3}[ ][0-9]{4}',dat$phone) &
                                          nchar(dat$phone) == 12), NA)==T"
  , "isDuplicated(dat$phone)==T"
  # Gender
  , "ifelse(!is.na(dat$gender), dat$gender %in% c('M', 'F'), NA)==T"
  # Over18
  , "ifelse(!is.na(dat$over18), dat$over18 %in% c('1', '0'), NA)==T"
  , "isValidOver18(dat$dateofbirth, dat$over18)==T"
) 

df <- data.frame(label = labels.lst, rule = rules.lst)
v <- validator(.data = df)
cf <- confront(dat,v)
quality <- as.data.frame(summary(cf))
measure <- as.data.frame(v)
result_validation <- (merge(quality,measure)) %>% 
  select(label, items, passes, fails, nNA, error, warning)

# Identify errors in the rules set
result_validation_error <- result_validation %>% filter(error == T)
if(nrow(result_validation_error) != 0) {
  print("WARNING: Following data validation rules contain errors")
  print(result_validation_error$label)
} else {
  print("INFO: No errors in the data validation rules")
  # Data that fails validation are pulled out only if there are no errors in the
  # data validation rules
  fail_vals <- data.frame(values(cf)[1])
  fail_vals <- as.matrix(fail_vals)
  fail_vals<- as.data.frame(which(fail_vals==0, arr.ind=TRUE))
  fail_vals <- mutate(fail_vals, label = labels_lst[fail_vals$col +1])%>% 
    select(-col) %>%
    mutate(id = dat[fail_vals$row, 1])
  vals <- c()  
  for (i in 1:nrow(fail_vals)){
     vals[i] <- dat[fail_vals$row[i], 
                            grep(str_split(fail_vals$label[i], " - ")[[1]][1],
                                 names(dat))]
  }
  fail_vals <- cbind(fail_vals,vals)
}


