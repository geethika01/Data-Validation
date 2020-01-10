#
# Title: Validation of tabular dataset using the Validate package
# Description: This script generates a POI dataset and infuse teh data with some
#             erroneous data vaules. Then it validate the data types, formats and
#             business rules accordeing to a predefined rules set.
# Outputs: (a) Summary of issue counts 
#          (b) Individual data issues
# Written by: Geethika Wijewardena
# Date: 20 Jan 2020
#-------------------------------------------------------------------------------
#---------------------------------------------------
# Setup
#---------------------------------------------------
rm(list = ls())
library(dplyr)
library(stringr)
library(validate)
library(lubridate)
library(generator)

#---------------------------------------------------
# Functions
#---------------------------------------------------
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

isValidEmail <- function(email) {
  # Function to check if email addresses in a given list of emails has the 
  #   correct format
  # Args : list of email addresses
  # returns : list of TRUE/FALSE/NA corresponding to each email 

  result <- ifelse(is.na(email), NA, 
                  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>"
                      , as.character(email), ignore.case=TRUE))
  
  return(result)
}

isValidEmailList <- function(emaillist){
  # Function that evaluates each email in a given list is valid using the 
  # isValidEmail()
  # Args: list of emails
  # Returns: logical vector 
  result_lst <- c()
  for(i in 1: length(emaillist)){
    result_lst[i] <- isValidEmail(emaillist[i])
  }
  return(result_lst)
}


isValidDOB <- function(dob){
  # Assess if the given date of birth is valid, i.e. if its a valid date in yyyy-mm-dd
  # format and less than the current date
  # Args: character or null value
  # returns: TRUE of FALSE
  
  dob_date <- ymd(dob)
  dob_valid <- case_when(is.na(dob_date)== T ~ F,
                         dob_date > Sys.Date() ~ F,
                         (nchar(dob)==10 & grepl('[0-9]{4}[-][0-9]{2}[-][0-9]{2}',dob))== F ~ F,
                         TRUE ~ T)
  
  return(dob_valid)
}

isValidDOBList <- function(doblist){
  # Function that evaluates each dateofbirth in a given list is valid using the 
  # isValidDOB()
  # Args: list of dates
  # Returns: logical vector
  result_lst <- c()
  for(i in 1: length(doblist)){
    result_lst[i] <- ifelse(is.na(doblist[i]), NA,isValidDOB(doblist[i]))
  }
  return(result_lst)
}

isValidOver18 <- function(dob, over18){
  # Function to calculate if a person is over 18 years, given the DOB and compare 
  # it with the given value
  # Args: DOB, vaule for over18 field
  # Returns: if calculated value and given value are the same => T else F

  # Check if the DOB is valid  
  dob_valid <- isValidDOB(dob)
  #dob_valid
  # Check if the calculation is correct
  num_years <- NA
  if (unlist(dob_valid) == T) {
    num_years <- trunc(time_length(interval(ymd(dob),Sys.Date()), "year"))
  } 
  
  # For over18 to be valid, 
  # a) dob should be valid
  # b) if dob is valid, then check for number of years
  # c) now check for over18
  #    if DOB is invalid, over18 = NA
  #    if dob is valid, 
  #    if number of years > 18, over18 = 1 
  #    if number of years < 18 over18 = 0
  
  over18_calc <- case_when(dob_valid == F ~ NA_character_,
                           dob_valid == T & num_years > 18 ~ "1",
                           dob_valid == T & num_years < 18 ~ "0")
  valid_over18 <- case_when(is.na(over18_calc) & is.na(over18) ~ T,
                            over18_calc == over18 ~ T,
                            TRUE ~ F)
  
  return(valid_over18)
}

isValidover18List <- function(doblst,over18lst){
  # Function to assess data in a list of over18 is correct given the 
  #   corresponding date of birth
  # Args : lists of date of birth and corresponding over18 
  # returns : list of TRUE/FALSE/NA corresponding to each vaue of the over18 
  result_lst <- c()
  for(i in 1: length(over18lst)){
    result_lst[i] <- isValidOver18(doblst[i],over18lst[i])
  }
  return(result_lst)
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
# Infusion of data with issues
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

# Firstname - lowercase
dat <- dat %>% mutate_at(vars(firstname),funs(ifelse(between(row_number(),11,20)
                                                     , tolower(.), .)))
# Make the first name Null for next 10 rows
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
# Make the birth year of next 10 rows = 2015 so that they are definelty below 18
dat <- dat %>% mutate_at(vars(dateofbirth), 
                         funs(ifelse(between(row_number(),41,50), 
                                     str_replace(.,str_sub(.,1,4),"2015"), .)))
# Make the year of the next 10 records > year of the current date such that the 
# dateOfBirth is invalid
dob_yr_change_dat <- dat$dateofbirth[51:60]
year_current <- year(Sys.Date())

for (i in 1:10){
  alt_year <- as.numeric(year_current) + i
  dob_yr_change_dat[i] <- paste0(as.character(alt_year),
                                 str_sub(dob_yr_change_dat[i],5,10))
}
dat$dateofbirth[51:60] <- dob_yr_change_dat

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

#----------------------
# Calculate over 18 
#----------------------
dat <- dat %>% mutate(over18 = case_when(
  is.na(trunc(time_length(interval(ymd(dateofbirth),Sys.Date()), "year")))== T 
                                                                ~ NA_character_,
  trunc(time_length(interval(ymd(dateofbirth),Sys.Date()), "year")) >= 18 ~ "1",
                                                                    TRUE ~ "0"))

#------------------------------------------
# Alter over 18 for testing
#------------------------------------------
# dateofbirth of the first 40 rows is not valid. Thus we are not going to alter 
# the over18 field, but going to see what returns from the calculation
# Introduce "1" to the next 10 rows. Year of these roes is 2015. 
# Thus they should be < 18. Therefore ass data to make the calculation incorrect.

dat <- dat %>% mutate_at(vars(over18), 
                         funs(ifelse(between(row_number(),41,50), "1", .)))

# Make the 1st 10 rows NA regardless the birthdate is valid or not
dat <- dat %>% mutate_at(vars(over18), 
                         funs(ifelse(between(row_number(),51,60), NA, .)))
# Over18 is a logical column where only NA, 0 or 1 are possible.
# Introduce 2 and "A" to fail this rule
dat <- dat %>% mutate_at(vars(over18), 
                         funs(ifelse(between(row_number(),61,70), "2", .)))
dat <- dat %>% mutate_at(vars(over18),
                         funs(ifelse(between(row_number(),71,80), "A", .)))


#----------------------------------------------------------------
# Data Validation
#----------------------------------------------------------------

labels_lst <-c(
    "id - consists only 9 digits"
  , "id - unique"
  , "firstname - contains no digits"
  , "firstname - Uppercase"
  , "lastname - contains no digits"
  , "lastname - Uppercase"
  , "dateofbirth - ia a valid date in YYYY-mm-dd format and less than current date"
  , "email - in valid format"
  , "email - is unique"
  , "phone - in correct format (XXX XXX XXXX)"
  , "phone - is unique"
  , "gender - either M or F"
  , "over18 - valid values 1,0, NA and calculation is correct"
) 

rules_lst <- c(
  # id
  "ifelse(!is.na(dat$id),(nchar(dat$id)== 9 & grepl('[0-9]{9}', dat$id)),NA)== T"
  , "isDuplicated(dat$id)==T"
  # firstname
  , "ifelse(!is.na(dat$firstname), grepl('\\\\d', dat$firstname)==F, NA) == T"
  , "isUpperCase(dat$firstname)==T"
  # lastname
  , "ifelse(!is.na(dat$lastname), grepl('\\\\d', dat$lastname)==F, NA) == T"
  , "isUpperCase(dat$lastname)==T"
  # dateofbirth
  , "isValidDOBList(dat$dateofbirth)==T"
  # email
  , "isValidEmailList(dat$email)==T"
  , "isDuplicated(dat$email)==T"
  # Phone Number
  , "ifelse(!is.na(dat$phone), 
            (grepl('[0-9]{3}[ ][0-9]{3}[ ][0-9]{4}',dat$phone) &
                                          nchar(dat$phone) == 12), NA)==T"
  , "isDuplicated(dat$phone)==T"
  # gender
  , "ifelse(!is.na(dat$gender), dat$gender %in% c('M', 'F'), NA)==T"
  # over18
  , "isValidover18List(dat$dateofbirth,dat$over18)==T"
  
) 

# Data validation and generate summary
df <- data.frame(label = labels_lst, rule = rules_lst)
v <- validator(.data = df)
cf <- confront(dat,v)
quality <- as.data.frame(summary(cf))
measure <- as.data.frame(v)
result_validation <- (merge(quality,measure)) %>% 
  select(label, items, passes, fails, nNA, error, warning)

write.csv(result_validation, "Data validation summary.csv", row.names = F)

# Identify errors in the rules set
result_validation_error <- result_validation %>% filter(error == T)
if(nrow(result_validation_error) != 0) {
  print("WARNING: Following data validation rules contain errors")
  print(result_validation_error$label)
} else {
  print("INFO: No errors in the data validation rules")
  # Data that fails validation are pulled out only if there are no errors in the
  # data validation rules
  fail_vals <- data.frame(values(cf))
  #names(fail_vals) <- labels_lst
  fail_vals <- as.matrix(fail_vals)
  fail_vals<- as.data.frame(which(fail_vals==0, arr.ind=TRUE))
  fail_vals <- mutate(fail_vals, label = labels_lst[fail_vals$col])%>% 
    select(-col) %>%
    mutate(id = dat[fail_vals$row, 1])
  vals <- c()  
  for (i in 1:nrow(fail_vals)){
    #i = 335
    
     vals[i] <- dat[fail_vals$row[i],
                            str_split(fail_vals$label[i], " - ")[[1]][1]]
    # print(fail_vals$row[i])
    # print(str_split(fail_vals$label[i], " - ")[[1]][1])
                        
  }
  fail_vals <- cbind(fail_vals,vals)
  write.csv(fail_vals, "Individual data with issues.csv", row.names = F)
}

#------------------------------------
# END
#------------------------------------


