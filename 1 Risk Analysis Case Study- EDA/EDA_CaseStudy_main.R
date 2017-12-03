#install.packages("lubridate")
####### Including the libraries ##############################
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(forcats)

####### cleaning the environment ##############################################################
rm(list = ls())

####### setting working directory #######################################################################
setwd("C:/Users/egargas/Google Drive/PGDDA_DDA1710439/Course 2 EDA/Module 6 Risk Analysis Case Study")

####### Importing loan csv file into R ######################################################################
loanDF <- read.csv("loan.csv")

# Checking if there are cells where NA or blanks are present
sum(sapply(loanDF, function(x)
  length(which(x == " "))))      # 210 values are blank values

sum(sapply(loanDF, function(x)
  length(which(x == ""))))     # 54101 values are present with space character only

# Updating importing function which directly converts blank and space values to NA
loanDF <- read.csv("loan.csv", na.strings = c("", " ", "NA"))

####### Data Cleaning###############################################
## Check for duplicate ids
sum(duplicated(loanDF$id)) # there are no duplicate loan ids

sum(duplicated(loanDF$member_id)) # there are no duplicate member ids

## It removes all the columns containing only NA as value for all the rows
index <- which(colSums(is.na(loanDF)) == nrow(loanDF))
loanDFImp <- loanDF[-index]

## Cleaning up other columns in the dataset
unique(loanDFImp$acc_now_delinq)   # contains only value 0 for all rows
unique(loanDFImp$delinq_amnt)    # contains only value 0 for all rows
unique(loanDFImp$pymnt_plan)  # contains only value "n" for all rows
unique(loanDFImp$initial_list_status)  # contains only value "f" for all rows
unique(loanDFImp$policy_code)  # contains only value 1 for all rows
unique(loanDFImp$application_type)  # contains only value "Individual" for all rows

# removing above columns as they have same value for all rows
loanDFImp <-
  loanDFImp[,-which(
    names(loanDFImp) %in% c(
      "acc_now_delinq",
      "delinq_amnt",
      "pymnt_plan",
      "initial_list_status",
      "policy_code",
      "application_type"
    )
  )]

## Removing percentage marks from interest rate
loanDFImp$int_rate <-
  str_replace(loanDFImp$int_rate,
              pattern = "[%]",
              replacement = "")

loanDFImp$revol_util <-
  str_replace(loanDFImp$revol_util,
              pattern = "[%]",
              replacement = "")

## Removing months from term column
loanDFImp$term <-
  str_replace(loanDFImp$term, pattern = " months", "")

## Removing + and years from emp_length and updating years for < 1 year
loanDFImp$emp_length <-
  str_replace(loanDFImp$emp_length, pattern = "< 1 year", "0")

loanDFImp$emp_length <-
  str_replace(loanDFImp$emp_length, pattern = "[+]", "")

loanDFImp$emp_length <-
  str_replace(loanDFImp$emp_length, pattern = " years", "")

loanDFImp$emp_length <-
  str_replace(loanDFImp$emp_length, pattern = " year", "")

table(loanDFImp$emp_length) ## checking values present in emp_length, it contains n/a

#Replacing n/a to NA
loanDFImp$emp_length[which(loanDFImp$emp_length == "n/a")] <- NA
table(loanDFImp$emp_length)   ## checking values present in emp_length

## Standarising loan_status, purpose, title column
loanDFImp$loan_status <- str_to_lower(loanDFImp$loan_status)
loanDFImp$purpose <- str_to_lower(loanDFImp$purpose)
loanDFImp$title <- str_to_lower(loanDFImp$title)

## Standarising verification_status field in the excel field
loanDFImp$verification_status <-
  str_replace(loanDFImp$verification_status, "Source Verified", "Verified")

## Writing the data to the file
write.csv(x = loanDFImp,
          file = "loanDFImp.csv",
          row.names = FALSE)

####### Filtering data to be analyzed#######################################
# We need to study charged off loans data to make an understanding of which type of
#customes default usually. So selecting data only for charged off customers
chargedOffLoans <-
  filter(loanDFImp, loan_status == "charged off") %>% arrange(desc(loan_amnt))

####### Univariate analysis ##################################
####### Checking the count of customers based on LOAN AMOUNT field###################
#Plotting the graph
ggplot(chargedOffLoans, aes(x = loan_amnt)) +
  geom_bar(fill = "blue") +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1,
    vjust = 0.5
  )) +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  scale_x_continuous(breaks = seq(0, 35000, 2000)) +
  labs(title = "Plotting Charged Off Loans Frequency w.r.t Loan Amount",
       x = "Loan Amounts", y = "Number of Charged Off Loans")
#### It shows that customers who take loan in round figures are more prone to be defaulters
####### Analysing count of customers based on HOME OWNERSHIP##################
#Plotting the graph
ggplot(chargedOffLoans, aes(fct_infreq(home_ownership))) +
  geom_bar(fill = "orange") +
  labs(title = "Plotting Charged Off Loans Frequency w.r.t Home Ownership",
       x = "Home Ownership", y = "Number of Charged Off Loans")

homeTypeWithMaxChargedOffCustomers <-
  chargedOffLoans %>%
  group_by(home_ownership) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts)) %>%
  head(2)

# Shows that a large number of Rent and Mortgage customers who have been charged off

####### Analyzing loan PURPOSE for the customers #################################
#Plotting the graph
ggplot(chargedOffLoans, aes(fct_infreq(purpose))) +
  geom_bar(fill = "pink") +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1,
    vjust = 0.5
  )) +
  scale_y_continuous(breaks = seq(0, 3000, 250)) +
  labs(title = "Plotting Charged Off Loans Frequency w.r.t Purpose",
       x = "Purpose of Loan", y = "Number of Charged Off Loans")

purposeWithMaxChargedOffCustomers <-
  chargedOffLoans %>%
  group_by(purpose) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts)) %>%
  head(5)
#### It shows that customers who take loan for debt_consolidation are major defaulters

####### Analyzing the count of charged off customers based on STATE field######
#Plotting the graph
ggplot(chargedOffLoans, aes(x = fct_infreq(addr_state))) +
  geom_bar(fill = "red") +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1,
    vjust = 0.5
  )) +
  scale_y_continuous(breaks = seq(0, 1200, 400)) +
  labs(title = "Plotting Count of Charged Off Loans w.r.t States",
       x = "States", y = "Number of Charged Off Loans")

topStatesWithMaxChargedOffCustomers <-
  chargedOffLoans %>%
  group_by(addr_state) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts)) %>%
  head(3)

#### It shows that CA, FL, NY are the top 3 states with maximum defaulters

####### Analyzing number of charged off customers per GRADE################
#Plotting the graph
ggplot(chargedOffLoans, aes(x = fct_infreq(grade))) +
  geom_bar(fill = "light cyan") +
  scale_y_continuous(breaks = seq(0, 1600, 200)) +
  labs(title = "Plotting Charged Off Loans Frequency w.r.t Grades",
       x = "Grades", y = "Number of Charged Off Loans")

# Checking the count of customers based on GRADE field
gradeWithMaxChargedOffCustomers <-
  chargedOffLoans %>%
  group_by(grade) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts)) %>%
  head(3)

#### It shows that top 3 grades for charged off laons are B, C, D

####### Analyzing number of charged off customers per EMPLOYEE LENGTH################
#Plotting the graph
ggplot(chargedOffLoans, aes(x = fct_infreq(emp_length))) +
  geom_bar(fill = "brown") +
  scale_y_continuous(breaks = seq(0, 1400, 200)) +
  labs(title = "Plotting Charged Off Loans Frequency w.r.t Employee Exp",
       x = "Employee Experience", y = "Number of Charged Off Loans")

# Checking the count of customers based on EMP LENGTH field
empLengthWithMaxChargedOffCustomers <-
  chargedOffLoans %>%
  group_by(emp_length) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts)) %>%
  head(5)
# It shows that customers with more than 10 years of experience are the major defaulters
# Others are 10, 0, 2, 3 ,4

####### Analyzing number of charged off customers  Earliest Credit Line ################
chargedOffLoans$creditLineYear = format(as.Date(
  paste("01-",
        chargedOffLoans$earliest_cr_line,
        sep = ""),
  format = "%d-%b-%y"
),
"%Y")

table(chargedOffLoans$creditLineYear)
# less than 1970 is giving values as 20xx. needs to be changed
# very few having numbers less than 1970 --128
chargedOffLoans$creditLineYear[chargedOffLoans$creditLineYear < 1970 |
                                 chargedOffLoans$creditLineYear > 2008] <- "<1970"
sort(unique(chargedOffLoans$creditLineYear))

ggplot(chargedOffLoans, aes(x = creditLineYear)) + geom_bar(fill = "blue") +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1,
    vjust = 0.5
  )) +
  labs(title = "Plotting Charged Off Loans Frequency w.r.t First Credit Line Year",
       x = "Credit Line Year", y = "Number of Charged Off Loans")

##There is spike in charged off loans for customers having
# first credit line year between 1994 to 2001

####### Analyzing average Annual Income for differnet loan bins################
loanDataWithBins <-
  loanDFImp %>%
  mutate(loanAmtBin = cut(
    loanDFImp$loan_amnt,
    10,
    include.lowest = TRUE,
    labels = c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10")
  )) %>%
  filter(loan_status != "current")



loanDataWithBinslayers <- loanDataWithBins %>%
  group_by(loanAmtBin, loan_status) %>%
  summarise(mean_ann_inc = median(annual_inc))

ggplot(loanDataWithBinslayers, aes(loanAmtBin, mean_ann_inc)) +
  geom_bar(
    stat = "identity",
    aes(fill = loan_status),
    position = position_dodge(0.3),
    alpha = 0.5
  ) +
  labs(title = "Plotting Mean Annual Salary w.r.t Loan amount Bins",
       x = "Loan Amount Bins", y = "Median Annual Salary")

## For higher loan amounts, difference between average income for charged off
#customers and full paid customers is quite high
########################### Multivariate analysis ##################################
####### Analyzing outliers for Annual Income of Charged off customers################
## Checking for the oultiers in the data

ggplot(chargedOffLoans,aes(grade,annual_inc))+geom_boxplot() + labs(title ="Analyzing Grade with Annual Income for Charged off customers" ,x = "Grade", y= "Annual Income")

income99Percentile <-
  quantile(chargedOffLoans$annual_inc, c(.99)) ## 205960

sum(chargedOffLoans$annual_inc > income99Percentile) ##57 customers have income more than 99 percentile

max(chargedOffLoans$annual_inc)  ##1.25 Million
## There are around 57 customers who have taken loan but have annual income more than 99% percentile of customers

####### Analyzing the assignment of interest rates as per loan grade #############
ggplot(loanDFImp[-(which(loanDFImp$loan_status == "current")), ], aes(x = as.integer(int_rate), fill = grade)) +
  geom_bar() +
  labs(title = "Plotting Loans Frequency w.r.t Interest and Grade",
       x = "Interest Rates", y = "Number of Charged Off Loans", fill = "Grade") +
  scale_y_continuous(breaks = seq(0, 4000, 200)) +
  facet_wrap( ~ loan_status)
ggplot(chargedOffLoans,aes(grade,as.numeric(int_rate),fill=grade))+geom_boxplot()+ylab("RoI")+
  labs(title = "Plotting Rate of Interest and Grade for Charged off customers",
       x = "Grade", y = "Rate of Interest")
  
#### It shows that as the grade of loan decreases as interest rate increases

####### Comparing Home STATE with PURPOSE for the count of charged off customers#####
stateWithPurposeAnalysis <-
  chargedOffLoans %>%
  filter(
    addr_state %in% topStatesWithMaxChargedOffCustomers$addr_state &
      purpose %in% purposeWithMaxChargedOffCustomers$purpose
  )

#Plotting the graph
ggplot(stateWithPurposeAnalysis, aes(x = fct_infreq(addr_state))) +
  geom_bar(aes(fill = purpose),
           alpha = 0.5,
           position = position_dodge(0.3)) +
  scale_y_continuous(breaks = seq(0, 550, 50)) +
  labs(title = "Plotting Charged Off Loans Frequency w.r.t State & Purpose",
       x = "States", y = "Number of Charged Off Loans", fill = "Purpose")

#checking the count
stateWithPurposeAnalysis %>%
  group_by(addr_state, purpose) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts))
### It shows that customers who take loan for debt consolidation
### and belong to CA, NY and FL are major defaulters.

####### Comparing STATE with HOME OWNERSHIP for the count of charged off customers####
stateWithOwnershipAnalysis <-
  chargedOffLoans %>%
  filter(
    addr_state %in% topStatesWithMaxChargedOffCustomers$addr_state &
      home_ownership %in% homeTypeWithMaxChargedOffCustomers$home_ownership
  )

#Plotting the graph
ggplot(stateWithOwnershipAnalysis, aes(x = fct_infreq(addr_state))) +
  geom_bar(aes(fill = home_ownership),
           alpha = 0.5,
           position = position_dodge(0.3)) +
  scale_y_continuous(breaks = seq(0, 750, 100)) +
  labs(title = "Plotting Charged Off Loans Frequency w.r.t State & Home Ownership",
       x = "States", y = "Number of Charged Off Loans", fill="Home Ownership")

# Checking the counts for addr_state and home_ownership
stateWithOwnershipAnalysis %>%
  group_by(addr_state, home_ownership) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts))

#### It shows the relation for charged off customers between Home Ownership
#### with Employee Length

####### Comparing EMP LENGTH with HOME OWNERSHIP for top 3 states customers#####
empLength_Ownership_State_Analysis <-
  chargedOffLoans %>%
  filter(
    emp_length %in% empLengthWithMaxChargedOffCustomers$emp_length &
      home_ownership %in% homeTypeWithMaxChargedOffCustomers$home_ownership &
      addr_state %in% topStatesWithMaxChargedOffCustomers$addr_state
  )

empLength_State_Analysis <- empLength_Ownership_State_Analysis %>%
  group_by(emp_length, addr_state) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts))

ggplot(empLength_Ownership_State_Analysis, aes(x = fct_infreq(emp_length))) +
  geom_bar(aes(fill = home_ownership),
           alpha = 0.5,
           position = position_dodge(0.3)) +
  labs(title = "Plotting Charged Off Loans Frequency w.r.t Emp Length, Home Ownersip & State",
       x = "Employee Length", y = "Number of Charged Off Loans", shape="State", fill="Home Ownership") +
  scale_y_continuous(breaks = seq(0, 750, 100)) +
    geom_point(data = empLength_State_Analysis, aes(x = emp_length, y = counts, shape = addr_state))
