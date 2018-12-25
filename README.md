# GramenerCaseStudy
To identify the risky loan applicants, so that such loans can be reduced thereby cutting down the amount of credit loss. Identification of such applicants using EDA is the aim of this case study.  

------------------------------------------------------
###   Project     : Gramener Case Study                                                     
###   Description : To identify the risky loan applicants, so that such loans can be reduced 
###                 thereby cutting down the amount of credit loss. Identification of such  
###                 applicants using EDA is the aim of this case study.                     
###   Date        : 30-Sept-2018                                                             
###   Author      : 1. Bhagyashree Barhate                                                   
###                 2. Deepankar Kotnala                                                     
###                 3. Gautami Ramesh Havele                                                 
###                 4. Rohit Saini                                                           
------------------------------------------------------


## Installing and Loading required libraries                       

setwd("D:/iiitB/Module 2- Statistics and Exploratory Data Analytics/LOAN Case Study")

### Clearing the previously loaded objects. 
remove(list = ls())
### Suppressing warnings
options(warn = -1)

### Install the required packages and load libraries:

### Uncomment these three lines if the packages are not installed.
### install.packages("dplyr")
### install.packages("ggplot2")
### install.packages("lubridate")
### install.packages("cowplot")
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(scales)
library(corrplot)
library(formattable)

## Importing and Exploring the Data                

### Import the data from input file

loan <- read.csv("loan.csv")

### Having a look at the data

dim(loan)

glimpse(loan)

summary(loan)

#Checking for duplicates
length(unique(loan[,1])) != length(loan[,1])

### FALSE
### 39717 IDs matches with 39717 total observations. So there are no duplicates.


## Data Cleaning 

### Removing the columns having all NA values.
loan <- loan[,which(colSums(is.na(loan)) != nrow(loan))]

### Removing columns having more than 50% of the values as NA
table(colSums(is.na(loan)))

### There is 1 column having 36931 NA values, 1 column having 25682 NA values
### With so many NA values,the columns are kind of useless to do any kind of analysis and 
### hence should be removed from our Data Frame.

loan <- loan[,!names(loan) %in% names(which(colMeans(is.na(loan)) > 0.5))]
### Found the mean of NAs in each column, and filtered out those having more than 0.5 (or 50%) NAs
### Then took the names of those columns and just removed them from the main Data Frame

### Removing rows which are having all NA values
loan <-loan[rowSums(is.na(loan)) != ncol(loan),]

### Removing columns having repeatitive values. 
### Columns which have just 1 level when they are taken as factors are not good enough for analysis.
### These kind of values are of no use to our analysis.

irrelevant_col_names <- names(which(sapply(loan,function(x){
                        length(levels(as.factor(x)))
                        }) == 1))


### Let's add a few extra column names to this list of useless columns which are not required 
### for our analysis

### "url" column does not provide us any information for analysis purpose.

### member_id, out_prncp_inv, total_pymnt_inv columns are not required 
### as we are not going to analyse about the investment part here.

### emp_title,desc,title are all not relevant to our analysis.

### purpose and title columns have somewhat overlapping information. Though, the purpose column 
### has fewer discrete values and is much cleaner to analyze than title column. 
### So we will remove title column and keep purpose column.

### There are some variables which are taken into consideration only after having a default or after full payment. 
### These variables are: out_prncp, recoveries, collection_recovery_fee, last_pymnt_d, last_pymnt_amnt, next_pymnt_d, last_credit_pull_d
### So removing them as we are not doing post default or post full payment analysis.


irrelevant_col_names <- append(irrelevant_col_names,
                            c("url","member_id","sub_grade", 
                              "pub_rec_bankruptcies","out_prncp", 
                              "recoveries","last_pymnt_d", 
                              "collection_recovery_fee","last_credit_pull_d", 
                              "next_pymnt_d","last_pymnt_amnt","member_id",
                              "out_prncp_inv","total_pymnt_inv",
                              "desc","title","emp_title","zip_code","total_pymnt"
                              ))


### Remove all these irrelevant columns from our data frame
loan <- loan[, !(names(loan) %in% irrelevant_col_names)]

### Checking for NA values now
colSums(is.na(loan))     
### No remaining NA values found.

### Convert column values with % sign to numeric values

loan$revol_util <-  as.numeric(str_extract(loan$revol_util, "\\d+\\.*\\d*"))
loan$int_rate   <-  as.numeric(str_extract(loan$int_rate, "\\d+\\.*\\d*"))


### Coercing the date-time columns to standard Date Time objects.
loan$issue_d <- parse_date_time(loan$issue_d, orders = c("my", "ym"))
loan$earliest_cr_line<- parse_date_time(loan$earliest_cr_line, orders = c("my", "ym"))

### Removing the applications whose status is "Current". These applications are currently paying loans 
### and we can not say anything about them whether they will pay the loan completely or will default on the loan. 
### So we will exclude them from our analysis.

#loan <- loan[!grepl("Current", loan$loan_status) , ]

loan <- loan[!loan$loan_status == "Current", ]


===========================================================================================
### Metadata

### Demographic Variables of Customers:
### home_ownership, annual_inc, verification_status, addr_state, loan status, 

### Loan variables:
### loan amount, interest rate, loan grade, loan sub-grade, issue date, term, installment, purpose

### Other inancial variables of customer information:
### dti, credit_line_age, total_acc, revol_util, revol_bal, pub_rec, inq_last_6mths, earliest_cr_line, delinq_2yrs

===========================================================================================
### Derived Columns

### Year of issuance of loan
loan$issue_year <- year(loan$issue_d)


## Univariate Analysis

===========================================================================================

### Setting a theme for the plots

plot_theme <-   theme_light() +
  theme(axis.text = element_text(size =12)) + theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )

===========================================================================================

### Proportion of loan status
  table(loan$loan_status)
###  Charged Off     Current  Fully Paid 
###         5627           0       32950 

### Plot for loan status
ggplot(loan,aes(x=loan$loan_status, fill=loan$loan_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width=0.5) +
  guides(fill=guide_legend(title="Loan Status")) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.35,size = 5 ) +
  scale_y_continuous(labels = percent) +
  labs(title = "Plot of Loan Status", y = "Frequency", x = "Status of Loan")+
  plot_theme + theme(axis.text=element_text(size=14),
                   axis.title=element_text(size=14))

### There are around 14.6% people who have a loan default. We have to further analyse on these people's data 
### that what could be the possible factors leading them to default on the loan.

===========================================================================================

### Proportion of Home ownership status

  table(loan$home_ownership)
### MORTGAGE     NONE    OTHER      OWN     RENT 
###    17021        3       98     2975    18480 

### There are only 3 people whose home ownership details are not present. 
### That is a negligible proportion - around 0.01 %.

### Plot of Proportion of Home Ownership
ggplot(loan,aes(x=loan$home_ownership, fill=loan$home_ownership)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  guides(fill=guide_legend(title="Home Ownership")) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.35) +
  scale_y_continuous(labels = percent) +
  labs(title = "Plot of Home Ownership Status", y = "Frequency", x = "Status of Home Ownership")+
  plot_theme + theme(axis.text=element_text(size=14),
                    axis.title=element_text(size=14))

### Majority(around 92%) of the borrowers are either living in Mortgaged or rented houses.
### The number of people who own the home are very less (7.7 % only).

===========================================================================================

### Proportion of Term
  table(loan$term)
###  36 months  60 months 
###      29096       9481

### Plot of Proportion by Term

ggplot(loan,aes(x=loan$term, fill=loan$term)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width=0.5) +
  guides(fill=guide_legend(title="Term in months")) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.35) +
  scale_y_continuous(labels = percent) +
  labs(title = "Plot of Proportion of Term", y = "Frequency", x = "Term of Loan")+
  plot_theme + theme(axis.text=element_text(size=14),
                     axis.title=element_text(size=14))
===========================================================================================

### Proportion by Verification Status
 table(loan$verification_status)
### Not Verified  Source Verified   Verified 
###       16694              9677      12206
  
### Plot of Proportion by Verification status 
ggplot(loan,aes(x=loan$verification_status, fill=loan$verification_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width=0.5) +
  guides(fill=guide_legend(title="Verification Status")) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.35) +
  scale_y_continuous(labels = percent) +
  labs(title = "Plot of Proportion by Verification status ", y = "Frequency", x = "Status of Verification ")+
  plot_theme + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=02))

===========================================================================================

### Proportion by grade
  table(loan$grade)
###      A     B     C     D     E     F     G 
###  10045 11675  7834  5085  2663   976   299

### Plot of Proportion by Grade 
ggplot(loan,aes(x=loan$grade, fill=loan$grade)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  guides(fill=guide_legend(title="Grade")) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.35) +
  scale_y_continuous(labels = percent) +
  labs(title = "Plot of Proportion by Grade", y = "Frequency", x = "Grade")+
  plot_theme

### Grade B is having the highest number of loans, followed by Grade A, 
### and the number decreases from grade C to grade F thereafter.

===========================================================================================

### Proportion by Issue Year

  table(loan$issue_year)
###  2007  2008  2009  2010  2011 
###   251  1562  4716 11532 20516

### Plot of Proportion by year of issuing loan
  ggplot(loan,aes(x=loan$issue_year, fill=as.factor(loan$issue_year))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    guides(fill=guide_legend(title="Grade")) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.35) +
    scale_y_continuous(labels = percent) +
    labs(title = "Plot of Proportion by Year of issuing loan", y = "Frequency", x = "Year")+
    plot_theme
  
### There is a considerable increase in the number of loan applications from the year 2007 to 2011.
### Highest number of loans are issued in the year 2011(around 54.5%).
  
===========================================================================================

### Proportion by Employment Experience
  
 table(loan$emp_length)
### < 1 year    1 year 10+ years   2 years   3 years   4 years   5 years   6 years   7 years   8 years   9 years       n/a 
###     4508      3169      8488      4291      4012      3342      3194      2168      1711      1435      1226      1033 

### Plot of Proportion by Employment Experience

 ggplot(loan,aes(x=loan$emp_length, fill=as.factor(loan$emp_length))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  guides(fill=guide_legend(title="Years of \nWork Experience")) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.35) +
  scale_y_continuous(labels = percent) +
  labs(title = "Plot of Proportion by Employment Experience", y = "Frequency", x = "Work Experience in Years")+
  plot_theme
  
### The number of applications for loan are higher with the people having lesser years of experience and decreases as the years of experience increases.
### Note: The value in 10+ years of experience is more because the number of applications are spread over a larger group of people.

===========================================================================================

### Proportion by Loan Purpose
 
 table(loan$purpose)
###  car credit_card debt_consolidation  educational   home_improvement  house major_purchase medical moving  other  
### 1499        5027              18055          325               2875    367           2150     681    576   3865

### renewable_energy  small_business  vacation  wedding
###              102            1754       375      926

### Plot of Proportion by Employment Experience
ggplot(loan,aes(x=loan$purpose, fill=as.factor(loan$purpose))) +
 geom_bar(aes(y = (..count..)/sum(..count..))) +
 guides(fill=guide_legend(title="Years of \nWork Experience")) +
 geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.35) +
 scale_y_continuous(labels = percent) +
 labs(title = "Plot of Purpose of Loan Application", y = "Frequency", x = "Purpose of Applying for Loan")+
 plot_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

### Most of the applicants(46.9%) are applying for loan for debt_consolidation purpose, followed by credit_card(12.9%). 

===========================================================================================
### Proportion by State in which loan was applied

table(loan$addr_state)

### Plot of Proportion by State in which loan was applied

ggplot(loan,aes(x=loan$addr_state, fill=as.factor(loan$addr_state))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  guides(fill=guide_legend(title="State")) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.35) +
  scale_y_continuous(labels = percent) +
  labs(title = "Plot of Proportion by State", y = "Frequency", x = "State Code")+
  theme_light() +
  theme(axis.text = element_text(size =10,angle = 90, hjust = )) + theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)) + 
  theme(axis.text.x = element_text(1)) 


===========================================================================================

## Bivariate Analysis

### Create a generalised function to perform analysis

functionBiVar <- function(xval,yval,xlab,ylab){
  as.data.frame(percent(prop.table(table(yval, xval), 2))) %>%
    ggplot(aes(x=xval,y=Freq,fill=yval))+
    geom_col(position = "fill")+ 
    geom_text(aes(label = Freq),
              position = position_fill(vjust = .5),  
              size = 5) +
    labs(fill=ylab,x=xlab,y="Proportion",
         title = paste0(ylab," vs ", xlab))+
    plot_theme
}
===========================================================================================
### Lets see the pattern between Loan status vs Grade
functionBiVar( loan$grade,loan$loan_status,
               "Grade", "Loan Status")
===========================================================================================
### Lets see the pattern between Loan status vs Home ownership

functionBiVar( loan$home_ownership,loan$loan_status,
               "Home Ownership", "Loan Status")

### It shows us the regular pattern for fully paid customers

===========================================================================================
###  ** Loan status vs Purpose  *** 
functionBiVar( loan$purpose,loan$loan_status,
               "Loan Purpose", "Loan Status") + theme(axis.text.x = element_text(angle=45, hjust=1))

===========================================================================================
### It implies that most of the customer who are in the category of defaultee are taking loan for small_business purpose

###  ** Loan status vs term  ***
functionBiVar( loan$term,loan$loan_status,
               "Term", "Loan Status")

#23% defaultee's are taking loan for 60 months(5 Year)

===========================================================================================
###  ** Loan status vs verification status  ***
functionBiVar( loan$verification_status,loan$loan_status,
               "Verification Status", "Loan Status") 
===========================================================================================

functionBiVar( loan$emp_length,loan$loan_status,
               "Emp Experience", "Loan Status") + 
  theme(axis.text.x = element_text(angle=45, hjust=1))  
===========================================================================================  

### Analysing the Annual salary vs the loan status
ggplot(data = loan, mapping = aes(x= loan$loan_status, y=loan$annual_inc))+ geom_boxplot()

### There are certain outliers in the Annual Salary column (Applicants having extremely high salary as compared to the mean salary of the other applicants).
summary(loan$annual_inc[loan$annual_inc<150000])

### Let's take care of these outliers and plot a box plot for the annual salary and loan status.
loan[which(loan$annual_inc > 150000),]$annual_inc <-  150000

ggplot(data = loan, mapping = aes(x= loan$loan_status, y=loan$annual_inc,fill=loan$loan_status)) + 
  geom_boxplot() + plot_theme + 
  labs(title = "Plot of Annual Income vs Loan Staus", y = "Annual Salary", x = "Loan Status")

===========================================================================================  
### END OF FILE  
===========================================================================================
