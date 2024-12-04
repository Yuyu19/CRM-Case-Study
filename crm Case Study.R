#this section installs and loads the packages you'll need for the assignment
library(dplyr)
library(sqldf)
library(readr)

#this option disables scientific notation
options(scipen = 999)

#this section reads in the CSVs you'll need to complete the assignment.
#Critical: export files to your desktop and replace "jmills' with whatever will navigate the function to your desktop location. You can change the entire path to wherever you'd like to export the files, if you know what you're doing
CRMCaseStudy <- read_csv(file = "C:\\Users\\yfa7201\\Desktop\\Onboard\\CRM\\CRM_CASE_STUDY.csv" , col_names=TRUE, col_types = 'ccnncc')


#this section fixes some issues with reading in the first column name from each table
names(CRMCaseStudy)[1] <- c("CustomerID")


# 1.	How many customers are in test vs control?  How could these counts impact measurement?
query_output_1 <-sqldf("SELECT count(customerid) as COUNTS,CONTROL_IND
                        FROM
                        CRMCaseStudy 
                        group by CONTROL_IND")

#  211272 --- control,  2138394 --- test
# the numbers of test customers are 10X the number of the control customer

#2.	Looking at test vs control overall, how many incremental responders and incremental sales did the coupon drive?
query_output_2 <-sqldf("select  SUM(RESPONSE_IND) as SUM_RESPONSE_IND, 
                               SUM(NET_SALES) as SUM_NET_SALES,
                               CONTROL_IND
                        FROM  CRMCaseStudy 
                        group by CONTROL_IND")

#3.	Looking by frequency, how many incremental responders and incremental sales did the coupon drive? Which frequency group performs best?
query_output_3 <-sqldf("SELECT  SUM(RESPONSE_IND) as SUM_RESPONSE_IND, 
                                SUM(NET_SALES) as SUM_NET_SALES,
                             CONTROL_IND, frequency
                        FROM  CRMCaseStudy 
                        group by CONTROL_IND, FREQUENCY")

#4.	Looking by gender & frequency group, how many incremental responders and incremental sales did the coupon drive? Which gender & frequency group performs best?
query_output_4 <-sqldf("SELECT avg(RESPONSE_IND) as AVG_RESPONSE_IND,  
                               avg(NET_SALES) as AVG_NET_SALES,
                       SUM(RESPONSE_IND) as SUM_RESPONSE_IND, 
                       SUM(NET_SALES) as SUM_NET_SALES,
                       CONTROL_IND, frequency, gender
                       FROM  CRMCaseStudy 
                       group by CONTROL_IND, FREQUENCY,gender")

#5.	What bias may exist in any of the results provided?
query_output_5 <-sqldf("SELECT count(CustomerID) as counts, CONTROL_IND, frequency, gender
                       FROM  CRMCaseStudy 
                       group by CONTROL_IND, FREQUENCY,gender")


#T-test 

query_output_6 <-sqldf("select  avg(RESPONSE_IND) as avg_RESPONSE_IND, 
                               avg(NET_SALES) as avg_NET_SALES,
                               CONTROL_IND
                        FROM  CRMCaseStudy 
                        group by CONTROL_IND")
t.test(CRMCaseStudy$RESPONSE_IND[CRMCaseStudy$CONTROL_IND=="test"], CRMCaseStudy$RESPONSE_IND[CRMCaseStudy$CONTROL_IND=="control"] )
t.test(CRMCaseStudy$NET_SALES[CRMCaseStudy$CONTROL_IND=="test"], CRMCaseStudy$NET_SALES[CRMCaseStudy$CONTROL_IND=="control"] )


# anova

query_output_7 <-sqldf("SELECT  avg(RESPONSE_IND) as avg_RESPONSE_IND, 
                                avg(NET_SALES) as avg_NET_SALES,
                             CONTROL_IND, frequency
                        FROM  CRMCaseStudy 
                        group by CONTROL_IND, FREQUENCY")

two.way <- aov(RESPONSE_IND ~ CONTROL_IND + FREQUENCY, data = CRMCaseStudy)

summary(two.way)

two.way <- aov(NET_SALES ~ CONTROL_IND + FREQUENCY, data = CRMCaseStudy)
summary(two.way)


three.way <- aov(RESPONSE_IND ~ CONTROL_IND + FREQUENCY +GENDER , data = CRMCaseStudy)

summary(three.way)


three.way <- aov(NET_SALES ~ CONTROL_IND + FREQUENCY +GENDER , data = CRMCaseStudy)

summary(three.way)