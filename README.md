# This code takes in Qualtrics CSV, wrangles, returns data set ready to be validated
# Missing values = -99
# All responses are in text format

library(readr)
library(dplyr)
library(psych)
library(plyr)
library(nFactors)
library(GPArotation)
library(ggplot2)
library(psy)
library(semTools)

OHP <- read_csv("C:/Users/jerry/OneDrive/Documents/OHP/Scale validation/OHP_April 21, 2019_16.53.csv", na = c("-99")) %>% 
          filter(Q31 == "Somewhat disagree", 
                 Q44 == "Yes",
                 Q45 == "Yes",
                 Q46 == "Yes") %>% 
          select(Q67_1:Q67_8,
                 Q67_9:Q67_12,
                 Q68_1:Q68_8,
                 race, sex, age) %>% 
          .[-c(1,2),] %>% 
          filter_all(., all_vars(.!="Not Applicable")) %>% 
          mutate(age = as.numeric(age))

#####

npf_recode <- function(x){
  recode(x,"Less than once per month or never" = 1,             
         "Once or twice per month" = 2,
         "Once or twice per day" = 3,  
         "Once or twice per week" = 4,      
         "Several times per day" = 5)
}

wse_recode <- function(x){
  recode(x, "Strongly disagree" = 1,
         "Disagree" = 2,
         "Neutral" =3,
         "Agree" = 4,                        
         "Strongly agree" = 5)
}

#####

OHP[,c(1:12)] <- apply(OHP[,c(1:12)],2,npf_recode)

OHP[,c(13:20)] <- apply(OHP[,c(13:20)],2,wse_recode)

OHP

#####

count(OHP$sex)
count(OHP$race)
describe(OHP$age)

hist(OHP$Q67_1)
hist(OHP$Q67_5)
hist(OHP$Q67_10)

#####


alpha(OHP[,c(1:12)])
alpha(OHP[,c(13:20)])
alpha(OHP[,c(1:4)])
alpha(OHP[,c(5:8)])
alpha(OHP[,c(9:12)])

#EFA

NPF <- (OHP[,c(1:12)])
WSE <- (OHP[,c(13:20)])

parallel <- fa.parallel(NPF, fm = 'minres', fa = 'fa')

fit <- factanal(NPF, 3, rotation="oblimin")
print(fit, digits=2, cutoff=.3, sort=TRUE)

onefactor <- fa(NPF,nfactors = 1,rotate = "oblimin",fm="ml")
print(onefactor)
twofactor <- fa(NPF,nfactors = 2,rotate = "oblimin",fm="ml")
print(twofactor)
threefactor <- fa(NPF,nfactors = 3,rotate = "oblimin",fm="ml")
print(threefactor) #question3,8,&9
fourfactor <- fa(NPF,nfactors = 4,rotate = "oblimin",fm="ml")
print(fourfactor) 

#####

scale1<- 'F1=~ Q67_1 + Q67_2 + Q67_3 + Q67_4

F2=~ Q67_5 + Q67_6 + Q67_7 + Q67_8

F3=~ Q67_9 + Q67_10 + Q67_11 + Q67_12


work=~ Q68_1+ Q68_2+ Q68_3+ Q68_4+ Q68_5+ Q68_6+ Q68_7+ Q68_8'

htmt(scale1, data=OHP)

scale<- 'F1=~ Q67_1 + Q67_2 + Q67_3 + Q67_4 +

Q67_5 + Q67_6 + Q67_7 + Q67_8 +

Q67_9 + Q67_10 + Q67_11 + Q67_12

work=~ Q68_1+ Q68_2+ Q68_3+ Q68_4+ Q68_5+ Q68_6+ Q68_7+ Q68_8'

htmt(scale, data=OHP)

x <- OHP$Q67_1 + OHP$Q67_2 + OHP$Q67_3 + OHP$Q67_4 + OHP$Q67_5 + OHP$Q67_6 + OHP$Q67_7 + OHP$Q67_8 + OHP$Q67_9 + OHP$Q67_10 + OHP$Q67_11 + OHP$Q67_12
M <- OHP$Q67_1 + OHP$Q67_2 + OHP$Q67_3 + OHP$Q67_4
Co <- OHP$Q67_5 + OHP$Q67_6 + OHP$Q67_7 + OHP$Q67_8
CC <- OHP$Q67_9 + OHP$Q67_10 + OHP$Q67_11 + OHP$Q67_12
y <- OHP$Q68_1 + OHP$Q68_2 + OHP$Q68_3 + OHP$Q68_4 + OHP$Q68_5 + OHP$Q68_6 + OHP$Q68_7 + OHP$Q68_8

cor(CC, y)
