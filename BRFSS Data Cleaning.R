#mainly used haven(import xpt file and dplyr)

#import file and packages, set columns print test file
library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)
library(survey)
setwd("~/UNMC/fall 23 capston")
df <- read_xpt('LLCP2021.XPT')
write.csv(df, 'bfs.csv')
colnames(df) <- tolower(colnames(df))
datacols <- c('sexvar','genhlth','_hlthpln','employ1','children','_incomg1','_age_g','marital','_educag',
                '_rfsmok3', '_hcvu652', '_rfhlth', '_llcpwt', '_ststr', '_psu')

colnames(df)[colnames(df) == "_state"] ="state"

# for (state in na.omit(unique(df$state))) {
#   state_rows = df$state %in% state
#   
#   groups = table(df[state_rows, "X_AGEG5YR"])
#   
#   groups = groups / sum(groups)
#   
#   groups = age.weights[1:length(groups)] / groups
#   
#   df[state_rows, "X_AAWEIGHT"] = 
#     df[state_rows, "X_LLCPWT"] *
#     groups[df[state_rows, "X_AGEG5YR"]]
# }
#_smoker3 = Smoke100(100 cigs ever) and smokeday = smoker 
#smoker3 (1 or 2) current smoker, 

unclear <- c('77','99') #77 is unknown insurance, 99 is no answer, same for income
#FYI 88 means no coverage

#children: 88 = 0 kids, 99 = no answer

#filter to only needed columns, remove non-adults, remove people without insurance answers, split out smokers, non smokers

#rename columns to be easier to use
df_shorter <- df[,c(datacols)]
colnames(df_shorter)[colnames(df_shorter) == "_rfsmok3"] ="rfsmok3"
colnames(df_shorter)[colnames(df_shorter) == "_incomg1"] ="incomg1"
colnames(df_shorter)[colnames(df_shorter) == "_age_g"] ="age_g"
colnames(df_shorter)[colnames(df_shorter) == "_hlthpln"] ="hlthpln"
colnames(df_shorter)[colnames(df_shorter) == "_rfhlth"] ="rfhlth"
colnames(df_shorter)[colnames(df_shorter) == "_educag"] ="educag"
colnames(df_shorter)[colnames(df_shorter) == "_llcpwt"] ="llcpwt"
colnames(df_shorter)[colnames(df_shorter) == "_ststr"] ="ststr"
colnames(df_shorter)[colnames(df_shorter) == "_psu"] ="psu"

#remove nulls, recode 0s, NAs, blanks, etc
df_adult <- df_shorter

df_adult <- df_adult[df_adult$children != 99,]
df_adult$kids <- ifelse(df_adult$children==88,0,df_adult$children)
df_adult$kid_dum <- ifelse(df_adult$children == 88,0,1)
df_adult$sexvar <- df_adult$sexvar-1
df_adult <- df_adult[df_adult$hlthpln != 9,]
df_adult <- df_adult[df_adult$children != 99,]
df_adult$inc_r <- ifelse(df_adult$incomg1<= 4,1,
                         ifelse(df_adult$incomg1 == 5, 2,
                                ifelse(df_adult$incomg1 > 8,4,3 )))

#drop rows missing variables or no answer
df_adult <- df_adult[df_adult$rfsmok3 !=9,]
df_adult <- df_adult[df_adult$educag !=9,]
df_adult <- df_adult[df_adult$marital !=9,]
df_adult <- df_adult[df_adult$rfhlth !=9,]

#test outcome for only two responses
unique(df_adult$rfsmok3)

#create interaction term
df_adult$kid_ins <- df_adult$kid_dum * df_adult$hlthpln

#test file, ddrop all other rows with NA
df_adult <- df_adult[complete.cases(df_adult),]

#print final file
write.csv(df_adult, 'dfadult.csv')

#find percent remaining after adjustments
1-(nrow(df)-nrow(df_adult))/nrow(df)
(nrow(df)-nrow(df_adult))

