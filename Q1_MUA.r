Q1_data <- read_excel("DeID2.xlsx", sheet = "Q1", skip = 1)

#Pearson's Chi-squared test
chisq.test(Q1_data$MUA, Q1_data$C_MUA)

Q1_table <- table(Q1_data)
rownames(Q1_table) <- c("No MUA", "MUA")
colnames(Q1_table) <- c("No C_MUA", "C_MUA")

#mosicplot
mosaicplot(Q1_table, main = "MUA plot", color = TRUE)

# fisher's exact test for count data 
test<-fisher.test(Q1_table)

# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  Q1_data, MUA, C_MUA,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.001, "< 0.001", round(test$p.value, 3))
  )
)


# reference : https://statsandr.com/blog/fisher-s-exact-test-in-r-independence-test-for-a-small-sample/

##### Jasons code
#Simulating how many No mua patients are needed to reject null
#Loop creating p-values from fisher exact test for n= 1:500
n = 500
pvalz = c()
for (i in 1:n) {
  x = matrix(c(3, 9, 21, i), ncol = 2)
  pvalz[i] = fisher.test(x)$p.value
}


#Plot of p-values with critical p=0.05 abline
plot(c(seq(1, 500, 1)), 
     pvalz, pch = 16, 
     main = "Number of MUA free patients required to reject null",
     xlab = "n", ylab= "P-value")
abline(h = 0.05, col = "red")
#n=293 is the first number that passes the critical value




##############
#Potential Visualizations of data
#Full patient lists
library(readxl)
Pts_full = read_excel("C:/Users/grfri/Google Drive/Spring 2022/Design/Q1.xlsx", sheet = "OriginalData")
Pts_2TKA = read_excel("C:/Users/grfri/Google Drive/Spring 2022/Design/Q1.xlsx", sheet = "2TKA")

#Separated patients with only 1 TKA into different group
Pts_1TKA =read_excel("C:/Users/grfri/Google Drive/Spring 2022/Design/Q1.xlsx", sheet = "1TKA")


#Histogram of MUA count on one knee
hist(Pts_1TKA$mua_count, xlab = "Frequency of multiple MUAs on one knee")


#Comparing health of 1 TKA patients vs 2 TKA patients
#Histogram of ASA rating 
par(mfrow=c(1,2))
hist(Pts_1TKA$asa_rating, main="Patients with 1 TKA", breaks = 5, xlim = range(0:4), xlab = "ASA Rating")
hist(Pts_2TKA$asa_rating, main = "Patients with 2 TKA", breaks = 5, xlim = range(0:4), xlab = "ASA Rating")
#While the health of each group appears similar, patients with 2 TKA's appear more right skewed
#Good to keep in mind that patients with multiple TKAs may be more unhealthy and that needs to be controlled for.


#Correlation Matrix of comorbities
 #Comorbities without "platelet_transfusion", "AIDS", "wound_infection", "hematoma", "knee_infection"
#These excluded variables had no counts
library(ggcorrplot)
comorb_2 = Pts_full[,c(20, 24:38)] 
ggcorrplot(cor(comorb_2), title = "Correlation matrix of comorbities of patients who underwent MUA")



##############
#Model building

library(readxl)
libary(tidyverse)
MUA_data = read_excel("C:/Users/grfri/Google Drive/Spring 2022/Design/TKAMUA5_IDSET_Nov02.xlsx", sheet = "Final", col_names = TRUE, skip = 2)


MUA_data1 = mutate(MUA_data,
                   MUA_bi= ifelse(MUA == "Yes", 1, 0),
                   C_MUA_bi = ifelse(C_MUA == "Yes", 1, 0),
                   MUA_count_t = MUA_bi + C_MUA_bi,
                   age_diff = age_C_TKA - age,
                   los_total = los+los_C_TKA,
                   op_time_total = op_time+ op_time_C_TKA,
                   date_diff =as.Date(`Date of contralateral TKA`) - as.Date(surgery_date),
                   ASA_H = ifelse(ASA == "4" | ASA == "4E", 1,0),
                   ASA_H_C = ifelse(ASA_C_TKA == "4" | ASA_C_TKA == "4E", 1,0)
                   )

MUA_data1$financial_class <- 
  ifelse(MUA_data1$financial_class %in% c("Blue Cross Commercial","Commercial LUHS","Insurance","Worker's Comp","Worker's Comp LUHS"), "Private",
         ifelse(MUA_data1$financial_class %in% c("Medicare","Medicare LUHS","Managed Medicare"),"Medicare",
                ifelse(MUA_data1$financial_class%in% c("Managed Medicaid","Medicaid","MMAI"),"Medicaid","Uninsured")))                  
MUA_data1 = MUA_data1[1:665,]
MUA_data1$Fac_age_C_TKA <- ifelse(MUA_data1$age_C_TKA<50,"less50",
                                  ifelse(MUA_data1$age_C_TKA<60, "50s",
                                         ifelse(MUA_data1$age_C_TKA<70, "60s","over70s")))


###C_MUA is response

#Model with  MUA predictiing C_mua
mua_glm = glm(C_MUA_bi ~ MUA_bi, data= MUA_data1, family = "binomial")
summary(mua_glm)

#Odds ratio
exp(mua_glm$coefficients[-1])
#Odds of receiving MUA on contralateral knee is almost 13x times larger than not


MUA_data1$Fac_age_C_TKA <- ifelse(MUA_data1$age_C_TKA<50,"less50",
                                  ifelse(MUA_data1$age_C_TKA<60, "50s",
                                         ifelse(MUA_data1$age_C_TKA<70, "60s","over70s")))



#Model with  MUA predictiing C_mua with time
mua_glm1 = glm(C_MUA_bi ~ MUA_bi+ date_diff, data= MUA_data1, family = "binomial")
summary(mua_glm1)


mua_glm2 = glm(C_MUA_bi ~ MUA_bi+ Fac_age_C_TKA+ as.factor(ASA_H_C) + Fac_age_C_TKA*as.factor(ASA_H_C), data= MUA_data1, family = "binomial")
summary(mua_glm2)



#Dont use this code
blood_transfusion+
  platelet_transfusion+
  AIDS +
  Malignancy +
  Cerebrovascular +
  COPD +
  CHF +
  Dementia +
  Diabetes_cc +
  Diabetes_no_cc +
  Hemiplegia +
  Metastatic +
  Mild_Liver +
  Moderate_Liver +
  MI +
  Peptic_Ulcer +
  PVD +
  CKD +
  Rheumatic +
  hematoma +
  wound_infection +
  knee_infection +
  `Readmission within 90 days (1=yes)`
race
#los+
library(pscl)



### Total MUA count is response

#Zero inflated model
mua_t_glm = zeroinfl(MUA_count_t ~ sex+ age+
                  
                  
                  
                  
                    los_total
                    
                  #'Readmission within 90 days (1=yes)'
                    
                   , data = MUA_data1, dist= "negbin")
summary(mua_t_glm)


#Standared model
mua_t_glm1 = glm(MUA_count_t ~ sex + age+
                       
                       
                       #tobacco+
                       
                       los_total+ los_total*sex + los_total*age
                   
                     
                     
                     #'Readmission within 90 days (1=yes)'
                     
                     , data = MUA_data1)
summary(mua_t_glm1)





#### Binomial models with just probability of MUA (no MUA/C_MUA distinction)
#Imputing MUA/C_MUA data as vectors
y= c(MUA_data1$MUA_bi, MUA_data1$C_MUA_bi)
los = c(MUA_data1$los, MUA_data1$los_C_TKA)
sex = rep(MUA_data1$sex, 2)
op_time = c(MUA_data1$op_time, MUA_data1$op_time_C_TKA)
tobacco = c(MUA_data1$tobacco, MUA_data1$tobacco_C_TKA)
ethnicity = rep(MUA_data1$ethnicity, 2)
BMI = c(MUA_data1$BMI, MUA_data1$bmi_C_TKA)
age = c(MUA_data1$age, MUA_data1$age_C_TKA)
Fac_age_TKA <- ifelse(age<50,"less50",
                                  ifelse(age<60, "50s",
                                         ifelse(age<70, "60s","over70s")))
levels(Fac_age_TKA) = c("less50", "50s", "60s", "over70s")
ASA = c(MUA_data1$ASA, MUA_data1$ASA_C_TKA)
ID = rep(MUA_data1$ID,2)



#Standard Glm binomial
M_glm = glm(y~ los +
              sex+
              
              
              
              BMI+
      BMI*sex+ age*sex+ + los*sex+ los*age+
              age, family = "binomial")


summary(M_glm)



#Generalized Estimating Equation
library(gee)
M_glm2 = gee(y~ los +
              sex+
              op_time+
              
              BMI+
              age , id = ID, family = "binomial", corstr ="exchangeable")


summary(M_glm2)
anova(M_glm2)
library(geepack)
mf=formula(y~ los +
  sex+
  op_time+
  ethnicity+
  BMI+
  age)
M_glm4 = geeglm(y~ los +
                  sex+
                  op_time+
                  ethnicity+
                  BMI+
                  age, id = ID, family = "binomial", corstr ="ind")
summary(M_glm4)




#Random effects
library(lme4)
M_glm3 = glmer(y~ los +
               sex+
               op_time+
                 #ethnicity+
                 as.factor(ASA)+
               
               BMI+
               age + (1|ID), family = "binomial")


summary(M_glm3)






##############
#Additional Q3 models
#1 lasso model with all relavant factors (not just comorbities)
#2 multinomial models



#Please rerun data cleaning for insurance with code below 
#Give Workers comp its own category. 
#It is different than private and is only used when a worker is hurt on the job
names(MUA_data1)[77] <- c("Insurance_C_TKA")
MUA_data1$Insurance_C_TKA <- 
  ifelse(MUA_data1$Insurance_C_TKA %in% c("Blue Cross Commercial","Commercial LUHS","Insurance"), "Private",
         ifelse(MUA_data1$Insurance_C_TKA %in% c("Medicare","Medicare LUHS","Managed Medicare"),"Medicare",
                ifelse(MUA_data1$Insurance_C_TKA %in% c("Worker's Comp","Worker's Comp LUHS"), "Work_Comp",
                       ifelse(MUA_data1$Insurance_C_TKA %in% c("Managed Medicaid","Medicaid","MMAI"),"Medicaid","Uninsured"))))




####Lasso models with relevant variables
#Variables include MUA_bi, Age, sex, ethnicity, BMI, tobacco, financial class (reduced), 
#los, ASA, OP time, comorbities, readmit_90, and redu_race
library(glmnet)
y = MUA_data1$C_MUA_bi
MUA_data2 = MUA_data1[,c(108, 6, 9,74:78, 80:104, 119)] #Your exact columns may be different
d <- data.frame(x=MUA_data2, y=y)

options(na.action="na.pass")
m <- model.matrix(y ~ ., data=d)

set.seed(1234)
cv.out = cv.glmnet(m, y, alpha =1)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam


lasso.mod = glmnet(m, y, alpha= 1, lambda = bestlam)


coef(lasso.mod)

#nonzero coefficients for the variables are:
#MUA_bi,
#Workers_comp (Insurance)
#Blood transfusion
#Readmit_90d_C_TKA




## Multinomial models:
library(VGAM)


#Multinomial model with Both, C_MUA, MUA, No MUA:
#Removed most comorbities except blood transfusion and readmit_90
#Model was not converging otherwise
M_MUA_set = MUA_data1[,c( 6, 9,74:78, 82, 104, 119)] #80:104
M_set = cbind(MUA_data1$MUA_type, M_MUA_set)
M_set = data.frame(M_set)
M_set = M_set[complete.cases(M_set),]
M_set$MUA_data1.MUA_type = as.factor(M_set$MUA_data1.MUA_type)
levels(M_set$MUA_data1.MUA_type) = c("No_MUA", "MUA", "C_MUA", "Both")

fit_multi1 =vglm(MUA_data1.MUA_type ~ ., family = multinomial, data = M_set)


summary(fit_multi1)
#Has trouble converging. Blood transfusion still appears to be significant
#Non Hispanic origin significant for "Both"






#Multinomial model with y response as: "Both" ,"One Knee", "None"

#Removed most comorbities except blood transfusion and readmit_90
#Model was not converging otherwise
M_set2 = MUA_data1[,c( 6, 9,74:78, 82, 104, 119)]
M_set2 = mutate(M_set,
                MUA_num = ifelse(MUA_data1.MUA_type == "Both", "Both_knees",
                ifelse(MUA_data1.MUA_type == "C_MUA" , "One_knee",
                       ifelse(MUA_data1.MUA_type == "MUA", "One_knee", "None"))),
                MUA_num = as.factor(MUA_num),
                )

levels(M_set2$MUA_num) = c("None", "One_knee", "Both_knees")

fit_multi2 =vglm(MUA_num ~ . -MUA_data1.MUA_type, family = multinomial, data = M_set2)


summary(fit_multi2)
#Non hispanic origin and blood transfusion significant for "Both"


##### ROC
#lasso.mod = lasso mod using bestlam from cv.glmnet
#m = original model matrix
predict = predict(lasso.mod, m, type = "response")

library(pROC)
roc_score=roc(y, as.numeric(predict)) 

#ROC Plot
plot(roc_score ,main ="ROC curve -- Lasso Regression ") 

roc_score$auc #AUC score



##### Bootstrapping AUC to find 95% CI
#Finding Bootstrapped AUC confidence intervals
library(fbroc)
boot_roc = boot.roc(as.numeric(predict), as.logical(y), n.boot = 10000)
plot(boot_roc)


w=perf(boot_roc, "auc") #Measuring performance
w #AUC confidence interval
hist(w$boot.results, main = "Bootstrapped AUC Values", xlab = "Area Under Curve") #Histogram of bootstrapped AUC



