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
MUA_data = read_excel("C:/Users/grfri/Google Drive/Spring 2022/Design/TKAMUA5_IDSET_Nov02.xlsx", sheet = "Final", col_names = TRUE, skip = 2)
#Changing MUA/C_MUA in to binary 1 or 0
MUA_data1 = mutate(MUA_data,
                   MUA_bi= ifelse(MUA == "Yes", 1, 0),
                   C_MUA_bi = ifelse(C_MUA == "Yes", 1, 0),
                   MUA_count_T = ifelse(MUA_type == "Both", mua_count +1, mua_count),
                   age_diff = age_C_TKA - age,
                   los_total = los+los_C_TKA,
                   op_time_total = op_time+ op_time_C_TKA,
                   date_diff =as.Date(`Date of contralateral TKA`) - as.Date(surgery_date) )
                   
                   
MUA_data1 = MUA_data1[1:665,]

###C_MUA is response

#Model with  MUA predictiing C_mua
mua_glm = glm(C_MUA_bi ~ MUA_bi, data= MUA_data1, family = "binomial")
summary(mua_glm)

#Odds ratio
exp(mua_glm$coefficients[-1])
#Odds of receiving MUA on contralateral knee is almost 13x times larger than not



#Model with  MUA predictiing C_mua with time
mua_glm1 = glm(C_MUA_bi ~ MUA_bi +date_diff + MUA_bi*date_diff + sex +age, data= MUA_data1, family = "binomial")
summary(mua_glm1)





### List dont use
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
los+
library(pscl)
#######


### Total MUA count is response

#Zero inflated model
mua_t_glm = zeroinfl(MUA_count_T ~ sex + age+
                  
                  
                  #tobacco+
                  op_time_total+
                    los_total
                  #BMI 
                    
                  #'Readmission within 90 days (1=yes)'
                    
                   , data = MUA_data1, dist= "negbin")
summary(mua_t_glm)


#Lasso model
library(glmnet)
library(fastDummies)
#Creating dummy matrix for lasso
#X matrix
x1matrix = data.matrix(MUA_data1[,c(20,23:45)])
x2matrix = dummy_cols(MUA_data1[,c(6:9,11)])
x22matrix = x2matrix[,-c(1:5)]
xmatrix= cbind(x22matrix,x1matrix)
xmatrix= data.matrix(xmatrix)
#Y matrix
y_muacount_t = as.matrix((MUA_data1$MUA_count_T))


#Finding lambda
cv.out= cv.glmnet(xmatrix, y_muacount_t, alpha=1, nfolds=600)
bestlam = cv.out$lambda.min
grid <- 10^ seq (10, -2, length = 100)

lasso.mod = glmnet(xmatrix, y_muacount_t, alpha=1, lambda=bestlam)
lasso.mod$beta


#Model with MUA count
library(pls)
MUA_count_data = MUA_data1[,22:44]
mua_count = pcr(mua_count ~ ., data= MUA_count_data)
summary(mua_count)

mua_count1 = glm(MUA_count ~., data = mua_count_reduced)
summary(mua_count1)
validationplot(mua_count)


MUA_data1[,24:44]

MUA_data1[,44]





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
ASA = c(MUA_data1$ASA, MUA_data1$ASA_C_TKA)
ID = rep(MUA_data1$ID,2)



#Standard Glm binomial
M_glm = glm(y~ los +
              sex+
              op_time+
              tobacco+
              ethnicity+
              BMI+
              age, family = "binomial")


summary(M_glm)



#Generalized Estimating Equation
library(gee)
M_glm2 = gee(y~ los +
              sex+
              op_time+
              ethnicity+
              BMI+
              age, id = ID, family = "binomial", corstr ="independence")


summary(M_glm2)
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







