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



