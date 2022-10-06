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


#Jason's code

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

