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