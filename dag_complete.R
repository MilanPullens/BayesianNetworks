install.packages("dagitty")
install.packages("gRbase")
install.packages("stargazer")

library(stargazer)
library(dagitty)
library(ggdag)
library(writexl)
library(ggplot2)
library(gRbase)

csvPath = "C:/Users/Milan/Documents/Bayesian Networks/assignment1/topCombi.csv"
excelPath = "C:/Users/Milan/Documents/Bayesian Networks/assignment1/local_tests_results.xlsx"
coefCsvPath = "C:/Users/Milan/Documents/Bayesian Networks/assignment1/dagCoefs.csv"
filteredCoefCsvPath = "C:/Users/Milan/Documents/Bayesian Networks/assignment1/dagCoefs_filtered.csv"

# Load data
projectCSV <- read.table(csvPath, header=TRUE, sep=",", quote = "\"")

#One hot encoding listener
projectCSV <- fastDummies::dummy_cols( projectCSV,
                                       select_columns=c("listener"),
                                       remove_most_frequent_dummy=FALSE )

# Columns to numerical
projectCSV$release_date <- as.numeric(as.factor(projectCSV$release_date))
projectCSV$popularity <- as.numeric(as.factor(projectCSV$popularity))
projectCSV$explicit <- as.numeric(as.factor(projectCSV$explicit))
projectCSV$danceability <- as.numeric(as.factor(projectCSV$danceability))
projectCSV$energy <- as.numeric(as.factor(projectCSV$energy))
projectCSV$key <- as.numeric(as.factor(projectCSV$key))
projectCSV$loudness <- as.numeric(as.factor(projectCSV$loudness))
projectCSV$mode <- as.numeric(as.factor(projectCSV$mode))
projectCSV$speechiness <- as.numeric(as.factor(projectCSV$speechiness))
projectCSV$acousticness <- as.numeric(as.factor(projectCSV$acousticness))
projectCSV$instrumentalness <- as.numeric(as.factor(projectCSV$instrumentalness))
projectCSV$liveness <- as.numeric(as.factor(projectCSV$liveness))
projectCSV$valence <- as.numeric(as.factor(projectCSV$valence))
projectCSV$tempo <- as.numeric(as.factor(projectCSV$tempo))
projectCSV$duration_ms <- as.numeric(as.factor(projectCSV$duration_ms))
projectCSV$time_signature <- as.numeric(as.factor(projectCSV$time_signature))

# Import initial DAG
head(projectCSV)
projectG <- dagitty('dag {
acousticness [pos="-0.481,-0.949"]
danceability [pos="-1.282,-0.320"]
duration_ms [pos="-0.446,0.111"]
energy [pos="-0.685,-1.233"]
explicit [pos="-1.823,0.145"]
instrumentalness [pos="-0.477,-1.418"]
key [pos="-0.684,-1.796"]
listener_0 [pos="-2.006,-1.181"]
listener_1 [pos="-1.845,-0.415"]
liveness [pos="-0.376,-1.855"]
loudness [pos="-1.307,-1.624"]
popularity [pos="-1.260,0.157"]
release_date [pos="-1.701,-1.793"]
speechiness [pos="-0.472,-0.420"]
tempo [pos="-0.677,-0.829"]
time_signature [pos="-0.672,-0.367"]
valence [pos="-1.307,-0.966"]
acousticness -> energy
acousticness -> loudness
acousticness -> tempo
danceability -> popularity
duration_ms -> popularity
energy -> danceability
energy -> valence
explicit -> popularity
instrumentalness -> acousticness
instrumentalness -> explicit
key -> energy
key -> valence
listener_0 -> danceability
listener_0 -> instrumentalness
listener_0 -> listener_1
listener_0 -> popularity
listener_0 -> release_date
listener_0 -> valence
listener_1 -> danceability
listener_1 -> instrumentalness
listener_1 -> popularity
listener_1 -> release_date
listener_1 -> valence
liveness -> duration_ms
liveness -> instrumentalness
loudness -> energy
loudness -> valence
release_date -> duration_ms
release_date -> key
release_date -> loudness
release_date -> speechiness
release_date -> tempo
release_date -> time_signature
speechiness -> acousticness
speechiness -> duration_ms
speechiness -> explicit
tempo -> danceability
tempo -> energy
time_signature -> tempo
valence -> danceability
}
')

projectCSV <- projectCSV[, names(projectG)]
plot(projectG)

# Testing independencies
testResults <- localTests(projectG, data = projectCSV, type = "cis.pillai", abbreviate.names = FALSE)
testResultsDF <- as.data.frame(testResults)
testResultsDF

# Set threshold of 0.005
p_threshold <- 0.005
testResultsDF$result <- ifelse(testResultsDF$p.value >= p_threshold,
                               "Consistent", "Inconsistent")

# To xlsx
testResultsDF <- cbind(Row = rownames(testResultsDF), testResultsDF)
write_xlsx(testResultsDF, excelPath)

# Print table
summary_table <- table(testResultsDF$result)
print(summary_table)

# Print inconsistent results and edges
inconsistent_results <- subset(testResultsDF, result == "Inconsistent")
inconsistent_edges <- inconsistent_results$Row
inconsistent_results
inconsistent_edges

# Make plot of distribution of p-values
filteredResultsDF <- testResultsDF %>% filter(p.value >= exp(-80))

ggplot(filteredResultsDF, aes(x = p.value, fill = result)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = p_threshold, linetype = "dashed", color = "red") +
  labs(title = "Distribution of p-values for Local Tests",
       x = "p-value", y = "Count") +
  scale_fill_manual(values = c("Consistent" = "blue", "Inconsistent" = "red")) +
  scale_x_log10() +  # Apply log scale to x-axis
  theme_minimal()

# Extract the edges from the inconsistent results (inconsistent_edges)
inconsistent_edges <- inconsistent_results$Row

# Remove conditional independencies based on the inconsistent results
updatedDAG <- dagitty('dag {
acousticness [pos="0.290,-0.486"]
danceability [pos="-1.282,-0.320"]
duration_ms [pos="0.339,0.782"]
energy [pos="-0.378,-1.208"]
explicit [pos="-2.219,0.782"]
instrumentalness [pos="0.304,-1.446"]
key [pos="-0.378,-1.858"]
listener_0 [pos="-2.233,-1.136"]
listener_1 [pos="-2.233,0.140"]
liveness [pos="0.834,-1.450"]
loudness [pos="-1.307,-1.624"]
popularity [outcome,pos="-1.293,0.786"]
release_date [exposure,pos="-1.791,-1.798"]
speechiness [pos="0.308,0.067"]
tempo [pos="-0.339,-0.772"]
time_signature [pos="-0.321,-0.175"]
valence [pos="-1.307,-0.966"]
acousticness -> energy
acousticness -> loudness
acousticness -> tempo
danceability -> popularity
duration_ms -> popularity
energy -> danceability
energy -> valence
explicit -> danceability
explicit -> loudness
explicit -> popularity
instrumentalness -> acousticness
instrumentalness -> duration_ms
instrumentalness -> explicit
instrumentalness -> loudness
instrumentalness -> tempo
key -> energy
key -> valence
listener_0 -> acousticness
listener_0 -> danceability
listener_0 -> instrumentalness
listener_0 -> popularity
listener_0 -> release_date
listener_0 -> valence
listener_0 -> explicit
listener_0 <-> listener_1
listener_1 -> acousticness
listener_1 -> danceability
listener_1 -> instrumentalness
listener_1 -> popularity
listener_1 -> release_date
listener_1 -> valence
listener_1 -> explicit
liveness -> duration_ms
liveness -> instrumentalness
loudness -> energy
loudness -> valence
release_date -> duration_ms
release_date -> explicit
release_date -> key
release_date -> loudness
release_date -> speechiness
release_date -> tempo
release_date -> time_signature
speechiness -> acousticness
speechiness -> duration_ms
speechiness -> explicit
tempo -> danceability
tempo -> energy
time_signature -> acousticness
time_signature -> tempo
valence -> danceability
}
')

# Plot the updated DAG
plot(updatedDAG)

tester <- c()
for( n in names(updatedDAG) ){
  for( p in dagitty::parents(updatedDAG,n) ){
    otherparents <- setdiff( dagitty::parents(updatedDAG,n), p )
    tst <- ciTest( X=n, Y=p, Z=otherparents, projectCSV, type="cis.pillai")
    tester <- rbind( tester, data.frame(list(X=p,A="->",Y=n,
                                             cor=tst[,"estimate"],p=tst[,"p.value"])) )
  }
}
tester

testResultsUpdated <- localTests(updatedDAG, data = projectCSV, abbreviate.names = FALSE)

# Convert the test results to a data frame for better readability
testResultsDFUpdated <- as.data.frame(testResultsUpdated)

# Set the p-value threshold
p_threshold <- 0.005
testResultsDFUpdated$result <- ifelse(testResultsDFUpdated$p.value >= p_threshold, "Consistent", "Inconsistent")

# Write the updated results to an Excel file
write_xlsx(testResultsDFUpdated, excelPath)

# View the summary table of the updated results
summary_table_updated <- table(testResultsDFUpdated$result)
print(summary_table_updated)

# View the inconsistent results in the updated DAG
inconsistent_results_updated <- subset(testResultsDFUpdated, result == "Inconsistent")
print(inconsistent_results_updated)

# Plot the p-value distribution for the updated DAG
ggplot(testResultsDFUpdated, aes(x = p.value, fill = result)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = p_threshold, linetype = "dashed", color = "red") +
  labs(title = "Distribution of p-values for Local Tests (Updated DAG)",
       x = "p-value", y = "Count") +
  scale_fill_manual(values = c("Consistent" = "blue", "Inconsistent" = "red")) +
  scale_x_log10() +  # Apply log scale to x-axis
  theme_minimal()



finalDAG = dagitty('
dag {
acousticness [pos="-0.481,-0.949"]
danceability [pos="-1.282,-0.320"]
duration_ms [pos="-0.446,0.111"]
energy [pos="-0.685,-1.233"]
explicit [pos="-1.823,0.145"]
instrumentalness [pos="-0.477,-1.418"]
key [pos="-0.684,-1.796"]
listener_0 [pos="-2.006,-1.181"]
listener_1 [pos="-1.845,-0.415"]
liveness [pos="-0.376,-1.855"]
loudness [pos="-1.307,-1.624"]
popularity [pos="-1.260,0.157"]
release_date [pos="-1.701,-1.793"]
speechiness [pos="-0.472,-0.420"]
tempo [pos="-0.677,-0.829"]
time_signature [pos="-0.672,-0.367"]
valence [pos="-1.307,-0.966"]
acousticness -> energy
acousticness -> loudness
acousticness -> tempo
danceability -> popularity
duration_ms -> popularity
energy -> danceability
energy -> valence
explicit -> popularity
instrumentalness -> acousticness
instrumentalness -> explicit
key -> energy
key -> valence
listener_0 -> danceability
listener_0 -> instrumentalness
listener_0 -> listener_1
listener_0 -> popularity
listener_0 -> release_date
listener_0 -> valence
listener_1 -> danceability
listener_1 -> instrumentalness
listener_1 -> popularity
listener_1 -> release_date
listener_1 -> valence
liveness -> duration_ms
liveness -> instrumentalness
loudness -> energy
loudness -> valence
release_date -> duration_ms
release_date -> key
release_date -> loudness
release_date -> speechiness
release_date -> tempo
release_date -> time_signature
speechiness -> acousticness
speechiness -> duration_ms
speechiness -> explicit
tempo -> danceability
tempo -> energy
time_signature -> tempo
valence -> danceability
}'
)

# Calculate coefficients
dagCoefs <- c()
for( n in names(finalDAG) ){
  for( p in dagitty::parents(finalDAG,n) ){
    otherparents <- setdiff( dagitty::parents(finalDAG,n), p )
    tst <- ciTest( X=n, Y=p, Z=otherparents, projectCSV, type="cis.pillai")
    dagCoefs <- rbind( dagCoefs, data.frame(list(X=p,A="->",Y=n,
                                                 cor=tst[,"estimate"],p=tst[,"p.value"])) )
  }
}

dagCoefs
write.csv(dagCoefs, coefCsvPath, row.names = FALSE)

# Keep rows with p-values < 0.05
dagCoefs <- dagCoefs[dagCoefs$p < 0.05, ]

# Write to CSV
write.csv(dagCoefs, filteredCoefCsvPath, row.names = FALSE)

dagCoefs.dagitty <- paste(dagCoefs$X, dagCoefs$A, dagCoefs$Y, "[beta=",signif(dagCoefs$cor,2),"] ", collapse="\n")

#Plot with coefficients
g_with_coefficients <- dagitty( dagCoefs.dagitty )
plot( g_with_coefficients, show.coefficients=TRUE )


# FINAL DAG 
finalDAG = dagitty('
dag {
  acousticness [pos="-0.829,-1.374"]
  danceability [pos="-1.239,-0.644"]
  duration_ms [pos="-0.713,-0.509"]
  energy [pos="-1.111,-1.363"]
  explicit [pos="-1.099,-0.664"]
  instrumentalness [pos="-0.834,-1.829"]
  key [pos="-1.111,-1.868"]
  listener_0 [pos="-1.547,-1.143"]
  listener_1 [pos="-1.425,-0.411"]
  liveness [pos="-0.735,-1.866"]
  loudness [pos="-1.336,-1.837"]
  popularity [pos="-1.089,-0.389"]
  release_date [pos="-1.539,-1.751"]
  speechiness [pos="-0.918,-0.659"]
  tempo [pos="-0.818,-1.076"]
  time_signature [pos="-0.813,-0.790"]
  valence [pos="-1.307,-0.966"]
  acousticness -> energy
  acousticness -> loudness
  acousticness -> tempo
  energy -> valence
  explicit -> popularity
  instrumentalness -> acousticness
  instrumentalness -> explicit
  listener_0 -> danceability
  listener_0 -> instrumentalness
  listener_0 -> listener_1
  listener_0 -> release_date
  listener_0 -> valence
  listener_1 -> danceability
  listener_1 -> instrumentalness
  listener_1 -> popularity
  listener_1 -> release_date
  listener_1 -> valence
  liveness -> duration_ms
  liveness -> instrumentalness
  loudness -> energy
  loudness -> valence
  release_date -> duration_ms
  release_date -> loudness
  release_date -> speechiness
  speechiness -> duration_ms
  speechiness -> explicit
  tempo -> danceability
  tempo -> energy
  time_signature -> tempo
  valence -> danceability
}
')

# Calculate coefficients
dagCoefs <- c()
for( n in names(finalDAG) ){
  for( p in dagitty::parents(finalDAG,n) ){
    otherparents <- setdiff( dagitty::parents(finalDAG,n), p )
    tst <- ciTest( X=n, Y=p, Z=otherparents, projectCSV, type="cis.pillai")
    dagCoefs <- rbind( dagCoefs, data.frame(list(X=p,A="->",Y=n,
                                                 cor=tst[,"estimate"],p=tst[,"p.value"])) )
  }
}

dagCoefs.dagitty <- paste(dagCoefs$X, dagCoefs$A, dagCoefs$Y, "[beta=",signif(dagCoefs$cor,2),"] ", collapse="\n")

withCoords = paste('acousticness [pos="-0.829,-1.374"]
  danceability [pos="-1.239,-0.644"]
  duration_ms [pos="-0.713,-0.509"]
  energy [pos="-1.111,-1.363"]
  explicit [pos="-1.099,-0.664"]
  instrumentalness [pos="-0.834,-1.829"]
  key [pos="-1.111,-1.868"]
  listener_0 [pos="-1.547,-1.143"]
  listener_1 [pos="-1.425,-0.411"]
  liveness [pos="-0.735,-1.866"]
  loudness [pos="-1.336,-1.837"]
  popularity [pos="-1.089,-0.389"]
  release_date [pos="-1.539,-1.751"]
  speechiness [pos="-0.918,-0.659"]
  tempo [pos="-0.818,-1.076"]
  time_signature [pos="-0.813,-0.790"]
  valence [pos="-1.307,-0.966"]', toString(dagCoefs.dagitty), sep = "\n", collapse=NULL)
  
  
# Plot final DAG with coefficients
g_with_coefficients <- dagitty( withCoords )
plot( g_with_coefficients, show.coefficients=TRUE )


# Relevent edges for outcome 'popularity'
relevant_edges <- dagCoefs[dagCoefs$Y == "popularity", ]
print(relevant_edges)

# For exposure = explicity and outcome = popularity

# Give adjustment sets
adjustmentSets(finalDAG, exposure = "explicit", outcome = "popularity")

# Make linear regression models
lm_model1 <- lm(popularity ~ explicit + listener_1, data = projectCSV)
summary(lm_model1)

lm_model2 <- lm(popularity ~ explicit + instrumentalness + release_date, data = projectCSV)
summary(lm_model2)

lm_model3 <- lm(popularity ~ explicit + instrumentalness + speechiness, data = projectCSV)
summary(lm_model3)

# Compare the different models
anova(lm_model1, lm_model2, lm_model3)


# For exposure = listener_1 and outcome = popularity
# Give adjustment sets
adjustmentSets(finalDAG, exposure = "listener_1", outcome = "popularity")

# Make linear regression models
lm_model4 <- lm(popularity ~ listener_1, data = projectCSV)
summary(lm_model4)

lm_model5 <- lm(popularity ~ listener_1 + listener_0, data = projectCSV)
summary(lm_model5)

# Compare the different models
anova(lm_model4, lm_model5)

adjustmentSets(finalDAG, exposure = "listener_0", outcome = "popularity")
# Make linear regression models
lm_model6 <- lm(popularity ~ listener_0 + explicit + listener_1 + instrumentalness, data = projectCSV)
summary(lm_model6)

#lm_model7 <- lm(popularity ~ listener_1 + listener_0, data = projectCSV)
#summary(lm_model5)

# Compare the different models
anova(lm_model4, lm_model5)


# Save the summary of the models to an HTML file
stargazer(lm_model1, lm_model2, lm_model3, lm_model4, lm_model5,
          type = "html", 
          out = "model_summaries.html")

# Linear regression model for speechiness
model_with_speechiness <- lm(explicit ~ release_date + instrumentalness + speechiness, data = projectCSV)
summary(model_with_speechiness)

# Perform regression for loudness, adjusted for listener_0 and listener_1
cat("\n### Regression Results for Loudness (Adjusted for listener_0 and listener_1) ###\n")
model_loudness_adjusted <- lm(loudness ~ release_date + listener_0 + listener_1, data = projectCSV)
print(summary(model_loudness_adjusted))
### Regression Results for Time Signature ###
model_time_signature <- lm(time_signature ~ release_date, data = projectCSV)
print(summary(model_time_signature))


# Top 20 results of updated p-value
top_20_results <- testResultsDFUpdated[order(testResultsDFUpdated$p.value), ][1:20, ]

# Simple bar plot of the top 20 results
ggplot(top_20_results, aes(x = reorder(rownames(top_20_results), p.value), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Local Independence with lowest p value",
    x = "Test (Variable independencies)",
    y = "Estimate"
  ) +
  theme_minimal()

library(lavaan)

seModel <- '
  energy ~ acousticness + loudness + tempo
  valence ~ energy + listener_0 + listener_1
  popularity ~ explicit + listener_1
  instrumentalness ~ listener_0 + listener_1
  explicit ~ instrumentalness + speechiness
  danceability ~ listener_0 + listener_1 + tempo + valence
  duration_ms ~ liveness + release_date + speechiness
  loudness ~ acousticness + release_date
  release_date ~ listener_0 + listener_1
  speechiness ~ release_date
  tempo ~ acousticness + time_signature
  listener_1 ~ listener_0
  liveness ~ instrumentalness
'
# scaleInt <- 0
# scale_to_range <- function(column, rangeMin, rangeMax) {
#   min_val <- min(column, na.rm = TRUE)
#   max_val <- max(column, na.rm = TRUE)
#   
#   if (abs(max_val - min_val) <= 1) {
#     scaled_column <- ((column - min_val) / (max_val - min_val)) * (max_val - min_val) * ((rangeMax - rangeMin))
#   } else {
#     scaled_column <- ((column - min_val) / abs(max_val - min_val)) * (max_val - min_val)
#   }
#   return(as.integer(scaled_column))
# }
# 
# scaledCSV = projectCSV
# 
# scaledCSV$release_date <- scale_to_range(as.numeric(projectCSV$release_date), 0, 65)
# scaledCSV$popularity <- scale_to_range(as.numeric(projectCSV$popularity), 0, 100)
# scaledCSV$explicit <- scale_to_range(as.numeric(projectCSV$explicit), 0, 1)
# scaledCSV$danceability <- scale_to_range(as.numeric(projectCSV$danceability), 0, 1000)
# scaledCSV$energy <- scale_to_range(as.numeric(projectCSV$energy), 0, 1000)
# scaledCSV$key <- scale_to_range(as.numeric(projectCSV$key), -1, 11)
# scaledCSV$loudness <- scale_to_range(as.numeric(projectCSV$loudness), -60, 0)
# scaledCSV$speechiness <- scale_to_range(as.numeric(projectCSV$speechiness), 0, 1000)
# scaledCSV$acousticness <- scale_to_range(as.numeric(projectCSV$acousticness), 0, 1000)
# scaledCSV$instrumentalness <- scale_to_range(as.numeric(projectCSV$instrumentalness), 0, 1000)
# scaledCSV$liveness <- scale_to_range(as.numeric(projectCSV$liveness), 0, 1000)
# scaledCSV$valence <- scale_to_range(as.numeric(projectCSV$valence), 0, 1000)
# scaledCSV$tempo <- scale_to_range(as.numeric(projectCSV$tempo), 0, 200)
# scaledCSV$duration_ms <- scale_to_range(as.numeric(projectCSV$duration_ms), 0, 900)
# scaledCSV$time_signature <- scale_to_range(as.numeric(projectCSV$time_signature), 3, 7)

# Fit the model
fit <- sem(seModel, data=scaledCSV)

# Display the summary
summary(fit, standardized=TRUE)


