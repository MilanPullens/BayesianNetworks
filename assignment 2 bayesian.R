library(dagitty)
library(ggplot2)
library(bnlearn)
library(broom)
library(MatchIt)

dag = dagitty('
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

# OR IMPORT PRE-PROCESSED AT LINE 134

csvPath <- "C:/Users/Milan/Documents/Bayesian Networks/Assignment1/topCombi.csv"

# Load data
projectCSV <- read.table(csvPath, header=TRUE, sep=",", quote = "\"")

# One hot encoding listener
projectCSV <- fastDummies::dummy_cols( projectCSV,
                                       select_columns=c("listener"),
                                       remove_most_frequent_dummy=FALSE )

scaleColumn <- function(column, scaleFactor) {
  # Ensure the input is numeric
  if (!is.numeric(column)) {
    stop("Input column must be numeric.")
  }
  
  # Find the minimum and maximum values of the column
  min_val <- min(column, na.rm = TRUE)
  max_val <- max(column, na.rm = TRUE)
  
  # Scale the values to the range [0, 1000]
  scaled_column <- as.integer(round((column - min_val) / (max_val - min_val) * scaleFactor))
  return(scaled_column)
}

integerizeColumn <- function(column, scaleFactor) {
  # Ensure the input is numeric
  if (!is.numeric(column)) {
    stop("Input column must be numeric.")
  }
  
  # Convert to integers
  integer_column <- as.integer(round(column * scaleFactor))
  return(integer_column)
}



# Columns to numerical
projectCSV$release_date <- scaleColumn(projectCSV$release_date, 55)
projectCSV$popularity <- scaleColumn(projectCSV$popularity, 100)
projectCSV$explicit <- integerizeColumn(projectCSV$explicit, 1)
projectCSV$danceability <- scaleColumn(projectCSV$danceability, 1000)
projectCSV$energy <- scaleColumn(projectCSV$energy, 1000)
projectCSV$key <- integerizeColumn(projectCSV$key, 1)
projectCSV$loudness <- integerizeColumn(projectCSV$loudness, 1)
projectCSV$mode <- integerizeColumn(projectCSV$mode, 1)
projectCSV$speechiness <- scaleColumn(projectCSV$speechiness, 1000)
projectCSV$acousticness <- scaleColumn(projectCSV$acousticness, 1000)
projectCSV$instrumentalness <- scaleColumn(projectCSV$instrumentalness, 1000)
projectCSV$liveness <- scaleColumn(projectCSV$liveness, 1000)
projectCSV$valence <- scaleColumn(projectCSV$valence, 1000)
projectCSV$tempo <- integerizeColumn(projectCSV$tempo, 1)
projectCSV$duration_ms <- integerizeColumn(projectCSV$duration_ms, 0.001)
projectCSV$time_signature <- integerizeColumn(projectCSV$time_signature, 1)

# Remove non-numerical columns
numerical_projectCSV <- projectCSV[, sapply(projectCSV, is.numeric)]

# Remove listener-column
numerical_projectCSV$listener <- NULL

integer_columns <- sapply(numerical_projectCSV, is.integer)
numerical_projectCSV[integer_columns] <- lapply(numerical_projectCSV[integer_columns], as.numeric)
projectCSV$listener_0 <- as.factor(projectCSV$listener_0)
projectCSV$listener_1 <- as.factor(projectCSV$listener_1)
projectCSV$listener_2 <- as.factor(projectCSV$listener_2)

numerical_projectCSV$listener_2 <- NULL

str(numerical_projectCSV)

# IMPORT PRE-Processed instead
# numerical_projectCSV <- read.table("C:/Users/Milan/Documents/Bayesian Networks/Assignment1/preProcessedData.csv", header=TRUE, sep=",", quote = "\"")

# DONT DO THE WHITE LIST FROM OUR INIITAL DAG ANYMORE, TO NOT BOTHER THE ALGORITHM TOO MUCH
# Define white list: edges that must exist
white_list <- data.frame(
  from = c("acousticness", "acousticness", "acousticness", "danceability", "duration_ms", 
           "energy", "energy", "explicit", "instrumentalness", "instrumentalness", 
           "key", "key", "listener_0", "listener_0", "listener_0", "listener_0", "listener_0", 
           "listener_0", "listener_1", "listener_1", "listener_1", "listener_1", "listener_1", 
           "liveness", "liveness", "loudness", "loudness", "release_date", "release_date", 
           "release_date", "release_date", "release_date", "release_date", "speechiness", 
           "speechiness", "speechiness", "tempo", "tempo", "time_signature", "valence"),
  to = c("energy", "loudness", "tempo", "popularity", "popularity", 
         "danceability", "valence", "popularity", "acousticness", "explicit", 
         "energy", "valence", "danceability", "instrumentalness", "listener_1", 
         "popularity", "release_date", "valence", "danceability", "instrumentalness", 
         "popularity", "release_date", "valence", "duration_ms", "instrumentalness", 
         "energy", "valence", "duration_ms", "key", "loudness", "speechiness", 
         "tempo", "time_signature", "acousticness", "duration_ms", "explicit", 
         "danceability", "energy", "tempo", "danceability")
)

# DONT DO THE BLACK LIST FROM OUR INIITAL DAG ANYMORE, TO NOT BOTHER THE ALGORITHM TOO MUCH
# Define black list: edges that must not exist
black_list <- data.frame(
  from = c("energy", "loudness", "tempo", "popularity", "popularity",
           "danceability", "valence", "popularity", "acousticness", "explicit",
           "energy", "valence", "danceability", "instrumentalness", "listener_1", 
           "popularity", "release_date", "valence", "danceability", "instrumentalness",
           "popularity", "release_date", "valence", "duration_ms", "instrumentalness", 
           "energy", "valence", "duration_ms", "key", "loudness", "speechiness",
           "tempo", "time_signature", "acousticness", "duration_ms", "explicit", 
           "danceability", "energy", "tempo", "danceability"),
  to = c("acousticness", "acousticness", "acousticness", "danceability", "duration_ms", 
         "energy", "energy", "explicit", "instrumentalness", "instrumentalness", 
         "key", "key", "listener_0", "listener_0", "listener_0", 
         "listener_0", "listener_0", "listener_0", "listener_1", "listener_1", "listener_1", 
         "listener_1", "listener_1", "liveness", "liveness", "loudness", 
         "loudness", "release_date", "release_date", "release_date", "release_date", 
         "release_date", "release_date", "speechiness", "speechiness", "speechiness", 
         "tempo", "tempo", "time_signature", "valence")
)

#THESE WHITE AND BLACK EDGES WILL BE ADDED:
# No edges from popularity to anything
popularity_no_outgoing <- data.frame(from = rep("popularity", times = 16), 
                                     to = c("acousticness", "danceability", "duration_ms", "energy", 
                                            "explicit", "instrumentalness", "key", "listener_0", 
                                            "listener_1", "liveness", "loudness", "release_date", 
                                            "speechiness", "tempo", "time_signature", "valence"))

# No edges to listener_0
listener_0_no_ingoing <- data.frame(from = c("acousticness", "danceability", "duration_ms", "energy", 
                                             "explicit", "instrumentalness", "key", "listener_1", 
                                             "liveness", "loudness", "release_date", "speechiness", 
                                             "tempo", "time_signature", "valence", "popularity"), 
                                    to = rep("listener_0", times = 16))

# No edges to listener_1, except for listener_0
listener_1_no_ingoing <- data.frame(from = c("acousticness", "danceability", "duration_ms", "energy", 
                                             "explicit", "instrumentalness", "key", "popularity", 
                                             "liveness", "loudness", "release_date", "speechiness", 
                                             "tempo", "time_signature", "valence"), 
                                    to = rep("listener_1", times = 15))

explicit_directions <- data.frame(from = c("explicit"), 
                                    to = c("release_date", "instrumentalness", "speechiness"))
loud_directions <- data.frame(from = c("energy"), 
                                  to = c("acousticness", "loudness", "instrumentalness", "tempo"))
dance_directions <- data.frame(from = c("danceability"), 
                              to = c("speechiness", "time_signature", "valence"))
duration_directions <- data.frame(from = c("duration_ms"), 
                               to = c("speechiness", "acousticness"))


# An edge from listener_0 to listener_1
white_list <- data.frame(
  from = c("listener_0"),
  to = c("listener_1")
)

black_list <- rbind(duration_directions, dance_directions, 
                    loud_directions, explicit_directions,
                    popularity_no_outgoing, listener_0_no_ingoing, listener_1_no_ingoing)

# structure learning algorithm
scores <- data.frame(k = numeric(0), score = numeric(0))

# determine 'optimal' k value
k_values <- seq(1, 30, by = 1)

for (k in k_values){
  learned_dag <- hc(numerical_projectCSV, 
                  whitelist = white_list, 
                  blacklist = black_list,
                  score = "bic-g", 
                  k=k) #can add debug=TRUE
  
  dag_score <- score(learned_dag, data = numerical_projectCSV, type = "bic-g")
  
  scores <- rbind(scores, data.frame(k = k, score = dag_score))
}

scores$difference <- c(NA, diff(scores$score))

print(scores)

library(ggplot2)
library(gridExtra)

# Plot 1: BIC Score vs k
plot1 <- ggplot(scores, aes(x = k, y = score)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "BIC Score vs k",
       x = "k-value",
       y = "BIC Score") +
  theme_minimal()

# Plot 2: Difference vs k
plot2 <- ggplot(scores, aes(x = k, y = difference)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Difference vs k",
       x = "k-value",
       y = "Difference") +
  theme_minimal()

# Combine the plots side by side
grid.arrange(plot1, plot2, ncol = 2)


# higher k values penalize more complex networks, so you get a sparser model
# bic-g penalizes complex models, useful to prevent overfitting

learned_dag <- hc(numerical_projectCSV, 
                  whitelist = white_list, 
                  blacklist = black_list,
                  score = "bic-g", 
                  k=6.4)
plot(learned_dag)

arcs <- bnlearn::arcs(learned_dag)
dag_string <- paste0(
  "dag { ",
  paste(apply(arcs, 1, function(x) paste(x[1], "->", x[2])), collapse = "; "),
  " }"
)

plot( learned_dag )

adjustmentSets(dag, exposure = "explicit", outcome = "popularity")
adjustmentSets(learned_dag, exposure = "explicit", outcome = "popularity")

lm_model <- lm(popularity ~ explicit, data = numerical_projectCSV)
summary(lm_model)

lm_model1 <- lm(popularity ~ explicit + listener_1, data = numerical_projectCSV)
summary(lm_model1)

lm_model2 <- lm(popularity ~ explicit + instrumentalness + release_date, data = numerical_projectCSV)
summary(lm_model2)

lm_model3 <- lm(popularity ~ explicit + instrumentalness + speechiness, data = numerical_projectCSV)
summary(lm_model3)

lm_modelL1 <- lm(popularity ~ explicit + instrumentalness + listener_1 + release_date, data = numerical_projectCSV)
summary(lm_modelL1)



df_mBase <- tidy(lm_model, conf.int = TRUE) 
df_m1 <- tidy(lm_model1, conf.int = TRUE) 
df_m2 <- tidy(lm_model2, conf.int = TRUE) 
df_m3 <- tidy(lm_model3, conf.int = TRUE) 
df_lm1 <- tidy(lm_modelL1, conf.int = TRUE)

df_mBase
df_m1
df_m2
df_m3
df_lm1

table(projectCSV$explicit)

ps_model <- glm(explicit ~ listener_1, data = numerical_projectCSV, family = binomial)
match <- matchit(explicit ~ listener_1, data = numerical_projectCSV, method = "nearest", caliper = 0.05)

matched_data <- match.data(match)

outcome_model <- lm(popularity ~ explicit, data = matched_data)

summary(outcome_model)
tidy(outcome_model)
# Second generated DAG
# Explicit to populairy with propensity score, on Explicit weighed, first 1, 2 to 0, 1

table(projectCSV$explicit)

ps_model <- glm(explicit ~ instrumentalness + speechiness, data = numerical_projectCSV, family = binomial)


match <- matchit(explicit ~ instrumentalness + speechiness, data = numerical_projectCSV, method = "nearest", caliper = 0.05)

matched_data <- match.data(match)

outcome_model <- lm(popularity ~ explicit, data = matched_data)

summary(outcome_model)
tidy(outcome_model)
