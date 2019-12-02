library(ggplot2)
library(e1071)
library(readr)
library(lattice)
library(caret)
library(quanteda)
library(irlba)
library(randomForest)
library(doSNOW)

# Load data
spam <- read_csv("Documents/R_flow/spam.csv", stringsAsFactors = FALSE, locale = locale(encoding = "windows-1252"))
View(spam)

# Clean the dataframe
spam.raw <- spam[, 1:2]
names(spam.raw) <- c("Label","Text")
View(spam.raw)

# Check for missing values
length(which(!complete.cases(spam.raw)))

# Convert class label into factor
spam.raw$Label <- as.factor(spam.raw$Label)

# View distribution of class labels (ham vs. spam)
prop.table(table(spam.raw$Label))

# View distribution of relative lengths of SMS
spam.raw$TextLength <- nchar(spam.raw$Text)
summary(spam.raw$TextLength)

# Visualize distribution
ggplot(spam.raw, aes(x = TextLength, fill = Label)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Text Length",
       title = "Distribution of Text Lenths with Class Labels")


# Use caret to create a 70%/30% stratified split. Set random seed for reproducibility
set.seed(32984)
indexes <- createDataPartition(spam.raw$Label, times = 1,
                               p = 0.7, list = FALSE)

train <- spam.raw[indexes,]
test <- spam.raw[-indexes,]

# Verify proportions
prop.table(table(train$Label))
prop.table(table(test$Label))


# PIPELINE

# Tokenize SMS text
train.tokens <- tokens(train$Text, what = "word",
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)
train.tokens <- tokens_tolower(train.tokens)
train.tokens <- tokens_select(train.tokens, stopwords(),
                              selection = "remove")
train.tokens <- tokens_wordstem(train.tokens, language = "english")

# Create bag-of-words model
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE, remove = stopwords())

# Transform to matrix 
train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)
  
# Establish feature df with labels
train.tokens.df <- cbind(Label = train$Label, as.data.frame(train.tokens.dfm))

# Clean column names
names(train.tokens.df) <- make.names(names(train.tokens.df))


# 10-fold cross validation
set.seed(48743)
cv.folds <- createMultiFolds(train$Label, k = 10, times = 3)
cv.cntrl <- trainControl(method = "repeatedcv", number = 10,
                         repeats = 3, index = cv.folds)

# Time code execution
start.time <- Sys.time()

cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)

rpart.cv.1 <- train(Label ~ ., data = train.tokens.df, method = "rpart",
                    trControl = cv.cntrl, tuneLength = 7)

stopCluster(cl)

total.time <- Sys.time() - start.time
total.time

# Check results
rpart.cv.1



