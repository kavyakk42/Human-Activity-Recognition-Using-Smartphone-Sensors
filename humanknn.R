dataset= read.csv('human.csv')
dataset=na.omit(dataset)




b=levels(dataset$Activity)
dataset$Activity= factor(dataset$Activity,levels = b,labels = c(1:6))



library(caTools)

set.seed(123)

split= sample.split(dataset$Activity,SplitRatio = 0.6)
training_set= subset(dataset,split==TRUE)
test_set= subset(dataset,split==FALSE)

training_set[1:165]=scale(training_set[1:165])
test_set[1:165]=scale(test_set[1:165])




library(class)
y_pred=knn(train = training_set[1:165],
           test = test_set[1:165],
           cl=training_set[,166],
           k=5, prob = TRUE)

cm=table(test_set[,166],y_pred)
print(cm)  #(91%)
