# MAX-MIN NORMALIZATION

dataset= read.csv('human.csv')
dataset=na.omit(dataset)

library(nnet)






library(caTools)

set.seed(123)

split= sample.split(dataset$Activity,SplitRatio = 0.75)
training_set= subset(dataset,split==TRUE)
test_set= subset(dataset,split==FALSE)

training_set[1:165]=scale(training_set[1:165])
test_set[1:165]=scale(test_set[1:165])

classifier = nnet(Activity~.,
                         data=training_set,size = 4,
                         decay = 0.0001,maxit = 500)



summary(classifier)

test_set$pred_nnet<-predict(classifier,test_set,type="class")

cm=table(test_set[,166],test_set$pred_nnet)
print(cm)

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

pdf("rrplot.pdf")
plot.nnet(classifier)



dev.off()

