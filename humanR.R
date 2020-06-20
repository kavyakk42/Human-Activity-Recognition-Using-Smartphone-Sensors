library(ggplot2) #for bar and line graphs
library(dplyr)
library(tidyr)
library(lubridate) # for date time and month
library(gganimate) # for animation
library(gifski)
library(av)
library(gapminder)


dataset= read.csv('human.csv',na.strings = c("","NA"))
dataset=na.omit(dataset)


mydata = select(dataset, -21,-23,-25,-27,-29,-31,-33,-35,-37,-39,-41,-43,-45,-47,-49,-51,-53,-55,-57,-59,-61,-63,-65,-67,-69,-71,-73,-75,
                -77,-79,-81,-83,-85,-87,-89,-91,-93,-95,-97,-99,-101,-103,-105,-107,-109,-111,-113,-115,-117,-119,-121,-123
                ,-125,-127,-129,-131,-133,-135,-137,-139,-141,-143,-145,-147,-149,-151,-153,-155,-157,-159,-161,-163,-165,-167
                ,-169,-171,-173,-175,-177,-179,-181,-183,-185,-187,-189,-191,-193,-195,-197,-199,-201,-203,-205,-207,-209,-211,-213
                ,-215,-217,-219,-221,-223,-225,-227,-229,-231,-233,-235,-237,-239,-241,-243,-245,-247,-249,-251,-253,-255,-257,-259,-261
                ,-263,-265,-267,-269,-271,-273,-275,-277,-279,-281,-283,-285,-287,-289,-291,-293,-295,-297,-299,-301,-303,-305,-307,-309,-311
                ,-313,-315,-317,-319,-321,-323,-325,-327,-329,-331,-333,-335,-337,-339,-341,-343,-345,-347,-349,-351,-353,-355,-357,-359,-361,-363
                ,-365,-367,-369,-371,-373,-375,-377,-379,-381,-383,-385,-387,-389,-391,-393,-395,-397,-399,-401,-403,-405,-407,-409,-411,-413,-415
                ,-417,-419,-421,-423,-425,-427,-429,-431,-433,-435,-437,-439,-441,-443,-445,-447,-449,-451,-453,-455,-457,-459,-461,-463,-465,-467
                ,-469,-471,-473,-475,-477,-479,-481,-483,-485,-487,-489,-491,-493,-495,-497,-499,-501,-503,-505,-507,-509,-511,-513,-515,-517,-519,
                -521,-523,-525,-527,-529,-531,-533,-535,-537,-539,-541,-543,-545,-547,
               
                -46,-48,-50,-52,-54,-56,-58,-60,-62,-64,-66,-68,-70,-72,-84,-86,-88,-90,-92,-94
                ,-96,-98,-100,-102,-104,-106,-108,-110,-132,-134,-136,
                -138,-140,-142,-144,-146,-148,-150,-152,-154,-156,-158,-160,-176,-178,-180,
                -182,-184,-186,-188,-190,-192,-194,-196,-198,-200,-202,-204,-206,-208,-210,-212,-214,-216,-218,-220,-222,-224,-226,-228,
                -230,-232,-234,-252,-254,-256,-258,-260,-262,-264,-266,-268,-270,-272,-274,-276,-278,
                -280,-282,-284,-286,-288,-290,-292,-294,-296,-318,-320,-322,-324,-326,-328,-330,
                -332,-334,-336,-338,-340,-342,-344,-346,-348,-350,-352,-354,-356,-358,-360,-362,-374,-376,-378,-380,-382,-384,
                -386,-388,-390,-392,-394,-396,-398,-400)

b=levels(mydata$Activity)
dataset$Activity= factor(mydata$Activity,levels = b,labels = c(1:6))



library(caTools)

set.seed(123)

split= sample.split(mydata$Activity,SplitRatio = 0.6)
training_set= subset(mydata,split==TRUE)
test_set= subset(mydata,split==FALSE)

training_set[1:165]=scale(training_set[1:165])
test_set[1:165]=scale(test_set[1:165])

library(randomForest)
set.seed(123)
classifier=randomForest(x=training_set[1:165],y=training_set$Activity,ntree=50)
y_pred= predict(classifier,newdata=test_set[1:165])
cm=table(test_set[,166],y_pred)
print(cm) #96% accuracy


png("randomforest.png")
plot(randomForest(Activity ~ ., training_set, keep.forest=FALSE, ntree=100), log="x")
dev.off()


#plots

#pie chart
training_set %>%
  ggplot(aes(x=Activity, fill=Activity))+
  geom_bar(width=1)+
  coord_polar(theta = 'x')



# bar graphs

s=tapply(dataset$tBodyAcc.mean.X,dataset$Activity,mean)
print(s)
h=c(0.2686486 ,0.2730596, 0.2791535,0.2763369,0.2881372,0.2622946  )
m=c( "LAY","SIT","STAND","WALK", "WALKD","WALKUP"  )
colors=c(1:6)
png(file="mean accelometer2.png")
barplot(h,names.arg = m,xlab = "Activity",ylab = "Accelometer",main = "Mean Accelometer",col=colors,width = 100)
dev.off()



s=tapply(dataset$tGravityAcc.mean.X,dataset$Activity,mean)
print(s)
h=c( -0.3750213       ,   0.8797312       ,   0.9414796     ,     0.9349916    ,      0.9264574     ,    0.8750034  )
m=c( "LAY","SIT","STAND","WALK", "WALKD","WALKUP"  )
colors=c(1:6)
png(file="mean gravityaccelometer2.png")
barplot(h,names.arg = m,xlab = "Activity",ylab = "Gravity Accelometer",main = "Mean Gravity Accelometer",col=colors,width = 100)
dev.off()




s=tapply(dataset$tBodyAccJerk.mean.X,dataset$Activity,mean)
print(s)
h=c( 0.08184739     ,    0.07587885     ,    0.07502792  ,       0.07671874   ,      0.08922669  ,       0.07672932 )
m=c( "LAY","SIT","STAND","WALK", "WALKD","WALKUP"  )
colors=c(1:6)
png(file="mean bodyjerkaccelometer2.png")
barplot(h,names.arg = m,xlab = "Activity",ylab = "Body Jerk",main = "Mean Bodsy Jerk",col=colors,width = 100)
dev.off()




s=tapply(dataset$tBodyGyro.mean.X,dataset$Activity,mean)
print(s)
h=c(-0.016725340  ,     -0.038431317   ,    -0.026687141    ,   -0.034727577    ,   -0.084034543  ,      0.006824496  )
m=c( "LAY","SIT","STAND","WALK", "WALKD","WALKUP"  )
colors=c(1:6)
png(file="mean gyroscope.png")
barplot(h,names.arg = m,xlab = "Activity",ylab = "Gyroscope",main = "Mean Gyroscope",col=colors,width = 100)
dev.off()



s=tapply(dataset$tBodyGyroJerk.mean.X,dataset$Activity,mean)
print(s)
h=c( -0.10186430    ,    -0.09565211     ,   -0.09972928    ,    -0.09430071   ,     -0.07285323   ,     -0.11211746 )
m=c( "LAY","SIT","STAND","WALK", "WALKD","WALKUP"  )
colors=c(1:6)
png(file="mean jerkgyro.png")
barplot(h,names.arg = m,xlab = "Activity",ylab = "jerk Gyroscope",main = "Mean Jerk Gyroscope",col=colors,width = 100)
dev.off()



s=tapply(dataset$tBodyAccMag.mean,dataset$Activity,mean)
print(s)
h=c(-0.9411107     ,    -0.9546439   ,      -0.9541797      ,   -0.1679379  ,        0.1012497   ,      -0.1002041  )
m=c( "LAY","SIT","STAND","WALK", "WALKD","WALKUP"  )
colors=c(1:6)
png(file="mean Accelometer bodymagnet.png")
barplot(h,names.arg = m,xlab = "Activity",ylab = "Accelometer Body Magnet",main = "Accelometer Mean Body Magnet",col=colors,width = 100)
dev.off()



s=tapply(dataset$tBodyGyroMag.mean,dataset$Activity,mean)
print(s)
h=c(   -0.9384360     ,    -0.9467241  ,       -0.9421525    ,     -0.2748660     ,    -0.1297856 ,        -0.1782811  )
m=c( "LAY","SIT","STAND","WALK", "WALKD","WALKUP"  )
colors=c(1:6)
png(file="mean gyro body magnet.png")
barplot(h,names.arg = m,xlab = "Activity",ylab = "Gyroscope Body Magnet",main = "Gyroscope Body Magnet",col=colors,width = 100)
dev.off()


s=tapply(dataset$angle.X.gravityMean.,dataset$Activity,mean)
print(s)
h=c( 0.5202611  ,       -0.7060418     ,    -0.7741428   ,      -0.7618862    ,     -0.7808731    ,     -0.6379801  )
m=c( "LAY","SIT","STAND","WALK", "WALKD","WALKUP"  )
colors=c(1:6)
png(file="angle x.png")
barplot(h,names.arg = m,xlab = "Activity",ylab = "Angle X",main = "ANgle X",col=colors,width = 100)
dev.off()


s=tapply(dataset$angle.Y.gravityMean.,dataset$Activity,mean)
print(s)
h=c(-0.43594428   ,      0.00613979    ,     0.20981843      ,   0.21859934   ,      0.20018859    ,     0.27864194  )
m=c( "LAY","SIT","STAND","WALK", "WALKD","WALKUP"  )
colors=c(1:6)
png(file="angle y.png")
barplot(h,names.arg = m,xlab = "Activity",ylab = "Angle Y",main = "ANgle Y",col=colors,width = 100)
dev.off()

s=tapply(dataset$angle.Z.gravityMean.,dataset$Activity,mean)
print(s)
h=c( -0.42774882    ,    -0.08953195  ,       0.03174291   ,      0.05977064   ,      0.05586593     ,    0.12279380  )
m=c( "LAY","SIT","STAND","WALK", "WALKD","WALKUP"  )
colors=c(1:6)
png(file="angle Z.png")
barplot(h,names.arg = m,xlab = "Activity",ylab = "Angle Z",main = "ANgle Z",col=colors,width = 100)
dev.off()







# Scatter graph

training_set %>% ggplot(aes(x=tBodyAcc.mean.Y,
                            y=tBodyGyro.mean.Y,
                            color=Activity))+
  geom_point()+
  stat_smooth(se=0)+
  facet_wrap(~Activity)




training_set %>% ggplot(aes(x=tBodyAcc.energy.X,
                            y=tBodyAcc.energy.Z,
                            
                            color=Activity))+
  geom_point()+
  stat_smooth(se=6)






