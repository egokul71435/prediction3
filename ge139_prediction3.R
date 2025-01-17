# setting up the assignment
library(rpart)
train <- read.csv('airbnbTrain.csv')
test <- read.csv('airbnbTestStudents.csv')
boroughs <- read.csv('Boroughs.csv')
sub<-read.csv('submission_airbnb.csv')

# analyzing the datasets
nrow(train) # 6835 rows
nrow(test) # 25574 rows
nrow(boroughs) # 130 rows
nrow(sub) # 25574 rows
# goal -> predict if an airbnb is Good, Neutral, or Bad
colnames(train) # cols -> name, host_name, neighbourhood, floor, price, footage, avgreview, Deal
colnames(test) # cols -> id, name, host_name, neighbourhood, floor, price, footage, avgreview
colnames(boroughs) # cols -> neighbourhood, borough
colnames(sub) # cols -> id, Deal
unique_neighborhoods_train <- unique(train$neighbourhood)
unique_neighborhoods_test <- unique(test$neighbourhood)
length(unique_neighborhoods_train) # 88 unique neighborhoods
length(unique_neighborhoods_test) # 130 unique neighborhoods
unique_boroughs <- unique(boroughs$borough)
length(unique_boroughs) # 3 boroughs
# Neighborhoods in Brooklyn
table(boroughs[boroughs$borough=='Brooklyn',]$neighborhood)
length(table(boroughs[boroughs$borough=='Brooklyn',]$neighborhood)) # 47
# Neighborhoods in Queens
table(boroughs[boroughs$borough=='Queens',]$neighborhood)
length(table(boroughs[boroughs$borough=='Queens',]$neighborhood)) # 51
# Neighborhoods in Manhattan
table(boroughs[boroughs$borough=='Manhattan',]$neighborhood)
length(table(boroughs[boroughs$borough=='Manhattan',]$neighborhood)) # 32

# naive testing with rpart, based on floor, price, footage, and avgreview
trial_tree <- rpart(Deal ~ floor+price+footage+avgreview, data = train, method = "class")
trial_tree

train_test <- train
pred_test<-predict(trial_tree, newdata = train_test, type='class')
accuracy<-mean(pred_test==train$Deal)
cat('Accuracy of rpart prediction model')
accuracy
# this allows for an accuracy of 81.2%, which is decent (0.8122897)
# however, we can still consider the neighborhood of each entry
# this will likely improve results
# however, operating rpart on just neighborhoods is not ideal, since there are ~130 total neighborhoods
# so, it is more ideal to check by borough


boroughs_train <- rep('Brooklyn', nrow(train))
boroughs_train[train$neighbourhood == 'Battery Park City' | 
                 train$neighbourhood == 'Chelsea' | 
                 train$neighbourhood == 'Chinatown' | 
                 train$neighbourhood == 'Civic Center' | 
                 train$neighbourhood == 'East Harlem' | 
                 train$neighbourhood == 'East Village' | 
                 train$neighbourhood == 'Financial District' | 
                 train$neighbourhood == 'Flatiron District' | 
                 train$neighbourhood == 'Gramercy' | 
                 train$neighbourhood == 'Greenwich Village' | 
                 train$neighbourhood == 'Harlem' | 
                 train$neighbourhood == 'Hell\'s Kitchen' | 
                 train$neighbourhood == 'Inwood' | 
                 train$neighbourhood == 'Kips Bay' | 
                 train$neighbourhood == 'Little Italy' | 
                 train$neighbourhood == 'Lower East Side' | 
                 train$neighbourhood == 'Marble Hill' | 
                 train$neighbourhood == 'Midtown' | 
                 train$neighbourhood == 'Morningside Heights' | 
                 train$neighbourhood == 'Murray Hill' | 
                 train$neighbourhood == 'NoHo' | 
                 train$neighbourhood == 'Nolita' | 
                 train$neighbourhood == 'Roosevelt Island' | 
                 train$neighbourhood == 'SoHo' | 
                 train$neighbourhood == 'Stuyvesant Town' | 
                 train$neighbourhood == 'Theater District' | 
                 train$neighbourhood == 'Tribeca' | 
                 train$neighbourhood == 'Two Bridges' | 
                 train$neighbourhood == 'Upper East Side' | 
                 train$neighbourhood == 'Upper West Side' | 
                 train$neighbourhood == 'Washington Heights' | 
                 train$neighbourhood == 'West Village'] <- 'Manhattan'
boroughs_train[train$neighbourhood == 'Arverne' | 
                 train$neighbourhood == 'Astoria' | 
                 train$neighbourhood == 'Bay Terrace' | 
                 train$neighbourhood == 'Bayside' | 
                 train$neighbourhood == 'Bayswater' | 
                 train$neighbourhood == 'Belle Harbor' | 
                 train$neighbourhood == 'Bellerose' | 
                 train$neighbourhood == 'Breezy Point' | 
                 train$neighbourhood == 'Briarwood' | 
                 train$neighbourhood == 'Cambria Heights' | 
                 train$neighbourhood == 'College Point' | 
                 train$neighbourhood == 'Corona' | 
                 train$neighbourhood == 'Ditmars Steinway' | 
                 train$neighbourhood == 'Douglaston' | 
                 train$neighbourhood == 'East Elmhurst' | 
                 train$neighbourhood == 'Edgemere' | 
                 train$neighbourhood == 'Elmhurst' | 
                 train$neighbourhood == 'Far Rockaway' | 
                 train$neighbourhood == 'Flushing' | 
                 train$neighbourhood == 'Forest Hills' | 
                 train$neighbourhood == 'Fresh Meadows' | 
                 train$neighbourhood == 'Glendale' | 
                 train$neighbourhood == 'Hollis' | 
                 train$neighbourhood == 'Holliswood' | 
                 train$neighbourhood == 'Howard Beach' | 
                 train$neighbourhood == 'Jackson Heights' | 
                 train$neighbourhood == 'Jamaica' | 
                 train$neighbourhood == 'Jamaica Estates' | 
                 train$neighbourhood == 'Jamaica Hills' | 
                 train$neighbourhood == 'Kew Gardens' | 
                 train$neighbourhood == 'Kew Gardens Hills' | 
                 train$neighbourhood == 'Laurelton' | 
                 train$neighbourhood == 'Little Neck' | 
                 train$neighbourhood == 'Long Island City' | 
                 train$neighbourhood == 'Maspeth' | 
                 train$neighbourhood == 'Middle Village' | 
                 train$neighbourhood == 'Neponsit' | 
                 train$neighbourhood == 'Ozone Park' | 
                 train$neighbourhood == 'Queens Village' | 
                 train$neighbourhood == 'Rego Park' | 
                 train$neighbourhood == 'Richmond Hill' | 
                 train$neighbourhood == 'Ridgewood' | 
                 train$neighbourhood == 'Rockaway Beach' | 
                 train$neighbourhood == 'Rosedale' | 
                 train$neighbourhood == 'South Ozone Park' | 
                 train$neighbourhood == 'Springfield Gardens' | 
                 train$neighbourhood == 'St. Albans' | 
                 train$neighbourhood == 'Sunnyside' | 
                 train$neighbourhood == 'Whitestone' | 
                 train$neighbourhood == 'Woodhaven' | 
                 train$neighbourhood == 'Woodside'] <- 'Queens'
boroughs_train[train$neighbourhood == 'Bath Beach' | 
                 train$neighbourhood == 'Bay Ridge' | 
                 train$neighbourhood == 'Bedford-Stuyvesant' | 
                 train$neighbourhood == 'Bensonhurst' | 
                 train$neighbourhood == 'Bergen Beach' | 
                 train$neighbourhood == 'Boerum Hill' | 
                 train$neighbourhood == 'Borough Park' | 
                 train$neighbourhood == 'Brighton Beach' | 
                 train$neighbourhood == 'Brooklyn Heights' | 
                 train$neighbourhood == 'Brownsville' | 
                 train$neighbourhood == 'Bushwick' | 
                 train$neighbourhood == 'Canarsie' | 
                 train$neighbourhood == 'Carroll Gardens' | 
                 train$neighbourhood == 'Clinton Hill' | 
                 train$neighbourhood == 'Cobble Hill' | 
                 train$neighbourhood == 'Columbia St' | 
                 train$neighbourhood == 'Coney Island' | 
                 train$neighbourhood == 'Crown Heights' | 
                 train$neighbourhood == 'Cypress Hills' | 
                 train$neighbourhood == 'Downtown Brooklyn' | 
                 train$neighbourhood == 'DUMBO' | 
                 train$neighbourhood == 'Dyker Heights' | 
                 train$neighbourhood == 'East Flatbush' | 
                 train$neighbourhood == 'East New York' | 
                 train$neighbourhood == 'Flatbush' | 
                 train$neighbourhood == 'Flatlands' | 
                 train$neighbourhood == 'Fort Greene' | 
                 train$neighbourhood == 'Fort Hamilton' | 
                 train$neighbourhood == 'Gowanus' | 
                 train$neighbourhood == 'Gravesend' | 
                 train$neighbourhood == 'Greenpoint' | 
                 train$neighbourhood == 'Kensington' | 
                 train$neighbourhood == 'Manhattan Beach' | 
                 train$neighbourhood == 'Midwood' | 
                 train$neighbourhood == 'Mill Basin' | 
                 train$neighbourhood == 'Navy Yard' | 
                 train$neighbourhood == 'Park Slope' | 
                 train$neighbourhood == 'Prospect Heights' | 
                 train$neighbourhood == 'Prospect-Lefferts Gardens' | 
                 train$neighbourhood == 'Red Hook' | 
                 train$neighbourhood == 'Sea Gate' | 
                 train$neighbourhood == 'Sheepshead Bay' | 
                 train$neighbourhood == 'South Slope' | 
                 train$neighbourhood == 'Sunset Park' | 
                 train$neighbourhood == 'Vinegar Hill' | 
                 train$neighbourhood == 'Williamsburg' | 
                 train$neighbourhood == 'Windsor Terrace'] <- 'Brooklyn'
train2 <- train
train2$borough <- boroughs_train
trial_tree2 <- rpart(Deal ~ floor + price + footage + avgreview + borough, 
                    data = train2, 
                    method = "class")
trial_tree2
#trial_tree2 <- rpart(Deal ~ floor+price+footage+avgreview+borough, data = train2, method = "class")
#trial_tree2
train_test2 <- train2
pred_test2<-predict(trial_tree2, newdata = train_test2, type='class')
accuracy2<-mean(pred_test2==train2$Deal)
cat('Accuracy of rpart prediction model')
accuracy2
# the accuracy is no different than the initial naive testing without the boroughs considered

confusion_matrix <- table(Predicted = pred_test, Actual = train$Deal)
print(confusion_matrix)
# results of confusion matrix:
# Actual Bad Good Neutral
# Bad 1596   58     328
# Good 43   946     146
# Neutral 297 411 3010

# model appears to have a bias towards saying an entry is neutral 
# 20, 10, 0.005 # 0.8348208 acccuracy
control_params <- rpart.control(
  minsplit = 20,
  minbucket = 10,
  cp = 0.001
)
trial_tree3 <- rpart(Deal ~ floor + price + footage + avgreview + borough, 
                     data = train2, 
                     method = "class",
                     control = control_params)
trial_tree3
#trial_tree2 <- rpart(Deal ~ floor+price+footage+avgreview+borough, data = train2, method = "class")
#trial_tree2
pred_test3<-predict(trial_tree3, newdata = train_test2, type='class')
accuracy3<-mean(pred_test3==train2$Deal)
cat('Accuracy of rpart prediction model')
accuracy3 # 0.88515 accuracy
# this customization in rpart's control_params improved the model slightly by being more lennient
# changing the cp has a large impact on the final accuracy
# however decreasing too much might cause future overfitting

# next, we can try feature engineering:
train2$price_per_sqft <- train2$price / train2$footage
train2$price_review_ratio <- train2$price / train2$avgreview
train2$price_category <- cut(train2$price, breaks = c(0, 1000, 5000, 10000, Inf), labels = c("low", "medium", "high", "very high"))
train2$review_category <- cut(train2$avgreview, breaks = c(0, 2, 4, 5), labels = c("low", "average", "high"))
train3 <- train2
train_test3 <- train3

control_params2 <- rpart.control(
  minsplit = 20,
  minbucket = 10,
  cp = 0.001
)
trial_tree4 <- rpart(Deal ~ floor + price + footage + avgreview + borough + price_per_sqft + price_review_ratio + price_category + review_category, 
                     data = train3, 
                     method = "class",
                     control = control_params2)
trial_tree4
#trial_tree2 <- rpart(Deal ~ floor+price+footage+avgreview+borough, data = train2, method = "class")
#trial_tree2
pred_test4<-predict(trial_tree4, newdata = train_test3, type='class')
accuracy4<-mean(pred_test4==train3$Deal)
cat('Accuracy of rpart prediction model')
accuracy4 # 0.9044623 accuracy

# after feature engineering and customizing rpart, the accuracy reached is better

# now setup the submission:
boroughs_test <- rep('Brooklyn', nrow(test))
boroughs_test[test$neighbourhood == 'Battery Park City' | 
                 test$neighbourhood == 'Chelsea' | 
                 test$neighbourhood == 'Chinatown' | 
                 test$neighbourhood == 'Civic Center' | 
                 test$neighbourhood == 'East Harlem' | 
                 test$neighbourhood == 'East Village' | 
                 test$neighbourhood == 'Financial District' | 
                 test$neighbourhood == 'Flatiron District' | 
                 test$neighbourhood == 'Gramercy' | 
                 test$neighbourhood == 'Greenwich Village' | 
                 test$neighbourhood == 'Harlem' | 
                 test$neighbourhood == 'Hell\'s Kitchen' | 
                 test$neighbourhood == 'Inwood' | 
                 test$neighbourhood == 'Kips Bay' | 
                 test$neighbourhood == 'Little Italy' | 
                 test$neighbourhood == 'Lower East Side' | 
                 test$neighbourhood == 'Marble Hill' | 
                 test$neighbourhood == 'Midtown' | 
                 test$neighbourhood == 'Morningside Heights' | 
                 test$neighbourhood == 'Murray Hill' | 
                 test$neighbourhood == 'NoHo' | 
                 test$neighbourhood == 'Nolita' | 
                 test$neighbourhood == 'Roosevelt Island' | 
                 test$neighbourhood == 'SoHo' | 
                 test$neighbourhood == 'Stuyvesant Town' | 
                 test$neighbourhood == 'Theater District' | 
                 test$neighbourhood == 'Tribeca' | 
                 test$neighbourhood == 'Two Bridges' | 
                 test$neighbourhood == 'Upper East Side' | 
                 test$neighbourhood == 'Upper West Side' | 
                 test$neighbourhood == 'Washington Heights' | 
                 test$neighbourhood == 'West Village'] <- 'Manhattan'
boroughs_test[test$neighbourhood == 'Arverne' | 
                 test$neighbourhood == 'Astoria' | 
                 test$neighbourhood == 'Bay Terrace' | 
                 test$neighbourhood == 'Bayside' | 
                 test$neighbourhood == 'Bayswater' | 
                 test$neighbourhood == 'Belle Harbor' | 
                 test$neighbourhood == 'Bellerose' | 
                 test$neighbourhood == 'Breezy Point' | 
                 test$neighbourhood == 'Briarwood' | 
                 test$neighbourhood == 'Cambria Heights' | 
                 test$neighbourhood == 'College Point' | 
                 test$neighbourhood == 'Corona' | 
                 test$neighbourhood == 'Ditmars Steinway' | 
                 test$neighbourhood == 'Douglaston' | 
                 test$neighbourhood == 'East Elmhurst' | 
                 test$neighbourhood == 'Edgemere' | 
                 test$neighbourhood == 'Elmhurst' | 
                 test$neighbourhood == 'Far Rockaway' | 
                 test$neighbourhood == 'Flushing' | 
                 test$neighbourhood == 'Forest Hills' | 
                 test$neighbourhood == 'Fresh Meadows' | 
                 test$neighbourhood == 'Glendale' | 
                 test$neighbourhood == 'Hollis' | 
                 test$neighbourhood == 'Holliswood' | 
                 test$neighbourhood == 'Howard Beach' | 
                 test$neighbourhood == 'Jackson Heights' | 
                 test$neighbourhood == 'Jamaica' | 
                 test$neighbourhood == 'Jamaica Estates' | 
                 test$neighbourhood == 'Jamaica Hills' | 
                 test$neighbourhood == 'Kew Gardens' | 
                 test$neighbourhood == 'Kew Gardens Hills' | 
                 test$neighbourhood == 'Laurelton' | 
                 test$neighbourhood == 'Little Neck' | 
                 test$neighbourhood == 'Long Island City' | 
                 test$neighbourhood == 'Maspeth' | 
                 test$neighbourhood == 'Middle Village' | 
                 test$neighbourhood == 'Neponsit' | 
                 test$neighbourhood == 'Ozone Park' | 
                 test$neighbourhood == 'Queens Village' | 
                 test$neighbourhood == 'Rego Park' | 
                 test$neighbourhood == 'Richmond Hill' | 
                 test$neighbourhood == 'Ridgewood' | 
                 test$neighbourhood == 'Rockaway Beach' | 
                 test$neighbourhood == 'Rosedale' | 
                 test$neighbourhood == 'South Ozone Park' | 
                 test$neighbourhood == 'Springfield Gardens' | 
                 test$neighbourhood == 'St. Albans' | 
                 test$neighbourhood == 'Sunnyside' | 
                 test$neighbourhood == 'Whitestone' | 
                 test$neighbourhood == 'Woodhaven' | 
                 test$neighbourhood == 'Woodside'] <- 'Queens'
boroughs_test[test$neighbourhood == 'Bath Beach' | 
                 test$neighbourhood == 'Bay Ridge' | 
                 test$neighbourhood == 'Bedford-Stuyvesant' | 
                 test$neighbourhood == 'Bensonhurst' | 
                 test$neighbourhood == 'Bergen Beach' | 
                 test$neighbourhood == 'Boerum Hill' | 
                 test$neighbourhood == 'Borough Park' | 
                 test$neighbourhood == 'Brighton Beach' | 
                 test$neighbourhood == 'Brooklyn Heights' | 
                 test$neighbourhood == 'Brownsville' | 
                 test$neighbourhood == 'Bushwick' | 
                 test$neighbourhood == 'Canarsie' | 
                 test$neighbourhood == 'Carroll Gardens' | 
                 test$neighbourhood == 'Clinton Hill' | 
                 test$neighbourhood == 'Cobble Hill' | 
                 test$neighbourhood == 'Columbia St' | 
                 test$neighbourhood == 'Coney Island' | 
                 test$neighbourhood == 'Crown Heights' | 
                 test$neighbourhood == 'Cypress Hills' | 
                 test$neighbourhood == 'Downtown Brooklyn' | 
                 test$neighbourhood == 'DUMBO' | 
                 test$neighbourhood == 'Dyker Heights' | 
                 test$neighbourhood == 'East Flatbush' | 
                 test$neighbourhood == 'East New York' | 
                 test$neighbourhood == 'Flatbush' | 
                 test$neighbourhood == 'Flatlands' | 
                 test$neighbourhood == 'Fort Greene' | 
                 test$neighbourhood == 'Fort Hamilton' | 
                 test$neighbourhood == 'Gowanus' | 
                 test$neighbourhood == 'Gravesend' | 
                 test$neighbourhood == 'Greenpoint' | 
                 test$neighbourhood == 'Kensington' | 
                 test$neighbourhood == 'Manhattan Beach' | 
                 test$neighbourhood == 'Midwood' | 
                 test$neighbourhood == 'Mill Basin' | 
                 test$neighbourhood == 'Navy Yard' | 
                 test$neighbourhood == 'Park Slope' | 
                 test$neighbourhood == 'Prospect Heights' | 
                 test$neighbourhood == 'Prospect-Lefferts Gardens' | 
                 test$neighbourhood == 'Red Hook' | 
                 test$neighbourhood == 'Sea Gate' | 
                 test$neighbourhood == 'Sheepshead Bay' | 
                 test$neighbourhood == 'South Slope' | 
                 test$neighbourhood == 'Sunset Park' | 
                 test$neighbourhood == 'Vinegar Hill' | 
                 test$neighbourhood == 'Williamsburg' | 
                 test$neighbourhood == 'Windsor Terrace'] <- 'Brooklyn'
test$borough <- boroughs_test
test$price_per_sqft <- test$price / test$footage
test$price_review_ratio <- test$price / test$avgreview
test$price_category <- cut(test$price, breaks = c(0, 1000, 5000, 10000, Inf), labels = c("low", "medium", "high", "very high"))
test$review_category <- cut(test$avgreview, breaks = c(0, 2, 4, 5), labels = c("low", "average", "high"))

pre <- predict(trial_tree4, newdata = test, type='class')
sub$Deal <- pre
write.csv(sub, 'mysubmission.csv' , row.names = FALSE)
# the final submission used the final rpart model: trial_tree4
