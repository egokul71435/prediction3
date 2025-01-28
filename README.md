# Predicting the Quality of an Airbnb Listing Using R

## Background
---

This project is based on a dataset containing AirBNB listings within New York City, based in various neighborhoods and boroughs.
The goal was to build a model that can accurately determine if the quality of a model is good, neutral, or bad, as judged by the original dataset.

## Model Used
---

* The model used involves rpart, which enables the training data to be sorted into smaller buckets that determine the outcome (good, neutral, or bad) based on that bucket's criteria.
* The final tree built using rpart was built by looking at the floor, price, footage, avgreview, and borough of each listing in the training data 
* Feature engineering was done to add additional variables of interest, particularly price_per_sqft, price_review_ratio, price_category, and review_category

## Results
---

* The tree model, built using rpart achieved an accuracy of 0.9044623 on the test data and an accuracy of 0.8801 on a separate dataset of the same format

## Further Considerations
---

* Further feature engineering could be done, combining more variables, though this could make the model overfit in certain situations
* Additionally, tuning the rpart paramaters could improve the result further, though it may require some trial and error, and could too lead to fitting issues
