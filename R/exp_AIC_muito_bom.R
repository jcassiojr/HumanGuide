# example of AIC use
# read in the film data that's been generated in Python
filmData = read.table("./data/filmData.txt",header=T)
attach(filmData)

# Get some basic descriptive statistics
table(Oscar)
summary(BoxOffice)
summary(Budget)
table(Country)
summary(Critics)
summary(Length)

# Look specifically at Oscar wins by country...
# Is there evidence that country affects the chances of winning?
table(Oscar,Country)

# Fit a null model, i.e., just the intercept.
# This is the simplest model possible, and describes the logit
# values by their mean, which translates as the overall 
# probability of winning an Oscar, regardless of the values of
# the predictor variables.
nullModel = glm ( Oscar ~ 1, family=binomial (link="logit"))

# Next fit a logistic regression model on the BoxOffice variable
# This asks whether or not the chances of winning an Oscar are 
# related to the the amount of money the film makes.
boxOfficeModel = glm( Oscar ~ BoxOffice, family=binomial (link="logit"))
summary(boxOfficeModel)
exp(boxOfficeModel$coefficients)

# Now fit the full model, i.e., predicting a film's Oscar 
# winning chances based on all five of the available 
# predictor variables.
fullModel = glm( Oscar ~ BoxOffice + Budget + Country + Critics + Length, family=binomial (link="logit"))
summary(fullModel)
exp(fullModel$coefficients)

# Ask for AIC values across all three of these models.
# Lower AIC values are better, as it's basically a 
# measure of distance from the data.  The full model
# wins the contest in this instance.
AIC(nullModel,boxOfficeModel,fullModel)


# The drop1 command asks what the AIC would be if we dropped
# each of the predictor variables in turn.  The results 
# suggest that length and critical reception could probably 
# go.  We drop length first and then re-check.
drop1(fullModel)

dropLModel = glm( Oscar ~ BoxOffice + Budget + Country + Critics, family=binomial (link="logit"))

# Now we re-examine things and then drop critical reception.
drop1(dropLModel)

dropLCModel = glm( Oscar ~ BoxOffice + Budget + Country, family=binomial (link="logit"))

# Checking this model indicates that there are no further
# variables to drop because the AIC would only increase.
drop1(dropLCModel)
summary(dropLCModel)
exp(dropLCModel$coefficients)

# To assess the success of our model, let's look at how well
# it predicts Oscar success.  First we pull the predicted logit
# scores out of our favoured model.
oscarLogits = predict(dropLCModel)

# Now we can use R's handy ifelse command to set a new variable
# "oscarPredictions" to 1 if the logit score is greater than 0,
# and to 0 if the logit score is lower than 0.  Recall that a 
# logit score of 0 corresponds to a probability of 0.5.
oscarPredictions = ifelse(oscarLogits > 0,1,0)

# Making a table of actual v. predicted Oscar success shows us
# that our model gets 215 films right and 85 films wrong.
# Better than guessing though!
table(oscarPredictions,Oscar)

# Let's close by working through an example of converting
# logit scores back into a probability, as failed to happen
# in the lecture!

# Let's say we're dealing with an American film of average
# box office takings ($80.74M) and budget ($54.24M).  
# We start with the intercept term: -6.309.
# We multiply the box office value by the appropriate coefficient
# That's 0.016353 x 80.74.
# We also multiply the budget value by its coefficient.
# That's 0.035859 x 54.24.

prediction = -6.309 + (0.016353 * 80.74) + (0.035859 * 54.24)
print(prediction)

# At this point we have the logit value.  We need to get it 
# back to a probability by exp(logit) to get odds, and then
# p = Odds / Odds+1.  And then we're done!  

odds = exp(prediction)

print (odds)

prob = odds / (odds + 1)

print (prob)


# If the same film was made in France, we get a bonus of 4.553517 
# to the logit score.  This changes everything

prediction = -6.309 + (0.016353 * 80.74) + (0.035859 * 54.24) + 4.553517
print(prediction)
odds = exp(prediction)
print (odds)
prob = odds / (odds + 1)
print (prob)




detach(filmData)
