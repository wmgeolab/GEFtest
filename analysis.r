#load data frame
dataframe=read.csv("ex01.csv")
hist(dataframe$NEAR_DIST)
#create outcome variable, what we want to test impact on (change in assets over time)
#difference in years; now differentiate treated
dataframe$treated=0
#creating binary
dataframe[dataframe$NEAR_DIST<50000,]["treated"]=1
#lets remove null
dataframe_nonull=dataframe[complete.cases(dataframe[c("mergedata_udel_air_temp_v4_0_241", "mergedata_udel_precip_v4_01__286" )]),]
dataframe_nonull=dataframe_nonull[c("mergedata_udel_air_temp_v4_0_241", "Tanzania_2008_and_2010_value_23" , "mergedata_udel_precip_v4_01__286" , "treated")]
#keeps everything meeting criteria
#creating arbitrary threshold for distance, will be what dis<
#want to create pairs/matches of similar cases
library("MatchIt")
#writing match it model, tell it variable that defines control vs treated, also tell variables to match on; always picking things before any treatment
matchingmodel=matchit(treated~mergedata_udel_air_temp_v4_0_241+mergedata_udel_precip_v4_01__286, 
                      data=dataframe_nonull,
                      caliper=0.25)
#model- actual (biases in treatments)
matchingmodel$model
#gives model -- coefficient likelihood of projects occurring there
matcheddataframe=match.data(matchingmodel)
#matcheddata frame has 306 observations, half treatment and half control
#caliper - the more strict the matches are, chooses which ones to throwout, another arbitrary value
matchingmodel
#control more than distance away but look like treatment (values sim)
#change in assets, value diff
finalmodelanalysis=lm(Tanzania_2008_and_2010_value_23~treated+mergedata_udel_air_temp_v4_0_241+mergedata_udel_precip_v4_01__286,
                      data=matcheddataframe)
summary(finalmodelanalysis)
#that's the final model, interpret treated row, estimate for that most accurate (its significance too), saying cells closer to projects tended to have x value more