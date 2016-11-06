##You will get following four at : dev.twitter.com
consumerKey = "izPjRHnjOaYWG9lQKusQtzaiD"
consumerSecret = "feyulEBoWGPw34JAO5LvmqluHJHMj1KCR8gXNqaZOKGZ1020qU"
accessToken = "711707724027973632-c3Es4BY5oZsGEgluy6WVJD1yEbvmlyi"
accessTokenSecret = "xLsbOcbcTWI4zkClbhUDqdGyxSTUYvCnlnRYOAn5yEbZB"

library(twitteR)##You need to install in Rstudio
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
tweets = searchTwitter('#homecooking', n=200)
tweetsDF = twListToDF(tweets)
write.csv(tweetsDF, file="tweetsDF.csv")##This "tweetsDF.csv" file will be in the root of your project
