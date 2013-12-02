# creates globals if they don't exist already
monitoredUsernames <- c("orfc", "oxfordfarming")
if (!exists('followerDescriptionWords')) followerDescriptionWords <- list()
if (!exists('followingDescriptionWords')) followingDescriptionWords <- list()
if (!exists('tweetWords')) tweetWords <- list()

# To create an OAuth session with Twitter do: 
# 1) twitCred <- preHandshake() 
# 2) twitCred$handshake() 
# 3) postHandshake(twitCred)
#
# To generate all wordclouds for the default accounts being monitored, do:
# 4) makeAllWordClouds()

preHandshake <- function () {
    require(twitteR)
    secret = read.csv("./secret.csv", colClasses= c("character", "character"))[1, ]
    twitCred <- OAuthFactory$new(consumerKey = secret$consumerKey, consumerSecret = secret$consumerSecret, requestURL = "https://api.twitter.com/oauth/request_token", accessURL = "http://api.twitter.com/oauth/access_token", authURL = "http://api.twitter.com/oauth/authorize")
    twitCred
}

postHandshake <- function (twitCred) {
    registerTwitterOAuth(twitCred)
}

fetchFollowersDescriptions <- function (username = "orfc") {
    user <- getUser(username)
    followers <- user$getFollowers()
    words <- Reduce(paste, sapply(followers, function (x) { x$description }))
}

fetchFollowingDescriptions <- function (username = "orfc") {
    user <- getUser(username)
    following <- user$getFriends()
    words <- Reduce(paste, sapply(following, function (x) { x$description }))
}

fetchTweets <- function (username = "orfc", noOfWeeks = 12) {
    oldestDate <- Sys.time() - noOfWeeks * 7 * 24 * 60 * 60 # 12 weeks
    tweets <- userTimeline(username, n = 1000, includeRts = TRUE)
    tweets <- Filter(function (t) { t$created >= oldestDate }, tweets)
    words <- Reduce(paste, sapply(tweets, function (x) { x$text }))
    # I remove all URLs (see http://stackoverflow.com/q/16003852/1218376 )
    words <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", words)
    # I remove all Twitter usernames (see http://stackoverflow.com/a/6351873/1218376 )
    words <- gsub("@([A-Za-z]+[A-Za-z0-9]+)", "", words)
}

fetchAll <- function (usernames = monitoredUsernames) {
    sapply(usernames, function (twitterUsername) {
        followerDescriptionWords[twitterUsername] <<- fetchFollowersDescriptions(twitterUsername)
        followingDescriptionWords[twitterUsername] <<- fetchFollowingDescriptions(twitterUsername)
        tweetWords[twitterUsername] <<- fetchTweets(twitterUsername)
        TRUE
    })
}

makeWordcloud <- function (words, filename = "wordcloud.png") {
    # thanks to http://onertipaday.blogspot.co.uk/2011/07/word-cloud-in-r.html
    # although it does not really explain what happens here :-(
    require(tm)
    require(RColorBrewer)
    require(wordcloud)
    # This is to remove punctuation but for hashtags. Package tm's 
    # 'removePunctuation' could not do that
    words <- gsub("[^A-Za-z0-9 #]", "", words)
    corpus <- Corpus(DataframeSource(data.frame(words)))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, function(x) removeWords(x, c("amp", "via", stopwords("english"))))
    tdm <- TermDocumentMatrix(corpus)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v), freq = v)
    pal <- brewer.pal(8, "Dark2")
    png(filename, width = 1280, height = 800)
    topWordMagnification <- 7
    wordcloud(d$word, d$freq, scale = c(topWordMagnification, topWordMagnification / 8), min.freq = 2, max.words = 100, random.order = T, rot.per = .15, colors = pal)
    dev.off()
}

makeAllWordClouds <- function (usernames = monitoredUsernames) {
    sapply(usernames, function (twitterUsername) {
        makeWordcloud(followerDescriptionWords[twitterUsername], paste0(twitterUsername, "_followers.png"))
        makeWordcloud(followingDescriptionWords[twitterUsername], paste0(twitterUsername, "_following.png"))
        makeWordcloud(tweetWords[twitterUsername], paste0(twitterUsername, "_tweetsAndRetweets.png"))
        TRUE
    })
    
}