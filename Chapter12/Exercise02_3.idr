
record Votes where
       constructor MkVotes
       upvotes : Integer
       downvotes : Integer

record Article where
       constructor MkArticle
       title : String
       url : String
       score : Votes

initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

getScore : Article -> Integer
getScore article = upvotes (score article) - downvotes (score article)

addUpvote : Article -> Article
addUpvote = record { score->upvotes $= (+1) }

addDownvote : Article -> Article
addDownvote = record { score->downvotes $= (+1) }


BadSite : Article
BadSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)

GoodSite : Article
GoodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)


it_works : (getScore BadSite = -42, 
            getScore GoodSite = 94,
            getScore (addUpvote GoodSite) = 95)
it_works = (Refl, Refl, Refl)
