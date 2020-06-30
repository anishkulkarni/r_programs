#load data
> text_file <- readLines("file.txt")

#collapse the text
> text_file1 <- paste(text_file, collapse = " ")

#reduce all charactres to lower case
> clean_text <- tolower(text_file1)

> substr(clean_text, 1, 100)
[1] "\"character\" \"dialogue\" \"1\" \"threepio\" \"did you hear that?  they've shut down the main reactor.  we'l"

#remove punctuations
> clean_text1 <- gsub(pattern = "\\W", replace = " " ,clean_text)

> substr(clean_text1, 1, 100)
[1] " character   dialogue   1   threepio   did you hear that   they ve shut down the main reactor   we l"

#remove digits
> clean_text2 <- gsub(pattern = "\\d", replace = " ", clean_text1)

> substr(clean_text2, 1, 100)
[1] " character   dialogue       threepio   did you hear that   they ve shut down the main reactor   we l"

#import required libraries
> library(NLP)
> library(tm)
> library(wordcloud2)
> library(SnowballC)

#list sropwords
> stopwords()
[1] "i"          "me"         "my"         "myself"     "we"         "our"        "ours"      
[8] "ourselves"  "you"        "your"       "yours"      "yourself"   "yourselves" "he"        
[15] "him"        "his"        "himself"    "she"        "her"        "hers"       "herself"   
[22] "it"         "its"        "itself"     "they"       "them"       "their"      "theirs"    
[29] "themselves" "what"       "which"      "who"        "whom"       "this"       "that"      
[36] "these"      "those"      "am"         "is"         "are"        "was"        "were"      
[43] "be"         "been"       "being"      "have"       "has"        "had"        "having"    
[50] "do"         "does"       "did"        "doing"      "would"      "should"     "could"     
[57] "ought"      "i'm"        "you're"     "he's"       "she's"      "it's"       "we're"     
[64] "they're"    "i've"       "you've"     "we've"      "they've"    "i'd"        "you'd"     
[71] "he'd"       "she'd"      "we'd"       "they'd"     "i'll"       "you'll"     "he'll"     
[78] "she'll"     "we'll"      "they'll"    "isn't"      "aren't"     "wasn't"     "weren't"   
[85] "hasn't"     "haven't"    "hadn't"     "doesn't"    "don't"      "didn't"     "won't"     
[92] "wouldn't"   "shan't"     "shouldn't"  "can't"      "cannot"     "couldn't"   "mustn't"   
[99] "let's"      "that's"     "who's"      "what's"     "here's"     "there's"    "when's"    
[106] "where's"    "why's"      "how's"      "a"          "an"         "the"        "and"       
[113] "but"        "if"         "or"         "because"    "as"         "until"      "while"     
[120] "of"         "at"         "by"         "for"        "with"       "about"      "against"   
[127] "between"    "into"       "through"    "during"     "before"     "after"      "above"     
[134] "below"      "to"         "from"       "up"         "down"       "in"         "out"       
[141] "on"         "off"        "over"       "under"      "again"      "further"    "then"      
[148] "once"       "here"       "there"      "when"       "where"      "why"        "how"       
[155] "all"        "any"        "both"       "each"       "few"        "more"       "most"      
[162] "other"      "some"       "such"       "no"         "nor"        "not"        "only"      
[169] "own"        "same"       "so"         "than"       "too"        "very"

#remove stopwords
> clean_text3 <- removeWords(clean_text2,words = c(stopwords(),"ai","â"))

> substr(clean_text3, 1, 100)
[1] " character   dialogue       threepio     hear     ve shut   main reactor    ll  destroyed  sure     "

#remove single letters
> clean_text4  <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", clean_text3 )

> substr(clean_text4, 1, 100)
[1] " character   dialogue       threepio     hear     ve shut   main reactor    ll  destroyed  sure     "

#remove whitespace
> clean_text5 <- stripWhitespace(clean_text4)

> substr(clean_text5, 1, 100)
[1] " character dialogue threepio hear ve shut main reactor ll destroyed sure madness threepio re doomed "

#stem words
> clean_text6 <- wordStem(clean_text5)

> substr(clean_text6, 1, 100)
[1] " character dialogue threepio hear ve shut main reactor ll destroyed sure madness threepio re doomed "

#split text into individual words
> clean_text7 <- strsplit(clean_text6, " ")

> substr(clean_text7, 1, 100)
[1] "c(\"\", \"character\", \"dialogue\", \"threepio\", \"hear\", \"ve\", \"shut\", \"main\", \"reactor\", \"ll\", \"destroyed"

#get frequency of words
> word_freq <- table(clean_text7)
> head(word_freq)
clean_text6
aa      aaah abandoned     abide   ability 
1         2         1         1         1         1 

#simple word_cloud
> wordcloud2(word_freq)

#modified wordcloud
> wordcloud2(word_freq, minRotation = -pi/20, maxRotation = -pi/20, minSize = 1, rotateRatio = 1, color = "random-light", backgroundColor = "black")

#set directory of text data
> txt <- "/data"

#generate VCorpus
> tmtext <- VCorpus(DirSource(txt, encoding = "UTF-8"), readerControl=list(readPlain, language="en", load=TRUE))

#convert to character data type
> lapply(tmtext[1], as.character)

#remove whitespace
> tmtext <- tm_map(tmtext, stripWhitespace)

#convert data to lowercase
> tmtext <- tm_map(tmtext, content_transformer(tolower))

#remove stopwords
> tmtext <- tm_map(tmtext, removeWords, stopwords("english"))

#stem document contents
> tm_map(tmtext, stemDocument)

#remove punctuation
> tm_text <- tm_map(tmtext, removePunctuation)

#create document term matrix
> dtm <- DocumentTermMatrix(tmtext)

#inspect the created document term matrix
> inspect(dtm)
<<DocumentTermMatrix (documents: 3, terms: 6063)>>
  Non-/sparse entries: 9276/8913
Sparsity           : 49%
Maximal term length: 37
Weighting          : term frequency (tf)
Sample             :
  Terms
Docs    "ben" "han" "lando" "leia" "luke" "threepio" "vader" get going will
4.txt    82   153       0     57    254        119      41  53    61   43
5.txt    15   182      61    114    128         92      56  31    31   42
6.txt    18   124      40     56    112         90      43  25    10   66

#find associations
> findAssocs(dtm, "owen", 0.999)
$owen
ain't      back     back!    battle       big      call     droid    entire      fast      five 
        1         1         1         1         1         1         1         1         1         1 
    luke!    luke."     okay,      part   range." technical     thing  thousand 
        1         1         1         1         1         1         1         1 

#removal of sparse terms
> inspect(removeSparseTerms(dtm, 0.4))
<<DocumentTermMatrix (documents: 3, terms: 2100)>>
Non-/sparse entries: 5313/987
Sparsity           : 16%
Maximal term length: 14
Weighting          : term frequency (tf)
Sample             :
       Terms
Docs    "ben" "han" "lando" "leia" "luke" "threepio" "vader" get going will
  4.txt    82   153       0     57    254        119      41  53    61   43
  5.txt    15   182      61    114    128         92      56  31    31   42
  6.txt    18   124      40     56    112         90      43  25    10   66

#inspect for specific elements  
> inspect(DocumentTermMatrix(tmtext, list(dictionary = c("luke", "han", "oil"))))
<<DocumentTermMatrix (documents: 3, terms: 3)>>
Non-/sparse entries: 5/4
Sparsity           : 44%
Maximal term length: 4
Weighting          : term frequency (tf)
Sample             :
       Terms
Docs    han luke oil
  4.txt   0    6   1
  5.txt   5    5   0
  6.txt   0    3   0