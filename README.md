# Inaugural-Address-of-Donald-Trump
Sentiment Analysis of Trump's Inaugural Address

The day is January 20th, 2017. Donald Trump has been sworn in as the 45th President of the United States. Like every one of his predecessors, Trump gave an inaugural address during today's ceremony. Lots of pundits will spend today closely analyzing, scrutinizing, praising, or whatever-ing Trump's speech. Few, however, will be applying text analysis techniques to examine the overall sentiment and emotion contained in Trump's address. I'm choosing to be a rebel by doing the latter...

### Preppig the Data for Analysis
First, after getting the transcript for the inaugural address ("as prepared for delivery") from [the White House's website](https://www.whitehouse.gov/inaugural-address), the text needs to be separated into a vector of sentences (there are 89 in total).

    address<-"Chief Justice Roberts, President Carter, President Clinton, President Bush, President Obama, fellow Americans,..."
    library(stringr)
    library(stringi)
    split_by_sentence <- function (text) {
  
      # split based on periods, exclams or question marks
      result <- unlist (strsplit (text, split = "[\\.!?]+"))
  
      # do not return empty strings
      result <- stri_trim_both (result)
      result <- result [nchar (result) > 0]
  
      # ensure that something is always returned
      if (length (result) == 0)
        result <- ""
  
      return (result)
    }
    addressText <- split_by_sentence(address)

Once this is done, the next step is to use a handy sentiment analysis tool:  get_nrc_sentiment().

    library(syuzhet)
    addressSent<-get_nrc_sentiment(addressText)
    addressData<-cbind(addressText,addressSent)
    addressData<-data.frame(addressData)

Finally, sentiment valence can be easily computed by subtracting the negative sentiment from the positive sentiment and saving the result as a vector caled "valence."

    addressData$valence<-addressData$positive-addressData$negative

Furthermore, for plotting purposes, I'll create a vector called "order," which will consist of a set of integers {1,2,3,...,89} that correspond with the order of sentences in "addressText."

    addressData$order<-c(1:89)

### Plotting the Data
With the data prepped and ready to go, plotting the data is the next, and most fun (!), step.

While there are many ways to plot out results from sentiment analysis, in this particular instance, I think using a line graph overlaid with a smoothed loess curve is a creat way to visualize sentiment and emotions in Trump's address.

First, let's plot the the overall sentiment valence in Trump's speech.

    library(ggplot2)
    ggplot(addressData,aes(x=order,y=valence)) + geom_line(size=1.5,color="red") + 
      geom_smooth(se=FALSE,color="blue",size=.75,span=.5) + theme_classic() +
      xlab("Time Line of the Address") + ylab("Sentiment Valence") + 
      ggtitle("Sentiment Valence in Donald Trump's Inaugural Address",
              subtitle = "Sentiment Analysis Conducted Via the NRC Emotion Lexicon") +
      theme(plot.subtitle=element_text(size=10, hjust=0, face="italic")) +
      theme(plot.title=element_text(size=14,hjust=0,face="bold")) +
      theme(text=element_text(family="serif")) +
      geom_hline(yintercept = 0,color="grey25",linetype=2,size=1)

![sentiment valence](https://cloud.githubusercontent.com/assets/23504082/22170912/f04e32c8-df49-11e6-8651-63251a913b96.jpeg)

It looks like a pretty positive speech. Sentiment only dips below zero a handful of times, but the overall sentiment is, on average, positive (certainly a good idea for any incoming president).

What about emotion?

    p1<-ggplot(addressData,aes(x=order,y=anger)) + geom_line(size=1.5,color="red") + 
      geom_smooth(se=FALSE,color="blue",size=.75,span=.5) + theme_classic() +
      xlab("Time Line of the Address") + ylab("Emotion Score") + 
      ggtitle("Anger in Donald Trump's\nInaugural Address") +
      theme(plot.subtitle=element_text(size=10, hjust=0, face="italic")) +
      theme(plot.title=element_text(size=14,hjust=0,face="bold")) +
      theme(text=element_text(family="serif")) 
    p2<-ggplot(addressData,aes(x=order,y=joy)) + geom_line(size=1.5,color="red") + 
      geom_smooth(se=FALSE,color="blue",size=.75,span=.5) + theme_classic() +
      xlab("Time Line of the Address") + ylab("Emotion Score") + 
      ggtitle("Joy in Donald Trump's\nInaugural Address") +
      theme(plot.subtitle=element_text(size=10, hjust=0, face="italic")) +
      theme(plot.title=element_text(size=14,hjust=0,face="bold")) +
      theme(text=element_text(family="serif")) 
    p3<-ggplot(addressData,aes(x=order,y=anticipation)) + geom_line(size=1.5,color="red") + 
      geom_smooth(se=FALSE,color="blue",size=.75,span=.5) + theme_classic() +
      xlab("Time Line of the Address") + ylab("Emotion Score") + 
      ggtitle("Anticipation in Donald Trump's\nInaugural Address") +
      theme(plot.subtitle=element_text(size=10, hjust=0, face="italic")) +
      theme(plot.title=element_text(size=14,hjust=0,face="bold")) +
      theme(text=element_text(family="serif")) 
    p4<-ggplot(addressData,aes(x=order,y=disgust)) + geom_line(size=1.5,color="red") + 
      geom_smooth(se=FALSE,color="blue",size=.75,span=.5) + theme_classic() +
      xlab("Time Line of the Address") + ylab("Emotion Score") + 
      ggtitle("Disgust in Donald Trump's\nInaugural Address") +
      theme(plot.subtitle=element_text(size=10, hjust=0, face="italic")) +
      theme(plot.title=element_text(size=14,hjust=0,face="bold")) +
      theme(text=element_text(family="serif")) 
    p5<-ggplot(addressData,aes(x=order,y=fear)) + geom_line(size=1.5,color="red") + 
      geom_smooth(se=FALSE,color="blue",size=.75,span=.5) + theme_classic() +
      xlab("Time Line of the Address") + ylab("Emotion Score") + 
      ggtitle("Fear in Donald Trump's\nInaugural Address") +
      theme(plot.subtitle=element_text(size=10, hjust=0, face="italic")) +
      theme(plot.title=element_text(size=14,hjust=0,face="bold")) +
      theme(text=element_text(family="serif")) 
    p6<-ggplot(addressData,aes(x=order,y=sadness)) + geom_line(size=1.5,color="red") + 
      geom_smooth(se=FALSE,color="blue",size=.75,span=.5) + theme_classic() +
      xlab("Time Line of the Address") + ylab("Emotion Score") + 
      ggtitle("Sadness in Donald Trump's\nInaugural Address") +
      theme(plot.subtitle=element_text(size=10, hjust=0, face="italic")) +
      theme(plot.title=element_text(size=14,hjust=0,face="bold")) +
      theme(text=element_text(family="serif")) 
    p7<-ggplot(addressData,aes(x=order,y=surprise)) + geom_line(size=1.5,color="red") + 
      geom_smooth(se=FALSE,color="blue",size=.75,span=.5) + theme_classic() +
      xlab("Time Line of the Address") + ylab("Emotion Score") + 
      ggtitle("Surprise in Donald Trump's\nInaugural Address") +
      theme(plot.subtitle=element_text(size=10, hjust=0, face="italic")) +
      theme(plot.title=element_text(size=14,hjust=0,face="bold")) +
      theme(text=element_text(family="serif")) 
    p8<-ggplot(addressData,aes(x=order,y=trust)) + geom_line(size=1.5,color="red") + 
      geom_smooth(se=FALSE,color="blue",size=.75,span=.5) + theme_classic() +
      xlab("Time Line of the Address") + ylab("Emotion Score") + 
      ggtitle("Trust in Donald Trump's\nInaugural Address") +
      theme(plot.subtitle=element_text(size=10, hjust=0, face="italic")) +
      theme(plot.title=element_text(size=14,hjust=0,face="bold")) +
      theme(text=element_text(family="serif")) 
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      library(grid)
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      numPlots = length(plots)
      # If layout is NULL, then use 'cols' to determine layout
      if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      if (numPlots==1) {
        print(plots[[1]])
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
    multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols=4)

![emotion](https://cloud.githubusercontent.com/assets/23504082/22170914/f76d1966-df49-11e6-846a-5fabe1bcb9a6.jpeg)

It looks like Trump seemed to really focus on building anticipation and joy and his speech, and gestures of trust seemed to remain fairly elevated for the duration of the speech.

There you go! A sentiment analysis of the President's inaugural address.
