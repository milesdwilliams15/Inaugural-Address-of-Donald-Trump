
## Analysis of Trump's Inaugural Address
address<-"Chief Justice Roberts, President Carter, President Clinton, President Bush, President Obama, fellow Americans, and people of the world: thank you.We, the citizens of America, are now joined in a great national effort to rebuild our country and to restore its promise for all of our people.Together, we will determine the course of America and the world for years to come.We will face challenges. We will confront hardships. But we will get the job done.Every four years, we gather on these steps to carry out the orderly and peaceful transfer of power, and we are grateful to President Obama and First Lady Michelle Obama for their gracious aid throughout this transition. They have been magnificent.Today’s ceremony, however, has very special meaning. Because today we are not merely transferring power from one Administration to another, or from one party to another – but we are transferring power from Washington, D.C. and giving it back to you, the American People.For too long, a small group in our nation’s Capital has reaped the rewards of government while the people have borne the cost.Washington flourished – but the people did not share in its wealth.Politicians prospered – but the jobs left, and the factories closed.The establishment protected itself, but not the citizens of our country.Their victories have not been your victories; their triumphs have not been your triumphs; and while they celebrated in our nation’s Capital, there was little to celebrate for struggling families all across our land.That all changes – starting right here, and right now, because this moment is your moment: it belongs to you.It belongs to everyone gathered here today and everyone watching all across America. This is your day. This is your celebration.And this, the United States of America, is your country.What truly matters is not which party controls our government, but whether our government is controlled by the people.January 20th 2017, will be remembered as the day the people became the rulers of this nation again. The forgotten men and women of our country will be forgotten no longer.Everyone is listening to you now.You came by the tens of millions to become part of a historic movement the likes of which the world has never seen before.At the center of this movement is a crucial conviction: that a nation exists to serve its citizens.Americans want great schools for their children, safe neighborhoods for their families, and good jobs for themselves.These are the just and reasonable demands of a righteous public.But for too many of our citizens, a different reality exists: Mothers and children trapped in poverty in our inner cities; rusted-out factories scattered like tombstones across the landscape of our nation; an education system, flush with cash, but which leaves our young and beautiful students deprived of knowledge; and the crime and gangs and drugs that have stolen too many lives and robbed our country of so much unrealized potential.This American carnage stops right here and stops right now.We are one nation – and their pain is our pain.  Their dreams are our dreams; and their success will be our success.  We share one heart, one home, and one glorious destiny.The oath of office I take today is an oath of allegiance to all Americans.For many decades, we’ve enriched foreign industry at the expense of American industry;Subsidized the armies of other countries while allowing for the very sad depletion of our military;We've defended other nation’s borders while refusing to defend our own;And spent trillions of dollars overseas while America's infrastructure has fallen into disrepair and decay.We’ve made other countries rich while the wealth, strength, and confidence of our country has disappeared over the horizon.One by one, the factories shuttered and left our shores, with not even a thought about the millions upon millions of American workers left behind.The wealth of our middle class has been ripped from their homes and then redistributed across the entire world.But that is the past. And now we are looking only to the future.We assembled here today are issuing a new decree to be heard in every city, in every foreign capital, and in every hall of power.From this day forward, a new vision will govern our land.From this moment on, it’s going to be America First.Every decision on trade, on taxes, on immigration, on foreign affairs, will be made to benefit American workers and American families.We must protect our borders from the ravages of other countries making our products, stealing our companies, and destroying our jobs.  Protection will lead to great prosperity and strength.I will fight for you with every breath in my body – and I will never, ever let you down.America will start winning again, winning like never before.We will bring back our jobs. We will bring back our borders.  We will bring back our wealth.  And we will bring back our dreams.We will build new roads, and highways, and bridges, and airports, and tunnels, and railways all across our wonderful nation.We will get our people off of welfare and back to work – rebuilding our country with American hands and American labor.We will follow two simple rules: Buy American and Hire American.We will seek friendship and goodwill with the nations of the world – but we do so with the understanding that it is the right of all nations to put their own interests first.We do not seek to impose our way of life on anyone, but rather to let it shine as an example for everyone to follow.We will reinforce old alliances and form new ones – and unite the civilized world against Radical Islamic Terrorism, which we will eradicate completely from the face of the Earth.At the bedrock of our politics will be a total allegiance to the United States of America, and through our loyalty to our country, we will rediscover our loyalty to each other.When you open your heart to patriotism, there is no room for prejudice.The Bible tells us, “how good and pleasant it is when God’s people live together in unity.”We must speak our minds openly, debate our disagreements honestly, but always pursue solidarity.When America is united, America is totally unstoppable.There should be no fear – we are protected, and we will always be protected.We will be protected by the great men and women of our military and law enforcement and, most importantly, we are protected by God.Finally, we must think big and dream even bigger.In America, we understand that a nation is only living as long as it is striving.We will no longer accept politicians who are all talk and no action – constantly complaining but never doing anything about it.The time for empty talk is over.Now arrives the hour of action.Do not let anyone tell you it cannot be done.  No challenge can match the heart and fight and spirit of America.We will not fail. Our country will thrive and prosper again.We stand at the birth of a new millennium, ready to unlock the mysteries of space, to free the Earth from the miseries of disease, and to harness the energies, industries and technologies of tomorrow.A new national pride will stir our souls, lift our sights, and heal our divisions.It is time to remember that old wisdom our soldiers will never forget: that whether we are black or brown or white, we all bleed the same red blood of patriots, we all enjoy the same glorious freedoms, and we all salute the same great American Flag.And whether a child is born in the urban sprawl of Detroit or the windswept plains of Nebraska, they look up at the same night sky, they fill their heart with the same dreams, and they are infused with the breath of life by the same almighty Creator.So to all Americans, in every city near and far, small and large, from mountain to mountain, and from ocean to ocean, hear these words:You will never be ignored again.Your voice, your hopes, and your dreams, will define our American destiny. And your courage and goodness and love will forever guide us along the way.Together, We Will Make America Strong Again.We Will Make America Wealthy Again.We Will Make America Proud Again.We Will Make America Safe Again.And, Yes, Together, We Will Make America Great Again. Thank you, God Bless You, And God Bless America."
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
library(syuzhet)
addressSent<-get_nrc_sentiment(addressText)
addressData<-cbind(addressText,addressSent)
addressData<-data.frame(addressData)
addressData$order<-c(1:89)
addressData$valence<-addressData$positive-addressData$negative
library(ggplot2)
windows()
ggplot(addressData,aes(x=order,y=valence)) + geom_line(size=1.5,color="red") + 
  geom_smooth(se=FALSE,color="blue",size=.75,span=.5) + theme_classic() +
  xlab("Time Line of the Address") + ylab("Sentiment Valence") + 
  ggtitle("Sentiment Valence in Donald Trump's Inaugural Address",
          subtitle = "Sentiment Analysis Conducted Via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic")) +
  theme(plot.title=element_text(size=14,hjust=0,face="bold")) +
  theme(text=element_text(family="serif")) +
  geom_hline(yintercept = 0,color="grey25",linetype=2,size=1)
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
windows()
multiplot(p1,p2,p3,p4,p5,p6,p7,p8,cols=4)