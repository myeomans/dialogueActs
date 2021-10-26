
#' Question type detection
#' @description identify mirror/followup questions from Huang et al., 2017
#' @param by_turn data.frame of turn-level data
#' @return data.frame of turn-level data
#' @keywords internal
questionTypes<-function(by_turn,textcol="text",
                        speakerID="speakerID",
                        convoID="convoID"){
  # add checks for the right columns

  if(textcol!="text"){
    by_turn[,"text"]<-by_turn[,textcol]
  }
  if(convoID!="convoID"){
    by_turn[,"convoID"]<-by_turn[,convoID]
  }
  if(speakerID!="speakerID"){
    by_turn[,"speakerID"]<-by_turn[,speakerID]
  }
  convoID=NULL
  speakerID=NULL
  textcol=NULL

  if(!("question"%in%names(by_turn))) {
    by_turn$question<-1*(grepl("?",by_turn$text,fixed=T))
  }
  side.set<-c("turnspan","Qturnlen","Qprefrac","multiQ","prevQ","preSQ")
  ###########################################################################################
  # Construct question-level dataset
  ###########################################################################################
  newQuestionSet<-(by_turn$question>0)
  BB=rep(NA,sum(newQuestionSet))
  testQs<-data.frame(ID=BB,convoID=BB,turn=BB,
                     ques=BB,Qpre=BB,Qonly=BB,
                     prev1=BB,preS1=BB,prev2=BB)

  Q<-1
  pb<-txtProgressBar(min=1,max=nrow(testQs))
  for(x in 1:nrow(by_turn)){
    testQs[Q,"turn"]<-mark<-as.numeric(by_turn[x,"turn"])
    if(newQuestionSet[x]){
      #testQs[Q,c("ID","groupID")]<-by_turn[x,c(speakerID,convoID)]
      testQs[Q,]$ID<-by_turn[x,]$speakerID
      testQs[Q,]$convoID<-by_turn[x,]$convoID

      qblock<-by_turn %>%
        filter(convoID==by_turn[x,]$convoID) %>%
        filter(turn%in%((mark-4):mark))
      qtext<-turn_join(qblock[qblock$turn==mark,"text"])
      testQs[Q,c("ques","Qpre","Qonly")]<-Q_start_split(qtext)
      testQs[Q,"prev1"]<-paste0(qblock[qblock$turn==(mark-1),"text"],collapse=". ")
      testQs[Q,"preS1"]<-paste0(qblock[qblock$turn==(mark-2),"text"],collapse=". ")
      testQs[Q,"prev2"]<-paste0(qblock[qblock$turn==(mark-3),"text"],collapse=". ")
      Q<-Q+1
      setTxtProgressBar(pb, Q)
    }
  }

  testQs<-testQs[!is.na(testQs$ID),]
  testQs$turnspan<-log(testQs$turn)
  testQs$Qturnlen<-stringr::str_count(testQs$ques, "[[:alpha:]]+")
  testQs$Qprefrac<-stringr::str_count(testQs$Qpre, "[[:alpha:]]+")/testQs$Qturnlen
  testQs$multiQ<-1*(textcounter("?",testQs$ques)>1)
  testQs$prevQ<-textcounter("?",testQs$prev1)
  testQs$preSQ<-textcounter("?",testQs$preS1)
  #
  test.ngrams<-doc2concrete::ngramTokens(testQs$ques,ngrams=1:3,
                                         vocabmatch=dialogueActs::qtypeNG)
  test.exes<-as.matrix(scale(cbind(test.ngrams,testQs[,side.set])))
  test.exes[is.na(test.exes)]<-0
  fit.test<-predict(dialogueActs::qTypeDetector, newx=test.exes,s="lambda.min",type="response")[,,1]
  ####################################################################
  # Record predictions
  ####################################################################

  testQs$switchQs<-fit.test[,"full"]
  testQs$followQs<-fit.test[,"followup"]
  testQs$mirrorQs<-fit.test[,"mirror"]
  testQs$introQs<-fit.test[,"intro"]


  postQspan<-function(text){
    postSpan<-gregexpr("?",paste(rev(strsplit(text,"")[[1]]),collapse=""),fixed=T)[[1]][1]
    return(postSpan)
  }

  testQs$rhetQs<-1*(unlist(lapply(testQs$ques, postQspan))>40)

  #
  # bowSimCalc<-function(x,y){
  #   if(length(y)>1){
  #     stop("One ground truth at a time!")
  #   }
  #   x<-DTMtools::ctxpand(x)
  #   y<-DTMtools::ctxpand(y)
  #   counts<-quanteda::dfm(c(x,y),stem=T,tolower=T,remove=quanteda::stopwords())
  #   sims<-as.matrix(quanteda::textstat_simil(counts, method="cosine"))
  #   return(sims[nrow(sims),-ncol(sims)])
  # }
  #
  # by_turn<- by_turn %>%
  #   left_join(read_csv("shortlist.csv"), by=c("topic"))
  #
  # testQs$topicMatch<-unlist(lapply(1:nrow(by_turn),function(x) bowSimCalc(by_turn[x,]$text,by_turn[x,]$message)))#

  .qNames<-paste0(c("switch","follow","mirror","repair","rhet","intro"),"Qs")

  ###########################################################################################
  repairs<-c("what?","sorry?","excuse me?","huh?","hmm?","hm?","who?","pardon?",
             "say again?","say it again?","what’s that?","what is that?")

  testQs$repairQs<-0
  for(r in repairs){
    testQs$repairQs<-testQs$repairQs+endsWith(tolower(testQs$ques),r)
  }
  testQs$repairQs[is.na(testQs$repairQs)]
  testQs[testQs$repairQs>0,.qNames[.qNames!="repairQs"]]<-0
  testQs[testQs$rhetQs>0,.qNames[.qNames!="rhetQs"]]<-0

  qFeatures<-testQs %>%
    select(convoID,turn,all_of(.qNames))

  by_turn<-by_turn %>%
    left_join(qFeatures,by=c("convoID","turn")) %>%
    mutate(introQs=replace_na(introQs,0),
           switchQs=replace_na(switchQs,0),
           followQs=replace_na(followQs,0),
           mirrorQs=replace_na(mirrorQs,0),
           repairQs=replace_na(repairQs,0),
           rhetQs=replace_na(rhetQs,0))

  return (by_turn)
}

#' Feature Counter
#' @description background function to load
#' @param counted character vector what are we looking for
#' @param texts character vector where are we looking
#' @param fixed pass to gregexpr
#' @return numeric vector of counts
#' @keywords internal
textcounter<-function (counted, texts, fixed=T) {
  counts<-rep(0,length(texts))
  for (x in counted){
    counts<-counts+sapply(gregexpr(x, texts, fixed = fixed), function(z) ifelse(z[1] == (-1), 0, length(z)))
  }
  return(counts)
}

#' Question segmenter
#' @description background function to load
#' @param texts character vector where are we looking
#' @return list with three items - the original text, the pre-question statement, and the question
#' @keywords internal
Q_start_split<-function(text){
  SD<-stringr::str_split(text,stringr::boundary("sentence"))[[1]]
  if (length(SD)>1){
    prelude<-(cumsum(textcounter("?",SD))==0)
    prelude[length(prelude)]<-F
    return(list(text=text,
                start=paste("",SD[prelude],collapse=" "),
                ques=paste(SD[!prelude],collapse=" ")))
  }
  else return(list(text=text,start="",ques=text))
}

#' Turn joiner
#' @description background function to load
#' @param textlist character vector turns to be joined
#' @return character joined turns
#' @keywords internal
turn_join<-function(textlist){
  tj<-paste0(textlist,collapse=". ")
  tj<-gsub("?.","?",tj,fixed=T)
  tj<-gsub("..",".",tj,fixed=T)
  tj<-gsub("!.","!",tj,fixed=T)
  return(tj)
}
