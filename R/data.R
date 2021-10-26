
#' Pre-trained Question Type Detector
#'
#' This model was pre-trained on 370 conversations to detect four different question types (switch, mirror, intro, follow-up)
#'
#' @format A pre-trained glmnet model
#' @source Huang et al., (2017) It doesn't hurt to ask: question-asking increases liking.
#'
"qDetect"


#' Pre-trained advice concreteness features
#'
#' For internal use only. This dataset demonstrates the ngram features that are used for the pre-trained question type model.
#'
#' @format A (truncated) matrix of ngram feature counts for alignment to the pre-trained question type model.
#' @source Huang et al., (2017) It doesn't hurt to ask: question-asking increases liking.
#'
"qtypeNG"
