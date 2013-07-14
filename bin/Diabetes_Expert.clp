;; Code for the Diabetes Advisor System
;;Benjamin Mmari CSC 4000W 2013
;;Expert system for Risk assesment of Diabetes
;;13/07/2013


(deftemplate user
 	"Users personal Information"
    (slot name
    	(type string)
    	(default "")
    )
 )
(deftemplate question 
    (slot text) 
    (slot ident))

(deftemplate answer 
    (slot text)  
    (slot ident))


(deffacts MAIN::questions
    "The first batch of questions"
    (question (ident "q1") (text "What is your name"))
    )

	(deffunction ask-question (?question)
    	"Ask the user a question and return the answer"
    	(printout t ?question)
    	(return (read))
    )


	(defrule ask-question-by-id
   	"Ask a question and assert the subewuet answer, if any"
    ;;(declare (auto-focus TRUE))
    (MAIN::question (ident ?ident) (text ?text))
    (not (MAIN::answer (ident ?ident)))
     ?ask <- (MAIN::ask ?ident)
    =>
    (bind ?answer (ask-question ?text))
    (assert (MAIN::answer (ident ?ident) (text ?answer)))
   	(retract ?ask)
    )
