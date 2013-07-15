;; Code for the Diabetes Advisor System
;;Benjamin Mmari CSC 4000W 2013
;;Expert system for Risk assesment of Diabetes
;;13/07/2013
;;no input Validation as of yet, assuming happy day scenario
;;No fuzzy logic capabilities yet. need to be able to accpet null answers.


(deftemplate MAIN::user
 	"Users personal Information"
    (slot name
    	(type string)
    	(default "")
    )
 )
(deftemplate MAIN::question 
    (slot text) 
    (slot ident))
(deftemplate MAIN::answer 
    (slot text)  
    (slot ident)
)
(defrule MAIN::create-user
    (answer (ident q1) (text ?answer))
    =>
    	(assert (user (name ?answer)))
    )
(deftemplate MAIN::ask   
    (slot ident)
)
(deffacts MAIN::questions
    "The first batch of questions"
    (question (ident q1) (text "What is your name"))
    (question (ident q2) (text "What is your Gender (M/F)"))
	(question (ident q3) (text "Are you Diabetic (Y/N/?)"))
)
(deffunction MAIN::init ()
    (assert 
        (main-menu)
    	(ask (ident q3))
    	(ask(ident q2))
    	(ask (ident q1)))
    (return TRUE)
 )
(deffunction MAIN::ask-question (?question)
    	"Ask the user a question and return the answer"
    	(printout t ?question crlf)
    	(return (read))
 )

(defmodule ask)
(defrule ask::ask-question-by-id
   	"Ask a question and assert the subsequent answer, if any"
    (declare (auto-focus TRUE))
    (MAIN::question (ident ?ident) (text ?text))
    (not (MAIN::answer (ident ?ident)))
    ?question <- (MAIN::question (ident ?ident) (text ?text)) 
    ?ask <- (MAIN::ask (ident ?ident))
    =>
    (bind ?answer (MAIN::ask-question ?text))
    (assert (MAIN::answer (ident ?ident) (text ?answer)))
    (retract ?ask)
    (retract ?question)
    )

(defmodule menu)
(deffunction menu::init ()
    (assert (ask (ident q24)))
    (assert (ask (ident q23)))
    (assert (ask(ident q22)))
    (assert (ask (ident q21)))
    (return TRUE)
 )
(defrule calculate-bmi
    (answer (ident q23) (text ?weight))
    (answer (ident q24) (text ?height))
           
	=>
    (bind ?bmi (* (/ ?weight ?height) 100))
    (printout t "Your BMI is " ?bmi) 
    )
(defrule menu::initialize
    (risk-factors)
    =>
    (assert (question (ident q21) (text "Do you have a family history of Diabetes (Y/N/?)"))
    (question (ident q22) (text "Are you currently/ Were you recently Pregnant (Y/N/?)"))
	(question (ident q23) (text "What is your weight (KG)"))
    (question (ident q24) (text "What is your height(Meters)")))
    (menu::init)
 ) 	
(defrule menu::main-menu
    "Shows the different menu options for the risk assessment"
    (declare (auto-focus TRUE))
    (MAIN::main-menu)
    (user (name ?name))
	=>
		(printout t 
        	crlf crlf
   			"----------------------------------------------" crlf
			?name ", welcome to the Diabetes Risk Assesment Expert System. " crlf crlf
        	"Please answer as many questions as possible to receive a Diabetes risk assesment" crlf
        	"Also, please ask WHY whenever you feel the need to" crlf
			crlf
        	"Main Menu:" crlf 
			"  1. Lifestyle" crlf 
			"  2. Risk Factors" crlf 
			"  3. Symptoms" crlf
			"Enter your selection: " )
			(bind ?choice (read)) 
			(while (and (neq ?choice 1)(neq ?choice 2)(neq ?choice 3))
			(printout t "Invalid input, enter (1, 2 or 3): " ) 
			(bind ?choice (read)))
			(if (eq ?choice 1) then (assert (life-style)))
			(if (eq ?choice 2) then (assert (risk-factors)))
			(if (eq ?choice 3) then (assert (system-exit)))
)
(defrule menu::ask-question-by-id	
    "Ask a question and assert the subsequent answer, if any"
    (declare (auto-focus TRUE))
    (question (ident ?ident) (text ?text))
    (not (answer (ident ?ident)))
    ?question <- (question (ident ?ident) (text ?text)) 
    ?ask <- (ask (ident ?ident))
    =>
    (bind ?answer (MAIN::ask-question ?text))
    (assert (MAIN::answer (ident ?ident) (text ?answer)))
    (retract ?ask)
    (retract ?question)
    )
(reset)
(MAIN::init)
(focus MAIN ask menu)
(run)
