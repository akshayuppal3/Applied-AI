;main template for storing user preference
(deftemplate user-pref
    (slot name)
    (slot range)
    (slot activity)
    (slot gender)
)

;template for question
(deftemplate question 
 (slot text)
 (slot type)
 (slot ident))

;template for answer
(deftemplate answer
  (slot ident)
  (slot answer)
 )


;perform validation based on the type of input
(deffunction is-type (?answer ?type) 
    "Checking the answer has correct type"
    (if (eq ?type male-female )then
        (return (or (eq ?answer male)(eq ?answer female)))
    else (if (eq ?type activity)then
        	(return (or (eq ?answer running)(eq ?answer training)(eq ?answer basketball)(eq ?answer soccer)(eq ?answer tennis)))      
    	  else (if(eq ?type range)then
                (return(or (or (eq ?answer A)(eq ?answer a))(or (eq ?answer B)(eq ?answer b))))
           	else(if(or(eq ?type age)(eq ?type height)(eq ?type weight))then
                 (return(numberp ?answer))))
    	 )
 	)
 )

;reads the answer provided the validation succeeds otherwise again request for correct input     
(deffunction ask-user(?question ?type)
    "Ask questiona and return answer"
    (bind ?answer "") 
    (while (not (is-type ?answer ?type))do
        (printout t ?question " " )
        (if (eq ?type male-female )then
            (printout t "( male or female ) "))
        (if (eq ?type range)then
            (printout t  crlf)
            (printout t "Which range of shoes price would you prefer," crlf " A. ($35-$150) " crlf " B. ($150-$350)" crlf "Please enter one of the range from above (A or B) ")) 
        (if (eq ?type activity )then
            (printout t  crlf)
            (printout t "<Shoes have following category based on activity>" crlf "( runnning, basketball, training, soccer, tennis ) "))
        (if (eq ?type height)then
            (printout t crlf "Please enter height in inches (1 foot = 12 inches) "))
        (if (eq ?type age )then
            (printout t crlf "Please enter your age (numeric value only) "))
        (if (eq ?type weight )then 
            (printout t crlf "Please enter your weight (in kgs) "))
        (bind ?answer (read))
     )        
    (return ?answer)
 )       

;Function to convert height in m
(deffunction convert-to-m(?ht)
  (bind ?ht (* ?ht 0.025)  )  
  (return ?ht) 
     )

;Function to calculate BMI
(deffunction calcBMI(?ht ?wt)
  (return(/ ?wt (* ?ht ?ht) ))  
 )

;Function to calculate BMI rating
(deffunction bmi-rating(?bmi)
   (if (> ?bmi 40)then
       (return 1)
      else(if (> ?bmi 35)then
        (return 2)
        else(if (> ?bmi 30)then
          (return 3)
           else(if (> ?bmi 25)then
            (return 4)
             else(if (> ?bmi 18.5)then
               (return 5)
                 else
                  (return 6)
                   )               
                 )           
               )
             )	    
           ) 
       )

;Measure fat level description based on bmi rating
(deffunction asgnFatLvlDesc(?FtLvlId)
	(if (= ?FtLvlId 1) then
        (return "Obese Class III")
        else (if (= ?FtLvlId 2) then
            (return "Obese Class II")
            else (if (= ?FtLvlId  3) then
                (return "Obese Class I")
                else (if (= ?FtLvlId 4) then
                    (return "Pre-Obese")
                    else (if (= ?FtLvlId 5) then
                        (return "Normal Range")
                        else (return "Under-Weight"))))))
)

;Measure fat percent in the person
(deffunction calcFatPercent(?sex ?age ?bmi)
	(if (= (str-compare ?sex "male") 0) then
        (bind ?val (+ (* 1.2 ?bmi) (* 0.23 ?age) -16.2))
        (return ?val)
        else
        (bind ?val (+ (* 1.2 ?bmi) (* 0.23 ?age) -5.4))
        (return ?val)
        )    
) 

;Measure the amount of weight to be lost or gained
(deffunction wgtToBeLostGain(?h ?w ?bm)
	(if (or (< ?bm 18.5) (> ?bm 24.9)) then
        (bind ?x (* 21.7 (* ?h ?h)))
        (return (- ?w ?x))
        else
        (return 0)
        )
)

;function for welcome message
(defrule welcome
(declare (salience 100))   
    =>
    (printout t crlf crlf " Please and enter your name ")
    (bind ?name (read))
    (assert ( answer (ident name)(answer ?name)))
    (printout t crlf "***************************" crlf crlf)
    (printout t "Hello " ?name "." crlf)
    (printout t "Welcome to Nike shoes recommender " crlf)
    (printout t "Please answer some question and " crlf)
    (printout t "I will recommend you some shoes based on your liking " crlf)
    (printout t crlf crlf "***************************" crlf crlf))

;rule that binds asserts the answer into the memeory
(defrule ask-question-by-id
 "Ask a question and assert the answer" 
(question (ident  ?id)(text ?text)(type ?type))
(not (answer (ident ?id)))
 ?ask <- (ask ?id)
 =>                        
 (bind ?answer (ask-user ?text ?type))
 (assert (answer (ident ?id)(answer ?answer)))
 (retract ?ask)
 (return)            
  )

;ask price range from the user
(defrule request-budget	
   
=>
  (assert(ask range))      
 ) 

;ask the gender
(defrule request-gender
 
 =>
 (assert (ask gender))    
) 


(defrule request-activity

=>
 (assert(ask activity))
)

(defrule request-weight
=>
    (assert(ask weight))    
)
(defrule request-age
=>    
    (assert(ask age))
)    
 
(defrule request-height
 
=>
   (assert(ask height))      
)
  
(defrule assert-user-fact

  (answer (ident gender)(answer ?i))
  (answer (ident activity)(answer ?a))
  (answer (ident range)(answer ?p))
  (answer (ident name)(answer ?name)) 
  (answer (ident height)(answer ?ht))       
    =>   
   (assert (user-pref (gender ?i)(activity ?a)(range ?p)(name ?name)))
 )     

;calculating fat-level in person
(defrule calculate-bmi-rating
   (answer (ident height)(answer ?ht))
   (answer (ident weight)(answer ?wt))
   (answer (ident gender)(answer ?sex))
   (answer (ident age)(answer ?age))  
   =>
   (bind ?ht (convert-to-m ?ht)) 
   (bind ?bmi(calcBMI ?ht ?wt))
   (bind ?bmi-r(bmi-rating ?bmi))
   (bind ?fat-level(asgnFatLvlDesc ?bmi-r)) 
   (bind ?fat-per(calcFatPercent ?sex ?age ?bmi))
   (bind ?wt-lose-gain(wgtToBeLostGain ?ht ?wt ?bmi))
   
    ;if you fall under one of the fat categories 
    (if (and (>= ?bmi-r 1)(<= ?bmi-r 4))then    
	   (printout t crlf crlf "*************************")
	   (printout t crlf "Those shoes will also look great while working out")
	   (printout t crlf "You are " ?fat-level " based on the BMI rating " crlf " and have " ?fat-per " of fat percentage "crlf" To be in perfect shape you would require to lose " ?wt-lose-gain " kg"
	     crlf " So start working out those fat with those nike shoes"  )  
	   (printout t crlf "***************************" crlf)
          ;if you are underweight 
         else (if (eq ?bmi-r 6) then
           (bind ?gain (abs ?wt-lose-gain)) 
	       (printout t crlf crlf "*************************")
		   (printout t crlf "Those shoes will also look great while working out")
		   (printout t crlf " You are  " ?fat-level " based on the BMI rating " crlf " To be in perfect shape you would require to gain "  ?gain " kg " crlf  "and dont forget to use your Nike shoes on to be in shape" )   
		   (printout t crlf "***************************" crlf)
            ;if you have perfect BMI range 
             else ( if (eq ?bmi-r 5)then
	            (printout t crlf crlf "*************************")
		        (printout t crlf "Those shoes will also look great while working out")
                (printout t crlf "You are in perfect shape based on your BMI index" crlf " But maintaining weight is also a challenge so keep working out on those nike shoes")    
                (printout t crlf "***************************" crlf)
                )	 
           )   
    )
 )   

/* Recommending Shoes to user based on his/her required activity and Budget
*/ 
 
(defrule recommend1

   (user-pref(gender male)(activity running)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  => 
   (printout t crlf " "?name " I would like to recommend following shows" crlf "1.Nike Air Max <$230>" "2.Nike Lunar Epic <$195> " "3.Nike Flyknit Racer <$160> " )
     )
(defrule recommend2

   (user-pref(gender male)(activity running)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name "I would like to recommend following shoes" crlf "1.Nike Air Max Fury <$99.97> 2.Nike Air zoom span <$94.47> 3.Nike revolution 3 <$60> " )
    )  
(defrule recommend3
 
   (user-pref(gender male)(activity basketball)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes" crlf "1.Nike LeBRon <$220> 2.Nike Air Jordan <$190> 3.Kobe AD <$180> " )
    )  
(defrule recommend4

   (user-pref(gender male)(activity basketball)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes" crlf "1.Nike Kyrie 3 ID <$140> 2.Nike Jordan Ultra Fly <$125> 3.Nike Zoom Evidence <$69.97> " crlf)
    ) 
(defrule recommend5

   (user-pref(gender male)(activity soccer)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name "I would like to recommend following shoes" crlf "1.Nike Vapor Untouchable ID <$250> 2.Nike Force Savage <$160> 3.Nike VApor Untouchable <$155> " crlf )
    )     
(defrule recommend6
  
   (user-pref(gender male)(activity soccer)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes" crlf "1.Nike Alpha Menace <$35> 2.Nike Vapor Shark <$48> 3.Nike Vapor Speed <$90> " crlf)
    )   
(defrule recommend7
  
  (user-pref(gender male)(activity training)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name "I would like to recommend following shoes" crlf "1.Nike Force Zoom <$150> 2.Nike Force Zoom trout 4 <$130> 3.Nike Force Air trout 4 <$85> " crlf )
    )     
(defrule recommend8
 
  (user-pref(gender male)(activity training)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name  " I would like to recommend following shoes" crlf "1.Nike Force <$45> 2.Nike Force Zoom <$60> 3.Nike Air <$100> " crlf)
    )      
(defrule recommend9
 
  (user-pref(gender male)(activity tennis)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes" crlf "1.Nike Court Zoom <$180> 2.Nike Court Lunar <$165> 3.Nike zoom cage 3 <$154.95> " crlf)
   )
(defrule recommend10
 
  (user-pref(gender male)(activity tennis)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes" crlf "1.Nike Court Line <$49.97> 2.Nike Court zoom cage 3 <$79.97> 3.Nike air zoom ultra <$100> " crlf)
   )
(defrule recommend11
   
  (user-pref(gender female)(activity running)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes- <for Women>" crlf "1.Nike Air max <$230> 2.Nike Air Max Flyknit<$190> 3.Nike air zoom pegasus 34 <$160> " crlf)
   )    
(defrule recommend12
  
  (user-pref(gender female)(activity running)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes- <for Women>" crlf "1.Nike Air max segment 2 <$79.97> 2.Nike air zoom <$125> 3.Nike air zoom  <$119.97> " crlf)
   )  
(defrule recommend13
   
  (user-pref(gender female)(activity basketball)(name ?name)(range ?r&:(or (eq ?r b)(eq ?r B))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes- <for Women>" crlf "1.Nike Air max <$230> 2.Nike Air Max Flyknit<$190> 3.Nike air zoom pegasus 34 <$160> " crlf)
   )    
(defrule recommend14
 
  (user-pref(gender female)(activity basketball)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes- <for Women>" crlf "1.Nike LEBRon Xiv 2 <$68> 2.Nike Kobe AD <$120> 3.Nike Kyrie 3  <$140> " crlf )
   )  
(defrule recommend15
  
  (user-pref(gender female)(activity soccer)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name "I would like to recommend following shoes- <for Women>" crlf "1.Nike Mercury Superfly <$340> 2.Nike Hypervenom phantom <$250> 3.Nike Magistra Orden <$180> " crlf )
   )  
(defrule recommend16
   
  (user-pref(gender female)(activity soccer)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes- <for Women>" crlf "1.Nike Hypervenom Phatal 3 <$145> 2.Nike Tiempo legacy <$130> 3.Nike Tiempo Ligera  <$70> " crlf)
   )  
(defrule recommend17
   
  (user-pref(gender female)(activity tennis)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes- <for Women>" crlf "1.Nike Court zoom <$180> 2.Nike Court zoom Vapor<$240> 3.Nike Court Air Life <$250> " crlf )
   )  
(defrule recommend18
 
  (user-pref(gender female)(activity tennis)(name ?name)(range ?r&:(or (eq ?r a)(eq ?r A))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes- <for Women>" crlf "1.Nike Court Phatal <$55> 2.Nike Court zoom <$44> 3.Nike Court Air Life <$145> " crlf)
   )  
(defrule recommend19
 
  (user-pref(gender female)(activity training)(name ?name)(range ?r&:(or (eq ?r B)(eq ?r b))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes- <for Women>" crlf "1.Nike Romaloes <$200> 2.Nike SFB <$165> 3.Nike fearless Flynit <$160> " crlf)
   ) 
(defrule recommend20

  (user-pref(gender female)(activity training)(name ?name)(range ?r&:(or (eq ?r A)(eq ?r a))))  
  =>       
  (printout t crlf " "?name " I would like to recommend following shoes- <for Women>" crlf "1.Nike zoom volley <$115> 2.Nike cheer scorpion <$85> 3.Nike air zoom condition <$64.97> " crlf)
   ) 

;fatcs for assertion of questions
(deffacts test-facts
(question (ident gender)(type male-female)
   (text "What is your gender? "))
(question (ident activity)(type activity)
   (text "What activities do you prefer "))
(question (ident range)(type range)
   (text "What is the rough budget for your shoes "))
(question (ident height)(type height)
   (text "What is your height "))       
(question (ident weight)(type weight)
   (text "  ")) 
(question (ident age)(type age)
   (text "  "))           
  )  

; Finally move the facts to the working memory and run it
	(reset)
	(run)


            