;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart Mattress System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import nrc.fuzzy.*)
(import nrc.fuzzy.jess.*)
(load-package nrc.fuzzy.jess.FuzzyFunctions)

;template for question
(deftemplate question
    (slot text)
    (slot type)
    (slot ident))

;template for answer
(deftemplate answer
    (slot ident)
    (slot answer(type float) )
    )
;perform validation based on the type of input
(deffunction is-type (?answer ?type) 
    "Checking the answer has correct type"
    (if(eq ?type range)then
                (return(or (eq ?answer 1)(eq ?answer 2)(eq ?answer 3)))
           	else(if(or (eq ?type height)(eq ?type weight)(eq ?type temperature))then
                 (return(numberp ?answer)))
    	 )
 	)
      
;reads the answer provided the validation succeeds otherwise again request for correct input
(deffunction ask-user(?question ?type)
    "Ask questiona and return answer"
    (bind ?answer "")
    (while (not (is-type ?answer ?type))do
        (printout t ?question " " )
        (if (eq ?type height)then
            (printout t crlf "Please enter height in inches (1 foot = 12 inches) "))
        (if (eq ?type age )then
            (printout t crlf "Please enter your age (numeric value only) "))
        (if (eq ?type weight )then
            (printout t crlf "Please enter your weight (in kgs) "))
        (if (eq ?type temperature)then
            (printout t crlf "Please enter current temperature readings (-35 to 100)"))
        (if (eq ?type range)then
            (printout t crlf "1.Awake  2.Drowsy  3.Sleepy "))  
        (bind ?answer (read))
        )
    (return ?answer)
    )
;Function to convert height in m
(deffunction convert-to-m(?ht)
    (bind ?ht (* ?ht 0.025)  )
    (return ?ht)
    )
;Fucntion to calculate body surface area
(deffunction bsa (?ht ?wt)
    return (/ (sqrt (* ?ht ?wt) ) 6 )
    )
;Function to calculate pressure on the mattress
(deffunction pressure (?wt ?bsa)
    return (/ (* ?wt 10) ?bsa)
    )


(defglobal ?*tempFvar* = (new nrc.fuzzy.FuzzyVariable "temperature" -30.0 100.0 "Degrees C"))
(defglobal ?*hotChangeFvar* = (new nrc.fuzzy.FuzzyVariable "hotChange" -5.0 5.0 ""))
(defglobal ?*coldChangeFvar* = (new nrc.fuzzy.FuzzyVariable "coldChange" -5.0 5.0 ""))
(defglobal ?*pressureFvar* = (new nrc.fuzzy.FuzzyVariable "pressure" 0.0 5000.0 "Pascals" ))

; Change 2
(defglobal ?*angleFvar* = (new nrc.fuzzy.FuzzyVariable "bedangle" -5.0 5.0 "Degree"))
(defglobal ?*sleepFvar* = (new nrc.fuzzy.FuzzyVariable "sleepindex" 0 10 "SleepScale")) 
; Change 2

(defglobal ?*rulesThatFired* = "")

;(reset)
;function for welcome message
(defrule welcome
(declare (salience 100))   
    =>
    (printout t crlf crlf " Please and enter your name ")
    (bind ?name (read))
    (assert ( answer (ident name)(answer ?name)))
    (printout t crlf "**************************************************************" crlf crlf)
    (printout t "Hello " ?name "." crlf)
    (printout t "Welcome to Smart Mattress control system " crlf)
    (printout t "Just enter your details " crlf)
    (printout t "So that Mattress can be customised based on your prefernces " crlf)
    (printout t "The process will take less than 5min for caibration " crlf)
    (printout t crlf crlf "*********************************************************" crlf crlf))


(defrule init
    (answer (ident pressurecalc)(answer ?p))
    (answer (ident temperature)(answer ?t))
    =>
    (?*tempFvar* addTerm "too-cold" (new RightLinearFuzzySet 0.0 5.0))
    (?*tempFvar* addTerm "cold" (new TrapezoidFuzzySet 0.0 5.0 10.0 15.0))
    (?*tempFvar* addTerm "warm" (new TrapezoidFuzzySet 10.0 15.0 20.0 25.0))
    (?*tempFvar* addTerm "hot" (new TrapezoidFuzzySet 20.0 25.0 30.0 35.0))
    (?*tempFvar* addTerm "too-hot" (new LeftLinearFuzzySet 30.0 35.0))
    ;(?*tempFvar* addTerm "hot" "not too-hot and (not warm)")
     
    ;Create a fuzzy set for pressure
    (?*pressureFvar* addTerm "defaultp" (new PIFuzzySet ?p 150.0))
    
    ;Set max Threshold value of fuzzy match
    (call nrc.fuzzy.FuzzyValue setMatchThreshold 0.0)
    
    ;Heat Big
    (?*hotChangeFvar* addTerm "HB" (new LeftLinearFuzzySet 0.0 5.0 ) )
    ;Heat Medium
    (?*hotChangeFvar* addTerm "HM" (new TriangleFuzzySet 2.0 2.5 3.0))
    ;Heat Small
    (?*hotChangeFvar* addTerm "HS" (new TriangleFuzzySet 0.25 0.5 0.75))
    ;HotZero
    (?*hotChangeFvar* addTerm "Z" (new PIFuzzySet 0 0.5))
    
    ;Cold Big
    (?*coldChangeFvar* addTerm "CB" (new RightLinearFuzzySet -5.0  0.0) )
    ;Cold Medium 
    (?*coldChangeFvar* addTerm "CM" (new TriangleFuzzySet -3.0 -2.5 -2.0))
    ;Cold Small
    (?*coldChangeFvar* addTerm "CS" (new TriangleFuzzySet -0.75 -0.5  -0.25 ))
    ;ColdZero
    (?*coldChangeFvar* addTerm "Z" (new PIFuzzySet 0 0.5))

    ;Change 2    
    ;Fuzzy sets for sleep index
	(?*sleepFvar* addTerm "awake" (new RightLinearFuzzySet 3.0 4.0))
    (?*sleepFvar* addTerm "drowsy" (new TrapezoidFuzzySet 3.0 4.0 6.0 7.0))
    (?*sleepFvar* addTerm "sleepy" (new LeftLinearFuzzySet 6.0 7.0 ) )
    
    ;Fuzzy Set for angle change
    (?*angleFvar* addTerm "CP" (new TriangleFuzzySet 0.0 2.5 5.0))
    (?*angleFvar* addTerm "CN" (new TriangleFuzzySet -5.0 -2.5 0))
    (?*angleFvar* addTerm "CZ" (new TriangleFuzzySet 2.0 2.5 3.0))
      
    ;current angle reading by the system
    (assert (currentang 150))    	;its value can be changed
    ;End Change 2
  
    ;sensor_readings of pressure
    (?*pressureFvar* addTerm "currentp" (new SingletonFuzzySet 400.0)) ; its value can be changed
    (assert (currentp (new nrc.fuzzy.FuzzyValue ?*pressureFvar* "currentp"))) 
    	
    ;Asserting facts
    ;Adding the value of current temperature by the sensor
    (?*tempFvar* addTerm "temp" (new SingletonFuzzySet ?t))
    (assert (temp (new nrc.fuzzy.FuzzyValue ?*tempFvar* "temp")))
    
    /* (?*pressureFvar* addTerm "currentp" (new SingletonFuzzySet ?p1))
    (assert (currentp (new nrc.fuzzy.FuzzyValue ?*pressureFvar* "currentp"))) */
  
    ;(printout t (call (new nrc.fuzzy.FuzzyValue ?*tempFvar* "too-cold") plotFuzzyValue "+") crlf)
      )

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

;ask weight of the person
(defrule request-weight
    =>
    (assert (ask weight)))

;ask height if the user
(defrule request-height
    =>
    (assert (ask height)) )

;check pressure readings
/*(defrule request-pressure
    =>
    (assert (ask pressure))
    ) */

;check temperature readings
(defrule request-temperature
    =>
    (assert (ask temperature))
    )

;Change 2
(defrule request-sleepyValue
    ;(temperature calibrated)
    =>
    (assert (ask sleepiness))
   )

;function to calculate default pressure of particular person
(defrule calculate-pressure
    (answer (ident height)(answer ?ht))
    (answer (ident weight)(answer ?wt))
    =>
    (bind ?ht (convert-to-m ?ht))
    (bind ?bsa (bsa ?ht ?wt))
    (bind ?p (pressure ?wt ?bsa))
    (assert (answer (ident pressurecalc)(answer ?p)))
    )

;Check if pressure is true
(defrule pressure_true
    (currentp ?p&:(fuzzy-match "defaultp" ?p))
     =>
   ;(printout t (call (new nrc.fuzzy.FuzzyValue ?*pressureFvar* "current-pressure") plotFuzzyValue "()") crlf)
    (printout t "pressure matched" crlf)
    (assert (pressureVaid true))
    )

(defrule too-cold
    (pressureVaid true)
    (temp ?t&:(fuzzy-match "too-cold" ?t))
    =>
    (assert (change_heat(new FuzzyValue ?*hotChangeFvar* "HB")))
    (assert (change_cool(new FuzzyValue ?*coldChangeFvar* "Z")))
    (bind ?*rulesThatFired* (str-cat ?*rulesThatFired*
        "!Rule: if Temp too cold and Pressure Valid then change Heat HB fires%")
      )
    )

(defrule cold_n_too-cold
    (pressureVaid true)
    (temp ?t&:(fuzzy-match "cold" ?t ))
    (temp ?t&:(fuzzy-match "too-cold" ?t))
    =>
    (assert (change_heat(new FuzzyValue ?*hotChangeFvar* "HM")))
    (assert (change_cool(new FuzzyValue ?*coldChangeFvar* "Z")))
     (bind ?*rulesThatFired* (str-cat ?*rulesThatFired*
        "!Rule: if Temp cold & too-cold and Pressure Valid then change Heat HM fires%")
      )
    )

(defrule cold
    (pressureVaid true)
    (temp ?t&:(fuzzy-match "cold" ?t ))
    =>
    (assert (change_heat(new FuzzyValue ?*hotChangeFvar* "HM")))
    (assert (change_cool(new FuzzyValue ?*coldChangeFvar* "Z")))
     (bind ?*rulesThatFired* (str-cat ?*rulesThatFired*
        "!Rule: if Temp cold and Pressure Valid then change Heat HM fires%")
      )
    )

(defrule warm_n_cold
    (pressureVaid true)
    (temp ?t&:(fuzzy-match ?t "warm"))
    (temp ?t&:(fuzzy-match ?t "cold"))
    =>
    (assert (change_cool(new FuzzyValue ?*coldChangeFvar* "Z")))
    (assert (change_heat(new FuzzyValue ?*hotChangeFvar* "HS")))
     (bind ?*rulesThatFired* (str-cat ?*rulesThatFired*
        "!Rule: if Temp warm & hot and Pressure Valid then change Cool HS fires%")
      )
    (printout t "warm hot" crlf)
    )

(defrule warm
    (pressureVaid true)
    (temp ?t&:(fuzzy-match ?t "warm"))
    =>
    (assert (change_heat(new FuzzyValue ?*hotChangeFvar* "Z" )))
    (assert (change_cool(new FuzzyValue ?*coldChangeFvar* "Z")))
     (bind ?*rulesThatFired* (str-cat ?*rulesThatFired*
        "!Rule: if Temp warm and Pressure Valid then change Heat Z fires%")
      )
    (printout t "warm" crlf)
    )

(defrule warm_n_hot
    (pressureVaid true)
    (temp ?t&:(fuzzy-match ?t "warm"))
    (temp ?t&:(fuzzy-match ?t "hot"))
    =>
    (assert (change_cool(new FuzzyValue ?*coldChangeFvar* "CS")))
    (assert (change_heat(new FuzzyValue ?*hotChangeFvar* "Z")))
     (bind ?*rulesThatFired* (str-cat ?*rulesThatFired*
        "!Rule: if Temp warm & hot and Pressure Valid then change Cool CS fires%")
      )
    (printout t "warm hot" crlf)
    )

(defrule hot
    (pressureVaid true)
    (temp ?t&:(fuzzy-match ?t "hot"))
    =>
    (assert (change_cool(new FuzzyValue ?*coldChangeFvar* "CM")))
    (assert (change_heat(new FuzzyValue ?*hotChangeFvar* "Z")))
     (bind ?*rulesThatFired* (str-cat ?*rulesThatFired*
        "!Rule: if Temp hot and Pressure Valid then change Cool CM fires%")
      )
    (printout t "HOT" crlf)
    )

(defrule hot_n_too-hot
    (pressureVaid true)
    (temp ?t&:(fuzzy-match "hot" ?t ))
    (temp ?t&:(fuzzy-match "too-hot" ?t))
    =>
    (assert (change_cool(new FuzzyValue ?*coldChangeFvar* "CM")))
    (assert (change_heat(new FuzzyValue ?*hotChangeFvar* "Z")))
     (bind ?*rulesThatFired* (str-cat ?*rulesThatFired*
        "!Rule: if Temp too cold and Pressure Valid then change Cool CM fires%")
      )
    (printout t "cold and too cold" crlf)
    )

(defrule too-hot
    (pressureVaid true)
    (temp ?t&:(fuzzy-match ?t "too-hot"))
    =>
    (assert (change_cool(new FuzzyValue ?*coldChangeFvar* "CB")))
    (assert (change_heat(new FuzzyValue ?*hotChangeFvar* "Z")))
     (bind ?*rulesThatFired* (str-cat ?*rulesThatFired*
        "!Rule: if Temp too cold and Pressure Valid then change Heat CB fires%")
      )
    ;(printout t "too-hot" crlf)
    )

;Change 2
(defrule awake
    (currentang ?angle)
    (answer (ident sleepiness)(answer 1))
    =>
    (if (> ?angle 120)then 
    (assert (change_angle(new FuzzyValue ?*angleFvar* "CN"))) 
    ))

(defrule drowsy
    (currentang ?angle)
    (answer (ident sleepiness)(answer 2))
    =>
    (if (<= ?angle 180)then 
    (assert (change_angle(new FuzzyValue ?*angleFvar* "CZ")))
    ))

(defrule sleepy
    (currentang ?angle)
    (answer (ident sleepiness)(answer 3))
    =>
    (if (<= ?angle 180)then 
    (assert (change_angle(new FuzzyValue ?*angleFvar* "CP")))
    ))

;defuzzify the facts
(defrule defuzzify "low salience to allow all rules to fire and do global contribution"
   (declare (salience -100))
  ?hf <- (change_heat ?h)
  ?cf <- (change_cool ?c)
  ?temp <- (temp ?)
  ?pressure <- (currentp ?)
  =>
  (clear-storage)
  (bind ?hot-change (?h centerOfAreaDefuzzify))
  (bind ?cold-change (?c centerOfAreaDefuzzify))
  (printout t "--------------------------------------------" crlf)
  (printout t "--------------------------------------------" crlf crlf)
  (printout t 
	" Mattress calibrated based on your readings.
     Following are the calibration of readings based on the outside temperature
     and your current posture that is based on pressure calculated on mattress " crlf)  
  (printout t crlf "Hot Change " ?hot-change crlf "Cold change "?cold-change crlf)
  (printout t crlf"--------------------------------------------" crlf)
  (printout t     "--------------------------------------------" crlf)  
  	 
  /*(store COLDCHANGE ?cold-change)
  (store HOTVCHANGE ?hot-change)*/
  (store RULESTHATFIRED ?*rulesThatFired*)
  (bind ?*rulesThatFired* "")
  (retract ?hf ?cf ?temp ?pressure)
  ;(assert (temperature calibrated))  
)

(defrule defuzzify_sleep
   (declare (salience -200))
   ;(temperature calibrated) 
   ?angle <- (change_angle ?an)
   =>
   (bind ?angle-change (?an momentDefuzzify))
   (printout t crlf "Angle Change of Mattress" ?angle-change crlf)
   (retract ?angle)      
    )

(deffacts test-facts
    (question (ident weight)(type weight)
        (text "  "))
    (question (ident height)(type height)
        (text "What is your height "))
    /*(question (ident pressure)(type pressure)
        (text " Current pressure readings "))*/
    (question (ident temperature)(type temperature)
        (text " Current temperature readings "))
    (question (ident sleepiness)(type range)
        (text "How sleepy are you feeling"))

     )
(reset)
(run)
;(facts)
