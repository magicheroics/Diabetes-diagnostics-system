; Diabetes Diagnosis Expert System
; Written in CLIPS
; Evaluates patient data against medically relevant risk factors to assist in early diabetes detection.

; ============================
; Patient Template Definition
; ============================

(deftemplate patient
   (slot name)
   (slot Gender (type SYMBOL) (allowed-values male female) (default female))
   (slot Height (default nil))  ; in meters
   (slot Weight (default nil))  ; in kg
   (slot Fasting-blood-sugar (type FLOAT))  ; in mg/dL
   (slot Age (type INTEGER))
   (slot Number-of-pregnancies (type INTEGER))
   (slot Blurred-Vision (type SYMBOL) (allowed-values yes no) (default no))
   (slot Tiredness (type SYMBOL) (allowed-values yes no) (default no))
   (slot Familial-History (type SYMBOL) (allowed-values yes no) (default no))
   (slot Low-Physical-Activity (type SYMBOL) (allowed-values yes no) (default no))
   (slot score (type INTEGER) (default 0))
)

; ============================
; Score Source Template (for tracking contributions)
; ============================

(deftemplate score-source
   (slot reason (type STRING))
   (slot points (type INTEGER))
)

; ============================
; Menu-Driven Input System
; ============================

(deffunction get-symbol-input (?prompt)
   (printout t ?prompt " (yes/no): ")
   (bind ?val (read))
   (if (or (eq ?val yes) (eq ?val no)) then
       (return ?val)
   else
       (printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
       (return (get-symbol-input ?prompt))
   )
)

(deffunction get-gender-input ()
   (printout t "Enter gender (male/female): ")
   (bind ?val (read))
   (if (or (eq ?val male) (eq ?val female)) then
       (return ?val)
   else
       (printout t "Invalid input. Please enter 'male' or 'female'." crlf)
       (return (get-gender-input))
   )
)

(deffunction get-numeric-input (?prompt ?min ?max)
   (printout t ?prompt ": ")
   (bind ?val (read))
   (if (and (numberp ?val) (>= ?val ?min) (<= ?val ?max)) then
       (return ?val)
   else
       (printout t "Invalid input. Please enter a number between " ?min " and " ?max "." crlf)
       (return (get-numeric-input ?prompt ?min ?max))
   )
)

(defrule menu-driven-input
   (declare (salience 100))
   (not (patient (name ?)))
   =>
   (printout t crlf "=== Diabetes Diagnosis Expert System ===" crlf)
   (printout t "Enter patient name: ") (bind ?name (read))
   (bind ?gender (get-gender-input))
   (bind ?age (get-numeric-input "Enter age" 0 120))
   (bind ?preg (get-numeric-input "Enter number of pregnancies" 0 20))
   (bind ?fbs (get-numeric-input "Enter fasting blood sugar (mg/dL)" 0 500))
   (bind ?height (get-numeric-input "Enter height in meters" 0.5 3.0))
   (bind ?weight (get-numeric-input "Enter weight in kg" 1 300))

   (bind ?bv (get-symbol-input "Blurred vision"))
   (bind ?ti (get-symbol-input "Tiredness"))
   (bind ?fh (get-symbol-input "Familial history"))
   (bind ?pa (get-symbol-input "Low physical activity"))

   (assert (patient (name ?name)
                    (Gender ?gender)
                    (Age ?age)
                    (Number-of-pregnancies ?preg)
                    (Fasting-blood-sugar ?fbs)
                    (Height ?height)
                    (Weight ?weight)
                    (Blurred-Vision ?bv)
                    (Tiredness ?ti)
                    (Familial-History ?fh)
                    (Low-Physical-Activity ?pa)))
)

; ============================
; Rule Summary Table (Comment)
; ============================

; Rule Name                  | Trigger Condition                                      | Score Contribution
; --------------------------|--------------------------------------------------------|--------------------
; check-blurred-vision      | Blurred vision = yes                                   | +1
; check-tiredness           | Tiredness = yes                                        | +1
; check-familial-history    | Familial history = yes                                 | +1
; check-low-physical-activity | Low physical activity = yes                          | +1
; check-pregnancies         | Gender = female AND pregnancies >= 3                   | +2
; check-fbs                 | Fasting blood sugar >= 126 mg/dL                        | +3
; check-bmi                 | BMI > 28                                               | +2
; check-age                 | Age > 25                                               | +1
; has-diabetes              | Total score >= 5 (fires once)                           | Diagnosis Trigger

; ============================
; Diagnostic Rules
; ============================

(defrule has-diabetes
   (patient (name ?name) (score ?score&:(>= ?score 5)))
   (not (diagnosed))
   =>
   (printout t crlf "Patient " ?name " has diabetes." crlf
               "Confidence Score: " ?score " (anything equal or above 5 is qualified)" crlf)
   (assert (diagnosed))
)

(defrule check-blurred-vision
   ?p <- (patient (Blurred-Vision yes) (score ?score))
   (not (processed check-blurred-vision))
   =>
   (printout t "Patient has blurred vision." crlf)
   (bind ?new-score (+ ?score 1))
   (modify ?p (score ?new-score))
   (assert (score-source (reason "Blurred vision") (points 1)))
   (assert (processed check-blurred-vision))
)

(defrule check-tiredness
   ?p <- (patient (Tiredness yes) (score ?score))
   (not (processed check-tiredness))
   =>
   (printout t "Patient has tiredness." crlf)
   (bind ?new-score (+ ?score 1))
   (modify ?p (score ?new-score))
   (assert (score-source (reason "Tiredness") (points 1)))
   (assert (processed check-tiredness))
)

(defrule check-familial-history
   ?p <- (patient (Familial-History yes) (score ?score))
   (not (processed check-familial-history))
   =>
   (printout t "Patient has familial history." crlf)
   (bind ?new-score (+ ?score 1))
   (modify ?p (score ?new-score))
   (assert (score-source (reason "Familial history") (points 1)))
   (assert (processed check-familial-history))
)

(defrule check-low-physical-activity
   ?p <- (patient (Low-Physical-Activity yes) (score ?score))
   (not (processed check-low-physical-activity))
   =>
   (printout t "Patient has low physical activity." crlf)
   (bind ?new-score (+ ?score 1))
   (modify ?p (score ?new-score))
   (assert (score-source (reason "Low physical activity") (points 1)))
   (assert (processed check-low-physical-activity))
)

(defrule check-pregnancies
   ?p <- (patient (Gender female)
                  (Number-of-pregnancies ?pregnancies&:(>= ?pregnancies 3))
                  (score ?score))
   (not (processed check-pregnancies))
   =>
   (printout t "Patient has had multiple pregnancies." crlf)
   (bind ?new-score (+ ?score 2))
   (modify ?p (score ?new-score))
   (assert (score-source (reason "Multiple pregnancies") (points 2)))
   (assert (processed check-pregnancies))
)

(defrule check-fbs
   ?p <- (patient (Fasting-blood-sugar ?fbs&:(>= ?fbs 126)) (score ?score))
   (not (processed check-fbs))
   =>
   (printout t "Patient has high fasting blood sugar." crlf)
   (bind ?new-score (+ ?score 3))
   (modify ?p (score ?new-score))
   (assert (score-source (reason "High fasting blood sugar") (points 3)))
   (assert (processed check-fbs))
)

(defrule check-bmi
   ?p <- (patient (Height ?height&:(numberp ?height)) (Weight ?weight&:(numberp ?weight)) (score ?score))
   (test (> (/ ?weight (* ?height ?height)) 28))
   (not (processed check-bmi))
   =>
   (bind ?bmi (/ ?weight (* ?height ?height)))
   (printout t crlf "Patient has high BMI: " ?bmi "." crlf)
   (bind ?new-score (+ ?score 2))
   (modify ?p (score ?new-score))
   (assert (score-source (reason "High BMI") (points 2)))
   (assert (processed check-bmi))
)
