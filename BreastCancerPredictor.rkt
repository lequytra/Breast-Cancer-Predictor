#lang racket

(require csc151)


;ASSIGNMENT 7
;a
#|
Write a procedure, (predict inputs weights) that takes equal-length vectors of
inputs and weights as input and produces the output that the weights predict.
|#

(define predict
  (lambda (inputs weights)
    (let ([len (vector-length weights)])
      (let kernel ([product null]
                   [progress 0])
        (if (<= len progress)
            (apply + product)
            (kernel (cons (* (vector-ref inputs progress)
                             (vector-ref weights progress))
                          product)
                    (+ progress 1)))))))



;;; Procedure:
;;;   improve
;;; Parameters:
;;;   inputs, a vector
;;;   weights, a vector
;;;   actual, a real number
;;;   adjustment, a real number
;;; Purpose:
;;;   To produce a new set of weights after using adjustment factor adjustment
;;; Produces:
;;;   improved-weights, a vector
;;; Preconditions:
;;;   [no additional]
;;; Postconditions:
;;;   (vector-length inputs) = (vector-length improved-weights)

(define improve
  (lambda (inputs weights actual adjustment)
    (let ([len (vector-length inputs)]
          [sum-of-inputs (apply + (vector->list inputs))]
          [difference (- actual (predict inputs weights))])
      (let kernel ([progress 0])
        (cond [(> len progress)
               (vector-set! weights
                            progress
                            (+ (vector-ref weights progress)
                               (* difference
                                  (/ (vector-ref inputs progress)
                                     (if (zero? sum-of-inputs)
                                         1
                                         sum-of-inputs))
                                  adjustment)))
               (kernel (+ 1 progress))]
              [else weights])))))

#|
(> (improve (vector 0 0 0 1) (vector 0 0.5 0 1) 1 (sqrt 2))
'#(0 0.5 0 1)
> (improve (vector 0 0 1) (vector 0 0.5 1) 0 0.1)
'#(0 0.5 0.9)
|#


;;; Procedure:
;;;   refine
;;; Parameters:
;;;   sample, a vector of vectors
;;;   weights, a vector
;;;   adjustments, a real number
;;;   steps, a real number
;;; Purpose:
;;;   To improve repeatedly steps times the set of weights by randomly
;;;   selecting one of the samples and then calling improve procedure
;;; Produces:v
;;;   improved-weights, a vector 
;;; Preconditions:
;;;   [no additional]
;;; Postconditions:
;;;   [no additional]

(define refine
  (lambda (samples weights adjustment steps)
    (let* ([vector (vector-ref samples
                               (random (vector-length samples)))]
           ;select a random vector from samples.
           [inputs (vector-take vector (- (vector-length vector) 1))]
           ;extract the inputs value in vector
           [actual (vector-ref vector (- (vector-length vector) 1))])
           ;extract the output value in vector.
      (cond [(positive? steps)
             (refine samples
                     (improve inputs weights actual adjustment)
                     adjustment
                     (- steps 1))]
            [else weights]))))

;examples

(define majority-samples
  (vector
   (vector 0 0 0 0)
   (vector 0 0 1 0)
   (vector 0 1 0 0)
   (vector 0 1 1 1)
   (vector 1 0 0 0)
   (vector 1 0 1 1)
   (vector 1 1 0 1)
   (vector 1 1 1 1)))
#|
> (refine majority-samples (vector 0 0.5 1) 0.1 10)
'#(0.017725980000000006 0.45074 0.6431092038)

>  (refine majority-samples (vector 0 0.5 1) 0.2 1)
'#(0.05 0.55 1)
|#

;;; Procedure:
;;;   analyze-solution
;;; Parameters:
;;;   samples, a vector of vectors of real numbers
;;;   weights, a vector of real numbers
;;; Purpose:
;;;   Conduct and print out an analysis of the given set of weights on the
;;;   given set of samples.
;;; Produces:
;;;   total-error, a real number
;;; Preconditions:
;;;   * Each element of samples is of the form
;;;     `#(input1 ... inputn desired-output)`
;;;   * Each element of samples has length one greater than the
;;;     length of weights.
;;; Postconditions:
;;;   total-error represents the sum of the squares of errors.

(define analyze-solution
  (lambda (samples weights)
    (let kernel ([pos 0]
                 [total-error 0])
      (cond
        [(< pos (vector-length samples))
         (let* ([sample (vector-ref samples pos)]
                [len (vector-length sample)]
                [inputs (vector-take sample (- len 1))]
                [actual (vector-ref sample (- len 1))]
                [prediction (predict inputs weights)]
                [difference (- actual prediction)])
           (println "Inputs: " inputs #\tab
                    "Predicted: " prediction #\tab
                    "Actual: " actual #\tab
                    "Difference: " difference)
           (kernel (+ pos 1)
                   (+ total-error (square difference))))]
        [else
         (println "Sum of squared errors: " total-error)
         total-error]))))

;;; Procedure:
;;;   println
;;; Parameters:
;;;   val1, a Scheme value
;;;   val2, a Scheme value
;;;   ...
;;;   valn, a Scheme value
;;; Purpose:
;;;   Print all of the values followed by a newline
;;; Produces:
;;;   [Nothing; called for the side effect.]
(define println
  (lambda vals
    (map display vals)
    (newline)))


#|
a. Arrange to read all of the data into a variable called raw-cell-data.
|#

(define raw-cell-data
;  (read-csv-file "/Users/giang/Desktop/breast-cancer-wisconsin.csv"))
 (read-csv-file "/Users/tranle/Desktop/breast-cancer-data.csv"))

;(take raw-cell-data 10)

;;; Procedure:
;;;   all
;;; Parameters:
;;;   pred?, a unary predicate
;;;   vec, a vector
;;; Purpose:
;;;   Determine if pred? holds for all the values in vector.
;;; Produces:
;;;   ok?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If there is an i such that (pred? (vector-ref vec i))
;;;     fails to hold, then ok? is false.
;;;   Otherwise, ok? is true.

(define all
  (lambda (pred? vec)
    (let ([len (vector-length vec)])
      (let kernel ([pos 0])
        (or (>= pos len)
            (and (pred? (vector-ref vec pos))
                 (kernel (+ pos 1))))))))

;;; Procedure:
;;;   change
;;; Parameters:
;;;   vector, a vector
;;; Purpose:
;;;   To convert a vector's last element (either 2 or 4) to either 0 or 1
;;; Produces:
;;;   converted-vector
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * (vector-ref converted-vector (- (vector-length converted-vector) 1)) is either 0 or 1
;;;   * if the last element is 2, convert to 0.
;;;   * if the last element is 4, convert to 1.

(define change
  (lambda (vector)
    (cond [(= (vector-ref vector (- (vector-length vector)
                                    1)) 2)
           (vector-set! vector (- (vector-length vector) 1) 0)
           vector]
          [else (vector-set! vector (- (vector-length vector) 1) 1)
                vector])))

        
#|

b. Write an instruction or instructions to filter out any data that do not fit
 the form (e.g., that have fewer than 11 values in a row,that have middle values
 that are not integers in the range 1-10, that have a final value that is not 2 or 4).
Call that result clean-cell-data.

First we write a local binding named enough?. Enough?'s input is a vector and check whether
      * this vector has length >= 11
      * all the elements in vector are integers
      * all the vectors middle values are 1 < = x < = 10
      * all vector's final value is either 2 or 4
If all of these return true, then enough? is true. Else, enough? is false
Then we use this predicate to filter our data

|#

;;; Procedure:
;;;   clean-cell-data
;;; Parameters:
;;;   [none]
;;; Purpose:
;;;   To filter out any data that
;;;    * have fewer than 11 values in a row
;;;    * have middle values that are not integers in the range 1-10,
;;;    * that have a final value that is not 2 or 4)
;;; Produces:
;;;   clean-cell-data

(define clean-cell-data
  (let ([enough? (lambda (vector)
                   (and (>= (vector-length vector) 11)
                        (all integer? vector)
                        (all (section <= 1 <> 10)
                             (vector-drop-right (vector-drop vector 1) 1))
                        (or (all (section = <> 2)
                                 (vector-take-right vector 1))
                            (all (section = <> 4)
                                 (vector-take-right vector 1)))))])
    (vector-map change
                (vector-filter enough?
                               (vector-map list->vector
                                           (list->vector raw-cell-data)))))) 
;;; Procedure:
;;;   training-samples
;;; Parameters:
;;;   [none]
;;; Purpose:
;;;   To select the first 400 rows in clean-cell-data 
;;; Produces:
;;;   training-samples, a vector of vectors

(define training-samples
  (vector-take clean-cell-data 400))

;;; Procedure:
;;;   test-samples
;;; Parameters:
;;;   [none]
;;; Purpose:
;;;   To put the remaining rows of data after the first 400 rows into a vector 
;;; Produces:
;;;   sample-test, a vector of vectors

(define test-samples
  (vector-drop clean-cell-data 400))

;(refine (vector-map (section vector-drop <> 1)
                  ;  training-samples) (make-vector 9 1)  0.1 10)

(define tss
  (lambda (lst)
    (let ([mean (/ (apply + lst) (length lst))])
      (let kernel ([sum-square 0]
                   [remaining lst])
           (if (null? remaining)
               sum-square
               (kernel (+ sum-square (square (- (car remaining)
                                                mean)))
                         (cdr remaining)))))))

    #|(tally-all
 (vector->list
  (vector-map =
              (vector-map truncate
                          (vector-map
                           (section predict <>
                                    (refine (vector-map (section vector-drop <> 1)
                                                        training-samples)
                                            (make-vector 9 1)  0.1 1000))
                           (vector-map (o (section vector-drop <> 1)
                                          (section vector-drop-right <> 1))
                                       test-samples)))
              (vector-map
               (section vector-ref <> 10) test-samples))))

    (analyze-solution (vector-map (section vector-drop <> 1) test-samples)
                  (refine (vector-map (section vector-drop <> 1)
                    training-samples) (make-vector 9 1)  0.1 10000))

|#

(analyze-solution (vector-map (section vector-drop <> 1) test-samples)
                  (refine (vector-map (section vector-drop <> 1)
                    training-samples) (make-vector 9 1)  0.1 10000))


    

