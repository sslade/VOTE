# VOTE

The VOTE program is based on the work presented in the book Goal Based
Decision Making by Stephen Slade
(https://www.amazon.com/Goal-based-Decision-Making-Interpersonal-Model/dp/0805813667)

The code is all written in Common Lisp.

For an explanation of the system and its goals please see the book
listed above.

# Trying out the program

In the root of the project you can start a Lisp interpreter, load the
code, and try it out interactively given the commands below.

```
> lisp

;; load the sbcl initialization file, or copy it to ~/.sbclrc
(load "sbclrc.lisp")
;; load the vote program
(load "load_vote.lisp")
;; initialize the vote databases
(init-pol)
;; Run the VOTE object-oriented database for members of congress
(dbase member)
;; Sample dbase commands:
help / header / hc / display
;; connect to issue database and run sample commands
cd issue
;; connect to group database and run sample commands
cd group
;; connect to bill database and run sample commands
cd bill
;; connect to strategy database and run sample commands
cd strategy
;; run the vote program on a decision
(vote 'udall 'hr-2978)
;; view the decision database
(dbase decision)
;; turn on sarcasm
(sarcastic-explanation (car (db-all decision)))
```
