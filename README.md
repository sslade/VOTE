# VOTE

The VOTE program is based on the work presented in the book Goal Based
Decision Making by Stephen Slade
(https://www.amazon.com/Goal-based-Decision-Making-Interpersonal-Model/dp/0805813667)

The code is all written in Common Lisp.

For an explanation of the system and its goals please see the book
listed above.

# Trying out the program

For the moment this has only been tested using sblc (brew instal sbcl)
on a late model (Apple Silicon) Macintosh.

To load libraries correctly make a change that's similar to this one,
to update your search path for sbcl:

```
diff --git a/lisp/load_vote.lisp b/lisp/load_vote.lisp
index 7a0c546..7edbf50 100644
--- a/lisp/load_vote.lisp
+++ b/lisp/load_vote.lisp
@@ -44,7 +44,7 @@
    (case *implementation-type*
      (MCL "Macintosh HD:lisp:")
      (Allegro "/class/sslade/vote/")
-     (SBCL "/c/cs458/lisp/")
+     (SBCL "/Users/gnn/Repos/Yale/VOTE/lisp/")
      (otherwise nil)))
   "Root directory in which VOTE code is installed")
```

In the root of the project you can start a Lisp interpreter, load the
code, and try it out interactively given the commands below.

Note that Lisp functions are enclosed in parentheses () and that bare
commands against the program are not.

When moving through the database it acts like a filesystem heirarchy
in UNIX where one uses the cd (change directory) command to access a
particular set of tables.  The current location in the heirarchy is
shown in the prompt for that level:

```
* cd issues

ISSUES> 
```

```
> lisp

;; load the sbcl initialization file, or copy it to ~/.sbclrc
(load "sbclrc.lisp")  <- Lisp Function
;; load the vote program
(load "load_vote.lisp")
;; initialize the vote databases
(init-pol)
;; Run the VOTE object-oriented database for members of congress
(dbase member)
;; Sample dbase commands:
help / header / hc / display
;; Show headers
MEMBER> headers <- Bare command
;; Select a member
MEMBER> hc
;; Display a Member from the Database
* display
;; connect to issue database and run sample commands
cd issue
;; Show the headers
ISSUE> headers
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
