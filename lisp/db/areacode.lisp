
;;; cm_area_codes
;
;area codes:
;    area-code   t
;    state
;    cities
;    time-zone   pacific mountain central eastern
;
;    (ac    st  tz          cities)
;
       

(defun concatenate-symbol (&rest args)
  (cond ((null args) nil)
        (else
         (->symbol (format nil "~A~A"
                           (car args)
                           (cond ((apply 'concatenate-symbol (cdr args)))
                                 (else "")))))))


(DEFVAR ac-table (my-make-table ac-table)) 

(defclass area-code ()
  ((ac
    :accessor area-code-ac
    :initarg :ac
    :type integer
    :initform 0)
   (state
    :accessor area-code-state
    :initarg :state
    :type symbol
    :initform nil)
    (tz
    :accessor area-code-tz
    :initarg :tz
    :type symbol
    :initform nil)
   (cities
    :accessor area-code-cities
    :initarg :cities
    :type list
    :initform nil)
 ))
 
(defun make-area-code (&rest initargs)
   (apply #'make-instance 'area-code initargs))

(define-predicate area-code)

(defmethod pretty-print ((self area-code) stream)
  (format stream   "  Area code: ~A" (area-code-ac self))
  (format stream "~%      State: ~A" (area-code-state self))
  (format stream "~%  Time Zone: ~A" (area-code-tz self))
  (format stream "~%     Cities: ~A" (area-code-cities self)))

(defmethod print-object ((self area-code) stream)
  (format stream "#{Area Code (~A) ~A ~A ~A ~A}"
          (object-hash self)
          (area-code-ac self)
          (area-code-state self)
          (area-code-tz self)
          (area-code-cities self)))

;;  Initialize all the buggers.

(MAPC 
 #'(LAMBDA (ac-list)
     (let ((new (make-area-code))
           (ac  (concatenate-symbol 'ac (car ac-list)))
           (num   (car ac-list))
           (state (cadr ac-list))
           (tz   (caddr ac-list))
           (cities (cdddr ac-list)))
       (setf (ac-table ac) new)
       (setf (ac-table state) (push new (ac-table state)))
       (SETF (area-code-ac new) num) 
       (SETF (area-code-state new) state) 
       (SETF (area-code-tz new) tz) 
       (SETF (area-code-cities new) cities)))
  '((205 al  central     alabama)
    (907 ak  alaskan     alaska)
    (602 az  mountain    arizona)
    (501 ar  central     arkansas)
    (809 bh  atlantic    bahamas puerto rico islands)
    (209 ca  pacific     fresno)
    (213 ca  pacific     los angeles)
    (310 ca  pacific     long beach)
    (510 ca  pacific     oakland)
    (714 ca  pacific     orange)
    (818 ca  pacific     pasadena)
    (916 ca  pacific     sacramento)
    (619 ca  pacific     san diego)
    (415 ca  pacific     san francisco)
    (408 ca  pacific     san jose)
    (805 ca  pacific     santa barbara)
    (707 ca  pacific     santa rosa)
    (604 cn  pacific     british columbia)
    (403 cn  mountain    alberta)
    (306 cn  mountain    saskatchewan)
    (204 cn  central     manitoba)
    (807 cn  eastern     ontario* thunder bay)
    (519 cn  eastern     ontario* london)
    (613 cn  eastern     ontario* ottawa)
    (705 cn  eastern     ontario* north bay)
    (416 cn  eastern     ontario* toronto)
    (514 cn  eastern     quebec* montreal)
    (418 cn  eastern     quebec* quebec)
    (819 cn  eastern     quebec* sherbrooke)
    (709 cn  atlantic    newfoundland)
    (506 cn  atlantic    new brunswick)
    (902 cn  atlantic    prince edward island - nova scotia)
    (303 co  mountain    aspen boulder breckenridge denver ft collins vail)
    (719 co  mountain    colorado springs pueblo)
    (203 ct  eastern     connecticut)
    (302 de  eastern     deleware)
    (202 dc  eastern     washington)
    (813 fl  eastern     clearwater tampa)
    (407 fl  eastern     belle glade boca raton ft pierce kissimmee orlando vero beach)
    (904 fl  eastern     daytona beach jacksonville)
    (305 fl  eastern     ft lauderdale miami)
    (404 ga  eastern     atlanta)
    (706 ga  eastern     augusta)
    (912 ga  eastern     macon savannah columbus brunswick)
    (808 hi  hawaiian    hawaii)
    (208 id  mountain    idaho)
    (618 il  central     alton belleville)
    (309 il  central     bloomington peoria)
    (217 il  central     champaign springfield)
    (312 il  central     chicago)
    (708 il  central     evanston suburban-chicago waukegan)
    (815 il  central     joliet rockford)
    (812 in  eastern     bloomington evansville)
    (219 in  eastern     elkhart south bend)
    (317 in  eastern     indianapolis kokomo)
    (319 ia  central     cedar rapids davenstream dubuque)
    (712 ia  central     council bluffs spencer)
    (515 ia  central     des moines)
    (316 ks  central     hutchinson wichita)
    (913 ks  central     lawrence topeka)
    (606 ky  eastern     ashland lexington)
    (502 ky  central     louisville owensboro)
    (504 la  central     baton rouge new orleans)
    (318 la  central     monroe shrevestream)
    (207 me  eastern     maine)
    (301 md  eastern     cumberland frederick hagerstown laurel rockville)
    (410 md  eastern     annapolis baltimore salisbury towson)
;; down to here
    (617 ma  eastern     boston)
    (508 ma  eastern     greater-boston framingham new bedford worcester)
    (413 ma  eastern     northampton pittsfield springfield)
    (313 mi  eastern     ann arbor detroit pontiac)
    (616 mi  eastern     grand rapids kalamazoo muskegon)
    (517 mi  eastern     jackson lansing saginaw)
    (906 mi  eastern     marquette sault ste marie)
    (218 mn  central     duluth)
    (612 mn  central     st paul minneapolis)
    (507 mn  central     rochester)
    (601 ms  central     mississippi)
    (314 mo  central     columbia st louis)
    (417 mo  central     joplin springfield)
    (816 mo  central     kansas city st joseph)
    (406 mt  mountain    montana)
    (402 ne  central     lincoln omaha)
    (308 ne  mountain    scottsbluff)
    (702 nv  pacific     nevada)
    (603 nh  eastern     new hampshire)
    (609 nj  eastern     atlantic city camden trenton vineland woodbury)
    (201 nj  eastern     hackensack morristown newark new brunswick paterson)
    (505 nm  mountain    new mexico)
    (518 ny  eastern     albany hudson schenectady troy)
    (607 ny  eastern     binghamton cortland elmira ithaca)
    (716 ny  eastern     buffalo lockstream niagara falls rochester)
    (516 ny  eastern     hempstead long island)
    (914 ny  eastern     monroe mount vernon peekskill poughkeepsie white plains)
    (212 ny  eastern     new york city)
    (315 ny  eastern     syracuse utica)
    (704 nc  eastern     asheville charlotte)
    (919 nc  eastern     greensboro raleigh winston-salem)
    (701 nd  central     north dakota)
    (216 oh  eastern     akron cleveland youngstown)
    (513 oh  eastern     cincinnati dayton springfield wpafb)
    (614 oh  eastern     columbus)
    (419 oh  eastern     mansfield toledo)
    (405 ok  central     lawton oklahoma city)
    (918 ok  central     muskogee tulsa)
    (503 or  pacific     oregon)
    (215 pa  eastern     allentown easton norristown philadelphia pottstown
                           reading)
    (814 pa  eastern     altoona bradford erie warren)
    (717 pa  eastern     harrisburg lancaster lebanon scranton stroudsburg
                           wilkes-barre)
    (412 pa  eastern     indiana pittsburgh rochester sharon washington)
    (809 pr  eastern     puerto rico)
    (401 ri  eastern     rhode island)
    (803 sc  eastern     south carolina)
    (605 sd  central     south dakota)
    (615 tn  central     chattanooga nashville)
    (901 tn  central     memphis)
    (806 tx  central     amarillo)
    (512 tx  central     austin san antonio)
    (214 tx  central     dallas)
    (915 tx  central     el paso sweetwater)
    (817 tx  central     fort worth waco)
    (713 tx  central     galveston houston)
    (801 ut  mountain    utah)
    (802 vt  eastern     vermont)
    (809 vi  eastern     virgin islands)
    (703 va  eastern     alexandria arlington roanoke)
    (804 va  eastern     charlottesville newstream news norfolk richmond)
    (206 wa  pacific     seattle tacoma)
    (509 wa  pacific     spokane)
    (304 wv  eastern     west virginia)
    (608 wi  central     beloit madison)
    (715 wi  central     eau claire wausau)
    (414 wi  central     milwaukee racine)
    (307 wy  mountain    wyoming)
    (800 mb  nil         ma bell* WATS)))
