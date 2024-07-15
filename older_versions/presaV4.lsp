;; UNITS command set precision for decimal to 0
;; DIMSTYLE take the 'medot' one and just uncheck the Trailing checkbox
;; Global variables to store:
(setq shapes '())

(setq circle_list '())

(setq dim_list '())

(setq offsetx 0.0)
(setq offsety 0.0)

(setq title_offsetx 70)
(setq title_offsety 35)
(setq bar_offset 58)
(setq name_offsetx 120)
(setq name_offsety 70)

(setq top_offset 0.0)
(setq bottom_offset 0.0)
(setq right_offset 0.0)
(setq left_offset 0.0)

(setq w 0.0)
(setq h 0.0)
(setq d 0.0)
(setq cx 0.0)
(setq cy 0.0)
(setq curr_type 1)

(defun c:AddShape (obj)
  (setq shapes (cons obj shapes))
  (princ (strcat "\nShape added to the list. Total shapes: " (itoa (length shapes))))
)

(defun c:AddCircle (obj)
  (setq circle_list (cons obj circle_list))
  (princ (strcat "\nCircle added to the list. Total circles: " (itoa (length circle_list))))
)

(defun c:AddDimlinear (obj)
  (setq dim_list (cons obj dim_list))
  (princ (strcat "\nDimlinear added to the list. Total dimlinears: " (itoa (length dim_list))))
)

(defun c:setOffsets ()
  (setq offsetx (getreal "\nEnter offset x: ")) 
  (setq offsety (getreal "\nEnter offset y: ")) 
)

; makes texts numbers appear without zeros from the right of decimal point
; in order to set dimensions style text and arrows -> use DIMSTYLE command and do them manually before you start drawing anything!
(defun c:setStyle ()
  (command "-units" "2" "0" "1" "0" "0" "N")
)

(defun get_layer (pit_type)
  (cond ((eq pit_type 1) "M5220-GENERAL")
        ((eq pit_type 2) "M4804-BIUV")
		((eq pit_type 3) "M4902-NIKUZ")
		((eq pit_type 4) "M4001-GENERAL")
		((eq pit_type 5) "M4903-KOLTAN")
		((eq pit_type 6) "M6702-IEC")
		((eq pit_type 7) "M4615-MYM")
		((eq pit_type 8) "M4001-HOT")
		((eq pit_type 9) "M4001-PARTNER")
		((eq pit_type 10) "M4001-CELLCOM")
		((eq pit_type 11) "M4001-BZQ")
		((eq pit_type 12) "M4453-RAMZOR")
		((eq pit_type 13) "M4429-LT TEORA")
		((eq pit_type 14) "M5305-DELEK")
		((eq pit_type 15) "M5145-GAS")
		((eq pit_type 16) "M4609-MAGOF")
        (t "M5220-GENERAL")))
		
(defun get_layer_name (pit_type)
  (cond ((eq pit_type 1) "")
        ((eq pit_type 2) " - \U+05E9. \U+05D1\U+05D9\U+05D5\U+05D1")
		((eq pit_type 3) " - \U+05E9. \U+05E0\U+05D9\U+05E7\U+05D5\U+05D6")
		((eq pit_type 4) " - \U+05E9. \U+05EA\U+05E7\U+05E9\U+05D5\U+05E8\U+05EA")
		((eq pit_type 5) " - \U+05E7\U+05D5\U+05DC\U+05D8\U+05DF")
		((eq pit_type 6) " - \U+05E9. \U+05D7\U+05E9\U+05DE\U+05DC")
		((eq pit_type 7) " - \U+05E9. \U+05DE\U+05D9\U+05DD")
		((eq pit_type 8) " - \U+05E9. \U+05D4\U+05D5\U+05D8")
		((eq pit_type 9) " - \U+05E9. \U+05E4\U+05E8\U+05D8\U+05E0\U+05E8")
		((eq pit_type 10) " - \U+05E9. \U+05E1\U+05DC\U+05E7\U+05D5\U+05DD")
		((eq pit_type 11) " - \U+05E9. \U+05D1\U+05D6\U+05E7")
		((eq pit_type 12) " - \U+05E9. \U+05E8\U+05DE\U+05D6\U+05D5\U+05E8")
		((eq pit_type 13) " - \U+05E9. \U+05EA\U+05D0\U+05D5\U+05E8\U+05D4")
		((eq pit_type 14) " - \U+05E9. \U+05D3\U+05DC\U+05E7")
		((eq pit_type 15) " - \U+05E9. \U+05D2\U+05D6")
		((eq pit_type 16) " - \U+05E9. \U+05DE\U+05D2\U+05D5\U+05E3")
        (t " - \U+05E9. \U+05DB\U+05DC\U+05DC\U+05D9\U+05EA")))

(defun c:zz ()
  (setq pit_number (getint "\nEnter pit number: "))
  (setq type (getint "\nEnter pit type: "))
  (setq edge_height (getint "\nEnter depth: ")) 
  (setq main_width (getint "\nEnter width: ")) 
  (setq main_height (getint "\nEnter height: ")) 
  (setq diamet (getint "\nEnter cover: "))
  (setq zero 0.0)
  
  ; Save them for later use:
  (setq w main_width)
  (setq h main_height)
  (setq d edge_height)
  (setq curr_type type)
  
  ; Reset lists:
  (setq shapes '()) 
  (setq circle_list '()) 
  (setq dim_list '()) 
  
  ; these offsets are for the pipes
  (setq top_offset 0.0)
  (setq bottom_offset 0.0)
  (setq right_offset 0.0)
  (setq left_offset 0.0)
  
  ; Toggles off osnap mode - for best drawing
  (setvar 'osmode 0)

  (setq min_val (min main_width main_height))

  (setq diamet (min diamet min_val))
 
  ; Calculate the corner points of the main rectangle
  (setq pt1 (list (+ zero offsetx) (+ zero offsety)))
  (setq pt2 (list (+ main_width offsetx) (+ main_height offsety)))

  ; Top edge
  (setq pt11 (list (+ zero offsetx) (+ main_height offsety)))
  (setq pt21 (list (+ main_width offsetx) (+ main_height edge_height offsety)))

  ; Right edge
  (setq pt12 (list (+ main_width offsetx) (+ main_height offsety)))
  (setq pt22 (list (+ main_width edge_height offsetx) (+ zero offsety)))

  ; Left edge
  (setq pt13 (list (+ (- zero edge_height) offsetx) (+ main_height offsety)))
  (setq pt23 (list (+ zero offsetx) (+ zero offsety)))

  ; Bottom edge
  (setq pt14 (list (+ zero offsetx) (+ zero offsety)))
  (setq pt24 (list (+ main_width offsetx) (+ (- zero edge_height) offsety)))
 
  ; Draw the rectangles
  (command "_RECTANG" pt1 pt2)
  (setq obj (entlast)) 
  (c:AddShape obj)
  (command "_RECTANG" pt11 pt21)
  (setq obj (entlast)) 
  (c:AddShape obj)
  (command "_RECTANG" pt12 pt22)
  (setq obj (entlast)) 
  (c:AddShape obj)
  (command "_RECTANG" pt13 pt23)
  (setq obj (entlast)) 
  (c:AddShape obj)
  (command "_RECTANG" pt14 pt24)
  (setq obj (entlast)) 
  (c:AddShape obj)

  ; Get center of main rectangle
  (setq center_x (/ (+ (car pt1) (car pt2)) 2.0)) 
  (setq center_y (/ (+ (cadr pt1) (cadr pt2)) 2.0))
  
  (setq cx center_x)
  (setq cy center_y)
  
  ; Calculate radius
  (setq radius (/ diamet 2.0))
  (setq center_pt (list center_x center_y))
  
  ; Draw the circle
  (command "_CIRCLE" center_pt radius)
  (setq obj (entlast))
  (c:AddCircle obj)

  (setq dist 10.0) 

  (setq d11 (list (+ zero offsetx) (+ main_height edge_height offsety)))
  (setq d12 (list (+ main_width offsetx) (+ main_height edge_height offsety)))
  (setq d13 (list (+ main_width offsetx) (+ main_height edge_height dist offsety)))
  
  (setq d21 (list (+ main_width offsetx) (+ main_height offsety)))
  (setq d22 (list (+ main_width offsetx) (+ main_height edge_height offsety)))
  (setq d23 (list (+ main_width dist offsetx) (+ main_height edge_height offsety)))

  (setq d31 (list (+ main_width edge_height offsetx) (+ main_height offsety)))	
  (setq d32 (list (+ main_width edge_height offsetx) (+ zero offsety))) 			
  (setq d33 (list (+ main_width edge_height dist offsetx) (+ zero offsety)))		

  ; In order to make dimlinear text height 5 (couldn't do it as command)
  ; do the following steps:
  ; command DIMSTYLE -> Modify.. -> Text -> Text height = 5
  (command "_dimlinear" d11 d12 d13)
  (setq obj (entlast)) 
  (c:AddDimlinear obj)
  (command "_dimlinear" d21 d22 d23)
  (setq obj (entlast)) 
  (c:AddDimlinear obj)
  (command "_dimlinear" d31 d32 d33)
  (setq obj (entlast)) 
  (c:AddDimlinear obj)
  ; IMPORTANT NOTE: if any problems happen, just hit F3 and turn OFF Osnap mode (Object Snap mode)
  
  (setq c1 (list (- center_x radius) center_y))
  (setq c2 (list (+ center_x radius) center_y))
  (setq c3 (list (+ center_x radius) center_y))
  
  (command "_dimlinear" c1 c2 c3)
  (setq obj (entlast)) 
  (c:AddDimlinear obj)
  
  (setq title_p1 (list (- center_x title_offsetx) (+ center_y (/ main_height 2) edge_height title_offsety)))
  (setq title_p2 (list (- center_x title_offsetx) (+ center_y (/ main_height 2) edge_height title_offsety 18)))
  (setq title_p3 (list (- center_x title_offsetx) (+ center_y (/ main_height 2) edge_height title_offsety)))
  
  (if (= main_width main_height)
	(command "_text" title_p1 title_p2 title_p3 (strcat "%%c" (rtos (/ main_width 100.0) 2 2) "/%%c" (rtos (/ diamet 100.0) 2 2)))
	(command "_text" title_p1 title_p2 title_p3 (strcat (rtos (/ main_width 100.0) 2 2) "X" (rtos (/ main_height 100.0) 2 2) "/%%c" (rtos (/ diamet 100.0) 2 2)))
  )
  (setq obj (entlast)) 
  (c:AddShape obj)
  
  (setq bar_p1 (list (- center_x title_offsetx 35) (+ center_y (/ main_height 2) edge_height bar_offset)))
  (setq bar_p2 (list (+ (- center_x title_offsetx) 180) (+ center_y (/ main_height 2) edge_height bar_offset 9)))
  (command "_RECTANG" bar_p1 bar_p2)
  (setq obj (entlast)) 
  (c:AddShape obj)
  
  (setq name_p1 (list (- center_x name_offsetx) (+ center_y (/ main_height 2) edge_height name_offsety)))
  (setq name_p2 (list (- center_x name_offsetx) (+ center_y (/ main_height 2) edge_height name_offsety 18)))
  (setq name_p3 (list (- center_x name_offsetx) (+ center_y (/ main_height 2) edge_height name_offsety)))
  (command "_text" name_p1 name_p2 name_p3 (strcat "\U+05D0\U+05DC\U+05DE\U+05E0\U+05D8 \U+05DE\U+05E1\U+05E4\U+05E8 " (itoa pit_number) (get_layer_name type)))
  (setq obj (entlast)) 
  (c:AddShape obj)
  (c:nn type)
  (c:bb)
  (c:vv)
  
  (setvar 'osmode 3583)
 
  (princ "\nShapes drawn successfully.")
 
  (princ)
)

(defun c:xx ()
  (setq amount (getint "\nEnter amount: ")) 
  (setq size_t (getreal "\nEnter size in inches: ")) 
  (setq depth_t (getint "\nEnter IL: "))
  (setq azimut (getint "\nEnter azimut: ")) 

  (setq zero_t 0.0)
  (setq inch 2.54)
  (setq spointx 0.4)
  (setq spointy 0.4)
  (if (>= size_t 10.0)
	(progn
	  (setq spointx 0.5)
	  (setq spointy 0.3)
	)
  )
  (setq sinch (* size_t inch))
  
  (if (and (>= azimut 0) (<= azimut 360))
      (princ "\nCorrect azimut.")
      (prompt "\nError: Azimut must be between 0 and 360 degrees.")
  )
  
  (setq startx cx)
  (setq starty cy)
  (setq p1 (list zero_t zero_t))
  (setq p2 (list zero_t zero_t))
  (setq p3 (list zero_t zero_t))
  
  (setq q1 (list zero_t zero_t))
  (setq q2 (list zero_t zero_t))
  (setq q3 (list zero_t zero_t))
  
  (setq r1 (list zero_t zero_t))
  (setq r2 (list zero_t zero_t))
  
  (setq depth_t (min d depth_t))
  
  (setvar 'osmode 0)
  
  (if (or (and (>= azimut 0) (< azimut 45)) (and (>= azimut 315) (< azimut 360)))
	(progn
	 (setq starty (+ starty (/ h 2.0) d))
	 (setq p1 (list (+ startx top_offset) starty))
	 (setq p2 (list (+ startx top_offset) (- starty depth_t)))
	 (setq p3 (list (+ startx top_offset 1.0) (- starty depth_t)))
	 
	 (setq i 0)
	 (while (< i amount)
		(progn
			(setq r1 (list (- (+ startx top_offset) (* i sinch)) (- starty depth_t)))
			(setq r2 (list (- (+ startx top_offset) (* i sinch) sinch) (+ (- starty depth_t) sinch)))
			(command "_RECTANG" r1 r2)
			(setq obj (entlast)) 
			(c:AddShape obj)
			(setq ccx (/ (+ (car r1) (car r2)) 2.0)) 
			(setq ccy (/ (+ (cadr r1) (cadr r2)) 2.0))
			(setq center (list ccx ccy))
			(command "_CIRCLE" center (/ sinch 2.0))
			(setq obj (entlast)) 
			(c:AddShape obj)
			(command "_text" (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (list (- ccx size_t) (+ (- ccy size_t) size_t)) (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (strcat (rtos size_t) "\""))
			(setq obj (entlast)) 
			(c:AddShape obj)
			(setq i (+ i 1))
		)
	 )
	 
	 (setq q1 (list (+ startx top_offset) (- starty depth_t)))
	 (setq q2 (list (+ startx top_offset) (- starty d)))
	 (setq q3 (list (+ startx top_offset 1.0) (- starty d)))
	 
	 (setq top_offset (+ top_offset 10.0))
	)
  )
  (if (and (>= azimut 45) (< azimut 135))
	(progn
	 (setq startx (+ startx (/ w 2.0) d))
	 (setq p1 (list startx (- starty right_offset)))
	 (setq p2 (list (- startx depth_t) (- starty right_offset)))
	 (setq p3 (list (- startx depth_t) (- starty right_offset 1.0)))
	 
	 (setq i 0)
	 (while (< i amount)
		(progn
			(setq r1 (list (- startx depth_t) (+ (- starty right_offset) (* i sinch))))
			(setq r2 (list (+ (- startx depth_t) sinch) (+ (- starty right_offset) (* i sinch) sinch)))
			(command "_RECTANG" r1 r2)
			(setq obj (entlast)) 
			(c:AddShape obj)
			(setq ccx (/ (+ (car r1) (car r2)) 2.0)) 
			(setq ccy (/ (+ (cadr r1) (cadr r2)) 2.0))
			(setq center (list ccx ccy))
			(command "_CIRCLE" center (/ sinch 2.0))
			(setq obj (entlast)) 
			(c:AddShape obj)
			(command "_text" (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (list (- ccx size_t) (+ (- ccy size_t) size_t)) (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (strcat (rtos size_t) "\""))
			(setq obj (entlast)) 
			(c:AddShape obj)
			(setq i (+ i 1))
		)
	 )
	 
	 (setq q1 (list (- startx depth_t) (- starty right_offset)))
	 (setq q2 (list (- startx d) (- starty right_offset)))
	 (setq q3 (list (- startx d) (- starty right_offset 1.0)))
	 
	 (setq right_offset (+ right_offset 10.0))
	)
  )
  (if (and (>= azimut 135) (< azimut 225))
	(progn
	 (setq starty (- starty (/ h 2.0) d))
	 (setq p1 (list (+ startx bottom_offset) starty))
	 (setq p2 (list (+ startx bottom_offset) (+ starty depth_t)))
	 (setq p3 (list (+ startx bottom_offset 1.0) (+ starty depth_t)))
	 
	 (setq i 0)
	 (while (< i amount)
		(progn
			(setq r1 (list (- (+ startx bottom_offset) (* i sinch)) (+ starty depth_t)))
			(setq r2 (list (- (+ startx bottom_offset) (* i sinch) sinch) (- (+ starty depth_t) sinch)))
			(command "_RECTANG" r1 r2)
			(setq obj (entlast)) 
			(c:AddShape obj)
			(setq ccx (/ (+ (car r1) (car r2)) 2.0)) 
			(setq ccy (/ (+ (cadr r1) (cadr r2)) 2.0))
			(setq center (list ccx ccy))
			(command "_CIRCLE" center (/ sinch 2.0))
			(setq obj (entlast)) 
			(c:AddShape obj)
			(command "_text" (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (list (- ccx size_t) (+ (- ccy size_t) size_t)) (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (strcat (rtos size_t) "\""))
			(setq obj (entlast)) 
			(c:AddShape obj)
			(setq i (+ i 1))
		)
	 )
	 
	 (setq q1 (list (+ startx bottom_offset) (+ starty depth_t)))
	 (setq q2 (list (+ startx bottom_offset) (+ starty d)))
	 (setq q3 (list (+ startx bottom_offset 1.0) (+ starty d)))
	 
	 (setq bottom_offset (+ bottom_offset 10.0))
	)
  )
  (if (and (>= azimut 225) (< azimut 315))
	(progn
	 (setq startx (- startx (/ w 2.0) d))
	 (setq p1 (list startx (+ starty left_offset)))
	 (setq p2 (list (+ startx depth_t) (+ starty left_offset)))
	 (setq p3 (list (+ startx depth_t) (+ starty left_offset 1.0)))
	 
	 (setq i 0)
	 (while (< i amount)
		(progn
			(setq r1 (list (+ startx depth_t) (- (+ starty left_offset) (* i sinch))))
			(setq r2 (list (- (+ startx depth_t) sinch) (- (+ starty left_offset) (* i sinch) sinch)))
			(command "_RECTANG" r1 r2)
			(setq obj (entlast)) 
			(c:AddShape obj)
			(setq ccx (/ (+ (car r1) (car r2)) 2.0)) 
			(setq ccy (/ (+ (cadr r1) (cadr r2)) 2.0))
			(setq center (list ccx ccy))
			(command "_CIRCLE" center (/ sinch 2.0))
			(setq obj (entlast)) 
			(c:AddShape obj)
			(command "_text" (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (list (- ccx size_t) (+ (- ccy size_t) size_t)) (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (strcat (rtos size_t) "\""))
			(setq obj (entlast)) 
			(c:AddShape obj)
			(setq i (+ i 1))
		)
	 )
	 
	 (setq q1 (list (+ startx depth_t) (+ starty left_offset)))
	 (setq q2 (list (+ startx d) (+ starty left_offset)))
	 (setq q3 (list (+ startx d) (+ starty left_offset 1.0)))
	 
	 (setq left_offset (+ left_offset 10.0))
	)
  )
  
  (command "_dimlinear" p1 p2 p3)
  (setq obj (entlast)) 
  (c:AddDimlinear obj)
  
  ;(command "_RECTANG" r1 r2)
  
  (if (not (= depth_t d))
	(progn
	  (command "_dimlinear" q1 q2 q3)
	  (setq obj (entlast)) 
	  (c:AddDimlinear obj)
	)
  )
  
  (c:nn curr_type)
  (c:bb)
  
  (setvar 'osmode 3583)
 
  (princ "\nDrawn successfully.")
 
  (princ)
)

(defun c:bb ()
  (setq new_layer "dim")  ; Put dim layer name
  
  (foreach dim dim_list
    (if (setq ent (entget dim)) 
      (progn
        (entmod (subst (cons 8 new_layer) (assoc 8 ent) ent))
      )
      (princ (strcat "\nEntity " dim " does not exist."))
    )
  )
  
  (princ "\nLayer changed for all shapes.")
  (princ)
)

(defun c:vv ()
  (setq new_layer "MAHSI") ; Put circle layer name
  
  (foreach circle circle_list
    (if (setq ent (entget circle)) 
      (progn
        (entmod (subst (cons 8 new_layer) (assoc 8 ent) ent))
      )
      (princ (strcat "\nEntity " circle " does not exist."))
    )
  )
  
  (princ "\nLayer changed for all shapes.")
  (princ)
)

; If a layer doesn't set as wanted - just use the Match Properties (MA) button and do it manually
(defun c:nn (pit_type)
  ;(setq pit_type (getint "\nEnter pit type: "))
  (setq new_layer (get_layer pit_type)) ; Put shape layer name
  
  (foreach shape shapes
    (if (setq ent (entget shape)) 
      (progn
        (entmod (subst (cons 8 new_layer) (assoc 8 ent) ent))
      )
      (princ (strcat "\nEntity " shape " does not exist."))
    )
  )
  
  (princ "\nLayer changed for all shapes.")
  (princ)
)
