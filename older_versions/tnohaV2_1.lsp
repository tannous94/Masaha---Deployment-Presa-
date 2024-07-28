;; UNITS command set precision for decimal to 0
;; DIMSTYLE take the 'medot' one and just uncheck the Trailing checkbox
;; Global variables to store:
(setq shapes '())

(setq circle_list '())

(setq dim_list '())

(setq offsetx 0.0)
(setq offsety 0.0)

(setq distance_in_between 120)

(setq title_offsetx 70)
(setq title_offsety 35)
(setq bar_offset 58)
(setq name_offsetx 120)
(setq name_offsety 70)

(setq top_offset 0.0)
(setq bottom_offset 0.0)
(setq right_offset 0.0)
(setq left_offset 0.0)

(setq prev_w 0.0)
(setq prev_h 0.0)
(setq prev_d 0.0)
(setq w 0.0)
(setq h 0.0)
(setq d 0.0)
(setq cx 0.0)
(setq cy 0.0)
(setq curr_type 1)

(setq magof "\U+05D9\U+05E9 \U+05D1\U+05E4\U+05E0\U+05D9\U+05DD \U+05DE\U+05D2\U+05D5\U+05E3")
(setq bezeq_code 11)
(setq koltan_code 5)
(setq magof_code 16)

(setq save_tl 0)
(setq start_point nil)

(setq zero_azimut_map '((0 . 0)
                (10 . 0)
                (20 . 0)
                (30 . 0)
                (40 . 0)
                (50 . 0)
                (60 . 0)
                (70 . 0)
                (80 . 0)
                (90 . 0)
                (100 . 0)
                (110 . 0)
                (120 . 0)
                (130 . 0)
                (140 . 0)
                (150 . 0)
                (160 . 0)
                (170 . 0)
                (180 . 0)
                (190 . 0)
                (200 . 0)
                (210 . 0)
                (220 . 0)
                (230 . 0)
                (240 . 0)
                (250 . 0)
                (260 . 0)
                (270 . 0)
                (280 . 0)
                (290 . 0)
                (300 . 0)
                (310 . 0)
                (320 . 0)
                (330 . 0)
                (340 . 0)
                (350 . 0)
				))
				
(setq azimut_map zero_azimut_map)

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
	(setq point (getpoint "\nClick where do you want the Presa to be drawn."))
	(setq offsetx (car point))
	(setq offsety (cadr point))
	;(setq offsetx (getreal "\nEnter offset x: ")) 
	;(setq offsety (getreal "\nEnter offset y: "))
	(setq prev_w 0.0)
	(setq prev_h 0.0)
	(setq prev_d 0.0)
	(setq w 0.0)
	(setq h 0.0)
	(setq d 0.0)
)

(defun shiftOffsets (new_x new_y)
  (setq offsetx new_x) 
  (setq offsety new_y) 
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
		((eq pit_type 17) "M4615-MYM")
		((eq pit_type 18) "M4001-GENERAL")
		((eq pit_type 19) "M6702-IEC")
        (t "M5220-GENERAL")))
		
(defun get_layer_name (pit_type)
  (cond ((eq pit_type 1) " - \U+05E9. \U+05DB\U+05DC\U+05DC\U+05D9\U+05EA")
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
		((eq pit_type 17) " - \U+05E9. \U+05D4\U+05E9\U+05E7\U+05D9\U+05D4")
		((eq pit_type 18) " - \U+05E9. \U+05E1\U+05D9\U+05D1 \U+05D0\U+05D5\U+05E4\U+05D8\U+05D9")
		((eq pit_type 19) " - \U+05E9. \U+05D4\U+05D0\U+05E8\U+05E7\U+05D4")
        (t " - \U+05E9. \U+05DB\U+05DC\U+05DC\U+05D9\U+05EA")))

(defun c:ff (p_number p_type p_depth p_width p_height p_cover)
  ;save previous dimensions to use them for the shift right
  (setq prev_w w)
  (setq prev_h h)
  (setq prev_d d)
  
  ;(setq pit_number (getint "\nEnter pit number: "))
  ;(setq type (getint "\nEnter pit type: "))
  ;(setq edge_height (getint "\nEnter depth: ")) 
  ;(setq main_width (getint "\nEnter width: ")) 
  ;(setq main_height (getint "\nEnter height: ")) 
  ;(setq diamet (getint "\nEnter cover: "))
  (setq pit_number p_number)
  (setq type p_type)
  (setq edge_height p_depth) 
  (setq main_width p_width) 
  (setq main_height p_height) 
  (setq diamet p_cover)
  (setq zero 0.0)
  
  ; Save them for later use:
  (setq w main_width)
  (setq h main_height)
  (setq d edge_height)
  (setq curr_type type)
  
  ; call shiftOffsets to shift the x coordinates to the right for the next presa
  (if (or (/= prev_w 0) (/= prev_h 0))
	(shiftOffsets (+ offsetx prev_w prev_d distance_in_between edge_height) (- (+ offsety prev_h prev_d) main_height edge_height))
  )
  
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

(defun c:gg (amount_tt size_tt depth_tt azimut_tt)
  ;(setq amount (getint "\nEnter amount: ")) 
  ;(setq size_t (getreal "\nEnter size in inches: ")) 
  ;(setq depth_t (getint "\nEnter IL: "))
  ;(setq azimut (getint "\nEnter azimut: ")) 
  (setq amount amount_tt) 
  (setq size_t size_tt) 
  (setq depth_t depth_tt)
  (setq azimut azimut_tt) 

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ALL FOR TNOHA HERE:

(defun convert_angle (angle_t)
	(cond ((eq angle_t 0) 90)
        ((eq angle_t 10) 80)
        ((eq angle_t 20) 70)
        ((eq angle_t 30) 60)
        ((eq angle_t 40) 50)
        ((eq angle_t 50) 40)
        ((eq angle_t 60) 30)
        ((eq angle_t 70) 20)
        ((eq angle_t 80) 10)
        ((eq angle_t 90) 0)
        ((eq angle_t 100) 350)
        ((eq angle_t 110) 340)
        ((eq angle_t 120) 330)
        ((eq angle_t 130) 320)
        ((eq angle_t 140) 310)
        ((eq angle_t 150) 300)
        ((eq angle_t 160) 290)
        ((eq angle_t 170) 280)
        ((eq angle_t 180) 270)
        ((eq angle_t 190) 260)
        ((eq angle_t 200) 250)
        ((eq angle_t 210) 240)
        ((eq angle_t 220) 230)
        ((eq angle_t 230) 220)
        ((eq angle_t 240) 210)
        ((eq angle_t 250) 200)
        ((eq angle_t 260) 190)
        ((eq angle_t 270) 180)
        ((eq angle_t 280) 170)
        ((eq angle_t 290) 160)
        ((eq angle_t 300) 150)
        ((eq angle_t 310) 140)
        ((eq angle_t 320) 130)
        ((eq angle_t 330) 120)
        ((eq angle_t 340) 110)
        ((eq angle_t 350) 100)
        (t 0))
)

(defun get_pit_text (pit_type)
  (cond ((eq pit_type 1) "\U+05E9. \U+05DB\U+05DC\U+05DC\U+05D9\U+05EA")
        ((eq pit_type 2) "\U+05E9. \U+05D1\U+05D9\U+05D5\U+05D1")
		((eq pit_type 3) "\U+05E9. \U+05E0\U+05D9\U+05E7\U+05D5\U+05D6")
		((eq pit_type 4) "\U+05E9. \U+05EA\U+05E7\U+05E9\U+05D5\U+05E8\U+05EA")
		((eq pit_type 5) "\U+05E7\U+05D5\U+05DC\U+05D8\U+05DF")
		((eq pit_type 6) "\U+05E9. \U+05D7\U+05E9\U+05DE\U+05DC")
		((eq pit_type 7) "\U+05E9. \U+05DE\U+05D9\U+05DD")
		((eq pit_type 8) "\U+05E9. \U+05D4\U+05D5\U+05D8")
		((eq pit_type 9) "\U+05E9. \U+05E4\U+05E8\U+05D8\U+05E0\U+05E8")
		((eq pit_type 10) "\U+05E9. \U+05E1\U+05DC\U+05E7\U+05D5\U+05DD")
		((eq pit_type 11) "\U+05E9. \U+05D1\U+05D6\U+05E7")
		((eq pit_type 12) "\U+05E9. \U+05E8\U+05DE\U+05D6\U+05D5\U+05E8")
		((eq pit_type 13) "\U+05E9. \U+05EA\U+05D0\U+05D5\U+05E8\U+05D4")
		((eq pit_type 14) "\U+05E9. \U+05D3\U+05DC\U+05E7")
		((eq pit_type 15) "\U+05E9. \U+05D2\U+05D6")
		((eq pit_type 16) "\U+05E9. \U+05DE\U+05D2\U+05D5\U+05E3")
		((eq pit_type 17) "\U+05E9. \U+05D4\U+05E9\U+05E7\U+05D9\U+05D4")
		((eq pit_type 18) "\U+05E9. \U+05E1\U+05D9\U+05D1 \U+05D0\U+05D5\U+05E4\U+05D8\U+05D9")
		((eq pit_type 19) "\U+05E9. \U+05D4\U+05D0\U+05E8\U+05E7\U+05D4")
        (t "\U+05E9. \U+05DB\U+05DC\U+05DC\U+05D9\U+05EA")))

(defun modify_block (blname p_number p_type p_tl p_il p_in p_out)
	(setq atlist nil) ; initialize empty list
	(if blname
		
			(if (= "INSERT" (cdr (assoc 0 (setq bldata (entget blname))))) ; if insert
				(if (setq blflag (cdr (assoc 66 bldata))) ; if it has atts follow flag
					(if (= 1 blflag) ; if atts follow flag indicates atts
						(while (= "ATTRIB"
							(cdr (assoc 0 (setq bldata (entget (setq blname (entnext blname))))))
						)
						(setq atlist (cons (list (strcase (cdr (assoc 2 bldata))) bldata) atlist)))
					)
				)
			)
		
	)
	
	(setq dout "D-OUT[M]")
	(setq din "D-INSIDE[M]")
	(setq pil "I.L/H=")
	(setq ptl "T.L=")
	(setq ptype "TYPE")
	(setq pnumber "NUMBER")
	
	(if (= p_type (get_pit_text bezeq_code))
		(progn 
			(setq dout "D-OUTSIDE")
			(setq din "D-INSIDE")
			(setq pil "I.L=")
			(setq ptl "T.L=")
			(setq ptype "BEZEQ")
			(setq pnumber "NUMBER")
		)
	)
	
	(if (= p_type (get_pit_text koltan_code))
		(progn
			(setq dout "D-OUTSIDE")
			(setq din "D-INSIDE")
			(setq pil "I.L=")
			(setq ptl "T.L=")
			(setq ptype "KOLTAN")
			(setq pnumber "NUMBER")
		)
	)
	
	(setq desc_data (assoc dout atlist))
	(setq desc_entity_data (cadr desc_data))
	(setq desc_entity_data (subst (cons 1 p_out) (assoc 1 desc_entity_data) desc_entity_data ))
	(entmod desc_entity_data)
	
	(setq desc_data (assoc din atlist))
	(setq desc_entity_data (cadr desc_data))
	(setq desc_entity_data (subst (cons 1 p_in) (assoc 1 desc_entity_data) desc_entity_data ))
	(entmod desc_entity_data)
	
	(setq desc_data (assoc pil atlist))
	(setq desc_entity_data (cadr desc_data))
	(setq desc_entity_data (subst (cons 1 p_il) (assoc 1 desc_entity_data) desc_entity_data ))
	(entmod desc_entity_data)
	
	(setq desc_data (assoc ptl atlist))
	(setq desc_entity_data (cadr desc_data))
	(setq desc_entity_data (subst (cons 1 p_tl) (assoc 1 desc_entity_data) desc_entity_data ))
	(entmod desc_entity_data)
	
	(setq desc_data (assoc ptype atlist))
	(setq desc_entity_data (cadr desc_data))
	(setq desc_entity_data (subst (cons 1 p_type) (assoc 1 desc_entity_data) desc_entity_data ))
	(entmod desc_entity_data)
	
	(setq desc_data (assoc pnumber atlist))
	(setq desc_entity_data (cadr desc_data))
	(setq desc_entity_data (subst (cons 1 p_number) (assoc 1 desc_entity_data) desc_entity_data ))
	(entmod desc_entity_data)
	
	(if (= p_type (get_pit_text magof_code))
		(progn
			(setq desc_data (assoc "MARK" atlist))
			(setq desc_entity_data (cadr desc_data))
			(setq desc_entity_data (subst (cons 1 magof) (assoc 1 desc_entity_data) desc_entity_data ))
			(entmod desc_entity_data)
		)
	)
)

(defun c:zz ()
	(setq blname (car (entsel "\nSelect pit block: ")))
	(setq number (getint "\nEnter pit number: "))
	(setq type_p (getint "\nEnter pit type: "))
	(setq tl (getreal "\nEnter pit T.L (meters): "))
	(setq depth (getint "\nEnter depth (centimeters): ")) 
	(setq width (getint "\nEnter width (centimeters): ")) 
	(setq height (getint "\nEnter height (centimeters): ")) 
	(setq diamet (getint "\nEnter cover (centimeters): "))
	
	(setq save_tl tl)
	(setq start_point nil)
	
	(setq azimut_map zero_azimut_map)
	
	(setq blkname (cdr (assoc 2 (entget blname))))
	
	(if (or (and (= blkname "koltan") (/= type_p koltan_code)) (and (/= blkname "koltan") (= type_p koltan_code)))
		(progn
			(alert (strcat "Block used (koltan) does not match pit type (" (itoa koltan_code) ")"))
			(exit)
		)
	)
	
	(if (or (and (= blkname "BEZEQQQ") (/= type_p bezeq_code)) (and (/= blkname "BEZEQQQ") (= type_p bezeq_code)))
		(progn
			(alert (strcat "Block used (BEZEQQQ) does not match pit type (" (itoa bezeq_code) ")"))
			(exit)
		)
	)
	
	(setq depth_m (/ depth 100.0))
	(setq tl_s (strcat "T.L=" (rtos tl 2 2)))
	(setq il_s (strcat "I.L=" (rtos (- tl depth_m) 2 2)))
	(setq width_m (rtos (/ width 100.0) 2 2))
	(setq height_m (rtos (/ height 100.0) 2 2))
	(setq pit_out (strcat "D= " (rtos (/ diamet 100.0) 2 2) " OUT"))
	
	(setq number_s (itoa number))
	(setq type_text (get_pit_text type_p))
	(setq pit_in "D= XXX IN")
	
	(if (= width height)
		(setq pit_in (strcat "D= " (rtos (/ width 100.0) 2 2) " IN"))
		(setq pit_in (strcat "D= " (rtos (/ width 100.0) 2 2) "X" (rtos (/ height 100.0) 2 2) " IN"))
	)
	
	(setq new_layer (get_layer type_p))
	(entmod (subst (cons 8 new_layer) (assoc 8 (entget blname)) (entget blname)))
	
	(modify_block blname number_s type_text tl_s il_s pit_in pit_out)
	(c:ff number type_p depth width height diamet)
)

(defun round (curr_angle)
	(setq new_angle curr_angle)
	(setq digit (rem curr_angle 10))
	(if (>= digit 5)
		(progn
			(setq new_angle (+ new_angle (- 10 digit)))
			(if (>= new_angle 360)
				(setq new_angle 0)
			)
		)
		(progn
			(setq new_angle (- new_angle digit))
		)
	)
	
	new_angle
)


(defun c:xx ()
	(if (= start_point nil)
		(setq start_point (getpoint "\nClick on the pit's center."))
	)
	(setq pointx (car start_point))
	(setq pointy (cadr start_point))
	(setq line_len 2)
	(setq text_h 0.12)
	(setq text_offset 0.01)
	(setq shift_val 10)
	(setq shift_val_left 5)
	
	(setq amount (getint "\nEnter amount: "))
	(setq size_t (getreal "\nEnter size in inches: "))
	(setq depth_t (getint "\nEnter IL (centimeters): "))
	(setq azimut (getint "\nEnter azimut: "))
	
	(setq angle_degrees (round azimut))

	(setq angle (* (/ (convert_angle angle_degrees) 180.0) pi)) ; Convert 30 degrees to radians

	; Calculate endpoint coordinates after rotation
	(setq dx (* line_len (cos angle)))
	(setq dy (* line_len (sin angle)))

	(setq end_point (list (+ (car start_point) dx) (+ (cadr start_point) dy))) ; Calculate endpoint coordinates
	
	; retreive amount from azimut map
	(setq curr_val (cdr (assoc angle_degrees azimut_map)))

	(setq angle2 (* (/ (convert_angle (+ angle_degrees (* shift_val curr_val))) 180.0) pi))
	(setq dx2 (* (/ line_len 2) (cos angle2)))
	(setq dy2 (* (/ line_len 2) (sin angle2)))
	(setq text_startp (list (+ (car start_point) dx2) (+ (cadr start_point) dy2)))
	
	(if (and (>= angle_degrees 0) (< angle_degrees 90))
		(setq text_startp (list (- (+ (car start_point) dx2) text_offset) (+ (cadr start_point) dy2 text_offset)))
	)
	(if (and (>= angle_degrees 90) (< angle_degrees 180))
		(setq text_startp (list (+ (car start_point) dx2 text_offset) (+ (cadr start_point) dy2 text_offset)))
	)
	
	(setq text_height (list (car text_startp) (+ (cadr text_startp) text_h)))
	(setq text_endp (list (+ (car text_startp) dx) (+ (cadr text_startp) dy)))
	
	(setq opposite_angle (* (/ (convert_angle (- (- angle_degrees 180) (* shift_val curr_val))) 180.0) pi))
	(setq dx1 (* line_len (cos opposite_angle)))
	(setq dy1 (* line_len (sin opposite_angle)))
	
	;(setq angle_for_text (* (/ (convert_angle (- angle_degrees (* shift_val curr_val))) 180.0) pi))
	(setq angle_for_text (* (/ (+ (convert_angle angle_degrees) (* shift_val_left curr_val)) 180.0) pi))

	(setq dx_for_text (* line_len (cos angle_for_text)))
	(setq dy_for_text (* line_len (sin angle_for_text)))
	(setq end_point_text (list (+ (car start_point) dx_for_text) (+ (cadr start_point) dy_for_text)))
	
	(setq opposite_angle2 (* (/ (convert_angle (- angle_degrees 180)) 180.0) pi))
	(setq dx3 (* line_len (cos opposite_angle2)))
	(setq dy3 (* line_len (sin opposite_angle2)))
	
	(if (and (>= angle_degrees 180) (< angle_degrees 270))
		(progn
			(setq text_startp (list (- (car end_point_text) text_offset) (+ (cadr end_point_text) text_offset)))
			(setq text_height (list (car text_startp) (+ (cadr text_startp) text_h)))
			(setq text_endp (list (+ (car text_startp) dx3) (+ (cadr text_startp) dy3)))
		)
	)
	(if (and (>= angle_degrees 270) (< angle_degrees 360))
		(progn
			(setq text_startp (list (+ (car end_point_text) text_offset) (+ (cadr end_point_text) text_offset)))
			(setq text_height (list (car text_startp) (+ (cadr text_startp) text_h)))
			(setq text_endp (list (+ (car text_startp) dx3) (+ (cadr text_startp) dy3)))
		)
	)
	

	; Toggles off osnap mode - for best drawing
	(setvar 'osmode 0)
	
	; check if it's 1 in map if no draw it and update to 1 otherwise don't draw it !
	(setq il_pipe (strcat (rtos (- save_tl (/ depth_t 100.0)) 2 2)))
	(setq pipe_text (strcat (itoa amount) "X" (rtos size_t) "\"-I.L=" il_pipe))
	(command "_text" text_startp text_height text_endp pipe_text)
	(setq obj (entlast)) 
	(c:AddShape obj)
	
	(if (= curr_val 0)
		(progn
			(command "_.line" start_point end_point "") ; Draw the rotated line
			(setq obj (entlast)) 
			(c:AddShape obj)
		)
	)
	
	(setvar 'osmode 3583)
	
	(c:gg amount size_t depth_t azimut)
	
	(setq entry (assoc angle_degrees azimut_map))
	(if entry
		(progn
		  (setq new_value (+ (cdr entry) 1)) ; Add one to the current value
		  (setq new_entry (cons angle_degrees new_value))
		  (setq new_azimut_map (subst new_entry entry azimut_map))
		  (setq azimut_map new_azimut_map)
		  (princ)
		)
	)
	
)
