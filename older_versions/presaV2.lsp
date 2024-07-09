;; Global variables to store:
(setq shapes '())

(setq circle_list '())

(setq dim_list '())

(setq offsetx 0.0)
(setq offsety 0.0)

(setq top_offset 0.0)
(setq bottom_offset 0.0)
(setq right_offset 0.0)
(setq left_offset 0.0)

(setq w 0.0)
(setq h 0.0)
(setq d 0.0)
(setq cx 0.0)
(setq cy 0.0)

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
  (setq offsetx (getint "\nEnter offset x: ")) 
  (setq offsety (getint "\nEnter offset y: ")) 
)

(defun c:zz ()
  (setq edge_height (getint "\nEnter depth: ")) 
  (setq main_width (getint "\nEnter width: ")) 
  (setq main_height (getint "\nEnter height: ")) 
  (setq diamet (getint "\nEnter cover: "))
  (setq zero 0.0)
  
  ; Save them for later use:
  (setq w main_width)
  (setq h main_height)
  (setq d edge_height)
  
  ; Reset lists:
  (setq shapes '()) 
  (setq circle_list '()) 
  (setq dim_list '()) 
  
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
			(setq ccx (/ (+ (car r1) (car r2)) 2.0)) 
			(setq ccy (/ (+ (cadr r1) (cadr r2)) 2.0))
			(setq center (list ccx ccy))
			(command "_CIRCLE" center (/ sinch 2.0))
			(command "_text" (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (list (- ccx size_t) (+ (- ccy size_t) size_t)) (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (strcat (rtos size_t) "\""))
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
			(setq ccx (/ (+ (car r1) (car r2)) 2.0)) 
			(setq ccy (/ (+ (cadr r1) (cadr r2)) 2.0))
			(setq center (list ccx ccy))
			(command "_CIRCLE" center (/ sinch 2.0))
			(command "_text" (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (list (- ccx size_t) (+ (- ccy size_t) size_t)) (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (strcat (rtos size_t) "\""))
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
			(setq ccx (/ (+ (car r1) (car r2)) 2.0)) 
			(setq ccy (/ (+ (cadr r1) (cadr r2)) 2.0))
			(setq center (list ccx ccy))
			(command "_CIRCLE" center (/ sinch 2.0))
			(command "_text" (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (list (- ccx size_t) (+ (- ccy size_t) size_t)) (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (strcat (rtos size_t) "\""))
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
			(setq ccx (/ (+ (car r1) (car r2)) 2.0)) 
			(setq ccy (/ (+ (cadr r1) (cadr r2)) 2.0))
			(setq center (list ccx ccy))
			(command "_CIRCLE" center (/ sinch 2.0))
			(command "_text" (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (list (- ccx size_t) (+ (- ccy size_t) size_t)) (list (- ccx (* size_t spointx)) (- ccy (* size_t spointy))) (strcat (rtos size_t) "\""))
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
  
  (setvar 'osmode 3583)
 
  (princ "\nDrawn successfully.")
 
  (princ)
)

(defun c:bb ()
  (setq new_layer "Layer2")  ; Put dim layer name
  
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
  (setq new_layer "Layer1") ; Put circle layer name
  
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
