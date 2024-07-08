HOW TO USE ?
* Load the script file into AutoCad using APPLOAD command
* Once loaded follow the usage below

USAGE

```
command zz: Deploys the pit
Takes 4 params, hit enter after entering each param
* Depth - the depth of the pit
* Width - if circle, then the diameter of the inside of the pit
* Height - if circle, then the diameter of the inside of the pit
* Cover - the diameter of the cover of the pit
```

```
command xx: Draws the lines for each of the inside pipes
Takes 3 params, hit enter after entering each param
* Amount - the amount of pipes of the specific size (that's been given in the next param) - ONLY available in version 2.0
* Size - size of the pipe in inches
* IL - depth of the pipe inside the pit (starting from the top)
* Azimut - angle of the pipe inside the pit
```

```
command vv: Colors the cover's layer
```

```
command bb: Colors the dimlinear lines' layer (all the measurements)
```

```
command setOffsets: Changes the offset for the whole deployment to change its location (default 0.0 x 0.0)
Takes 2 params
* x - new x coordinate
* y - new y coordinate
```

```
command nn: Changes the layer of the pipes and all rectanges for the pit (changes the coloring)
Takes 1 param
* pit_type - a number between 1 - 16 according to the following:
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
```
