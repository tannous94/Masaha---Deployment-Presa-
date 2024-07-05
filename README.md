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
