'two = ^f. ^x. f (f x); three = ^f. ^x. f (f (f x)); mul = ^m. ^n. ^f. m (n f); main = mul two three' runHasntkell displayNl.
'main = (^x. x) (^y. y)' runHasntkell displayNl.

'fortytwo = 42; main = fortytwo' runHasntkell displayNl.
'main = fortytwo' runHasntkell displayNl.
 'fortytwo = 14; main = (^fortytwo. fortytwo) 42' runHasntkell displayNl.
'two = ^f. ^x. f (f x); three = ^f. ^x. f (f (f x)); mul = ^m. ^n. ^f. m (n f); main = mul two three (^x. (+ x 1)) 0' runHasntkell displayNl.
'main = * 2 3' runHasntkell displayNl.
'main = - 2 3' runHasntkell displayNl.
'main = - 3 2' runHasntkell displayNl.
'main = true' runHasntkell displayNl.   
'main = false' runHasntkell displayNl.
'main = = 1 1' runHasntkell displayNl.
'main = = 1 2' runHasntkell displayNl.
'main = = 1 (- 2 1)' runHasntkell displayNl.
'factorial = ^x. if (= x 0) then 1 else * x (factorial (- x 1)); main = factorial 12' runHasntkell displayNl.
'factorial = ^x. if (= x 0) then 1 else * x (factorial (- x 1)); main = factorial 21' runHasntkell displayNl.
'isFiveToTen = ^x. if (le x 10) then (ge x 5) else false; main = isFiveToTen 5' runHasntkell displayNl.
'isFiveToTen = ^x. if (le x 10) then (ge x 5) else false; main = isFiveToTen 11' runHasntkell displayNl.
'main = error 42' runHasntkell displayNl.
'infinite = ^x. infinite (+ x 1); main = infinite error' runHasntkell displayNl.
'main = let x = 42 in x' runHasntkell displayNl.
'main = [10 21 42] 3' runHasntkell displayNl.
'main = writeNum 42' runHasntkell displayNl.
'main = bind readNum (^x. writeNum (+ x 30))' runHasntkell displayNl.
'echo = bind readNum (^x. writeNum (+ x 30)); main = bind echo (^x. echo)' runHasntkell displayNl. 
'findZeroBadly = ^r. bind (get r) ^x. if (= x 0) then (writeNum x) else bind (put r (- x 1)) ^ig. findZeroBadly r ; main = bind (ref 42000) ^r. findZeroBadly r' runHasntkell displayNl.
'refFac = ^i. ^o. bind (get i) ^x. bind (get o) ^y. if (= x 0) then writeNum y else bind (put i (- x 1)) ^ig. bind (put o (* x y)) ^ig. refFac i o; main = bind (ref 10) ^i. bind (ref 1) (refFac i)' runHasntkell displayNl.
'refFac = ^i. ^o. bind (get i) ^x. bind (get o) ^y. if (= x 0) then writeNum y else bind (put i (- x 1)) ^ig. bind (put o (* x y)) ^ig. refFac i o; refFacIn = bind readNum ^i. bind (ref i) ^i. bind (ref 1) ^o. refFac i o; refFacN = ^i. if (le i 1) then refFacIn else bind refFacIn ^ig. (refFacN (- i 1)); main = bind readNum refFacN' runHasntkell displayNl.
