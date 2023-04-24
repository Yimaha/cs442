| x l s |
x := LambdaParser parse: '(^f.((^f.(^x.(f (f x)))) ((^f.(^x.(f (f x)))) f)))'.
l := Lambda new: x.
x displayNl.
l toDeBruijn displayNl.

x := LambdaParser parse: '(^mul.^two.mul two two) (^m.^n.^f.m(n f)) (^f.^x.f (f x))'.
x displayNl.
l := Lambda new: x.
x := l aoe: 1000.
x displayNl.

l toDeBruijn displayNl.
l aoe displayNl.
x := LambdaParser parse: '(^mul.^two.mul two two) (^m.^n.^f.m(n f)) (^f.^x.f (f x))'.
l := Lambda new: x.
x displayNl.
x := l aoe: 1000.
s := x displayString.
x displayNl.
l toDeBruijn displayNl.
"(^.((^.(^.(2 (2 1)))) ((^.(^.(2 (2 1)))) 1)))"
"((^two.(((^m.(^n.(^f.(m (n f))))) two) two)) (^f.(^x.(f (f x)))))"
'eyyy' displayNl.
x := LambdaParser parse: s , 'f x'.
l := Lambda new: x.
x displayNl.
l aoe displayNl.
l aoe displayNl.
l aoe displayNl.
l aoe displayNl.
l aoe displayNl.
l aoe displayNl.

x := LambdaParser parse: '(^head.^tail.^cons.^zero.^two.(^succ.(^pred.(pred two)) (^n.(tail(n(^p.cons(succ(head p))
(head p))(cons zero zero))))) (^n.^f.^x.n f(f x))) (^l.l^x.^y.x) (^l.l^x.^y.y) (^h.^t.^s.s h t) (^f.^x.x) (^f.^x.f
(f x)) f x'.
l := Lambda new: x.


x := l aoe: 1000.
l toDeBruijn displayNl.
x displayNl.

x := LambdaParser parse: '(^mul.^two.mul two two) (^m.^n.^f.m(n f)) (^f.^x.f (f x))'.
l := Lambda new: x.
x displayNl.
l nor displayNl.
l nor displayNl.
l nor displayNl.
l nor displayNl.
l nor displayNl.
l nor displayNl.
l nor displayNl.
l nor displayNl.
x := l nor dup.
x displayNl.
l toDeBruijn displayNl.
l := Lambda new: x.
l nor displayNl.
x := LambdaParser parse: '(^mul.^two.mul two two) (^m.^n.^f.m(n f)) (^f.^x.f (f x))'.
l := Lambda new: x.
x := l nor: 1000.
x displayNl.
s := x displayString.
l toDeBruijn displayNl.
x := LambdaParser parse: s , 'f x'.
l := Lambda new: x.
(l nor: 1000) displayNl.
x := LambdaParser parse: '(^head.^tail.^cons.^zero.^two.(^succ.(^pred.(pred two)) (^n.(tail(n(^p.cons(succ(head p))
(head p))(cons zero zero))))) (^n.^f.^x.n f(f x))) (^l.l^x.^y.x) (^l.l^x.^y.y) (^h.^t.^s.s h t) (^f.^x.x) (^f.^x.f
(f x)) f x'.
l := Lambda new: x.
x := l nor: 1000.
x displayNl.
l toDeBruijn displayNl.
x := LambdaParser parse: '(^head.^tail.^cons.^isNull.^nil.^zero.^succ.(^Y.^F.(^len.(len (cons zero (cons zero nil))
)) (Y F)) (^f.(^x.f(x x))(^x.f(x x))) (^f.^l.(isNull l) zero (succ (f(tail l))))) (^l.l(^x.^y.x)) (^l.l(^x.^y.y))
(^h.^t.^s.s h t) (^l.l^h.^t.^x.^y.y) (^s.^x.^y.x) (^f.^x.x) (^n.^f.^x.n f(f x))'.
l := Lambda new: x.
x displayNl.


(l nor: 1000) displayNl.
" x := LambdaParser parse: '^m.^n.^f.^x.m(n f)x'. "
x := LambdaParser parse: '^m.^n.^f.^x.m(n f)x'.
l := Lambda new: x.
x displayNl.
l eta displayNl.
l eta displayNl.

x := LambdaParser parse: '^m.^n.^f.^x.n m f x'.
l := Lambda new: x.
(l eta: 1000) displayNl.

