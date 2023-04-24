



HasntkellVar extend [
    substitute: var for: newVar [
        name = var ifTrue: [
            ^ newVar
        ].    
        ^self
    ]

    noe: globals [
        "in noe it doesn't reduce itself"
        (globals includesKey:name) ifTrue: [
            ^ ((globals at: name) dup)
        ] ifFalse: [
            ^nil
        ]
    ] 
]

HasntkellAbs extend [
    substitute: oldVar for: newVar [
        |free hadKey|
        var = oldVar ifFalse: [
            free := newVar freeVars.
            hadKey := free includesKey: var.
            hadKey ifFalse: [
                body := body substitute: oldVar for: (newVar dup).
            ] ifTrue: [
                |nvar|
                nvar := var , (self hash asString).
                body := body substitute: var for: (HasntkellVar withName: nvar).
                var := nvar.
                body := body substitute: oldVar for: (newVar dup).
            ].
        ].
        ^self
    ]

    noe: globals [
        "in noe it doesn't reduce itself" 
        ^nil
    ] 
]

HasntkellApp extend [

    substitute: var for: newVar [
        rator := rator substitute: var for: (newVar dup).
        rand := rand substitute: var for: (newVar dup).
        ^self
    ]

    noe: globals [
        |var body|
        "first try to reduce the left side, which should result in something"
        "based on the result value of left, return the substitue result"
        rator isError ifTrue: [
            ^ rator
        ].
        rand isError ifTrue: [
            ^ rand
        ].

        rator isAbs ifTrue: [
            var := rator var.
            body := rator body.
            body := body substitute: var for: (rand dup).
            ^ body
        ]. 

        rator isIOAbs ifTrue: [
            rator := rator apply: (rand dup).
            ^rator
        ].

        rator := (rator noe: globals).
        rator isNil ifTrue: [
            ^nil
        ] ifFalse: [
            ^self
        ]

    ] 

]


HasntkellNum extend [

    substitute: var for: newVar [
        ^self
    ]

    noe: globals [
        ^nil
    ] 
]


HasntkellNumExp extend [

    substitute: var for: newVar [
        left := left substitute: var for: (newVar dup).
        right := right substitute: var for: (newVar dup).
        ^self
    ]


    noe: globals [
        |leftReduction rightReduction|
        leftReduction := left noe: globals.
        leftReduction isNil ifFalse: [
            left := leftReduction.
            ^ self
        ]. 
        rightReduction := right noe: globals.
        rightReduction isNil ifFalse: [
            right := rightReduction.
            ^ self
        ]. 

        "if both return nil, time to evaluate"
        left isNum ifTrue: [
             right isNum ifTrue: [
                op = '+' ifTrue: [
                    ^ HasntkellNum withValue: ((left value) + (right value))
                ].
                op = '-' ifTrue: [
                    ((left value) - (right value)) < 0 ifTrue: [
                            ^ HasntkellNum withValue: 0
                    ] ifFalse: [
                            ^ HasntkellNum withValue: ((left value) - (right value)) 
                    ].
                ].
                op = '*' ifTrue: [
                    ^ HasntkellNum withValue: ((left value) * (right value))
                ].
                op = '=' ifTrue: [
                    (left value) = (right value) ifTrue: [
                        ^ HasntkellTrue new
                    ] ifFalse: [
                        ^ HasntkellFalse new
                    ]
                ].
             ].
        ].

        ^nil
    ] 
]


HasntkellBoolLiteral extend [
    substitute: var for: newVar [
        ^self
    ]

    noe: globals [
        ^nil
    ] 
]


HasntkellIf extend [
    substitute: var for: newVar [
        condition := condition substitute: var for: (newVar dup).
        thenExp := thenExp substitute: var for: (newVar dup).
        elseExp := elseExp substitute: var for: (newVar dup).
        ^self
    ]

    noe: globals [
        |condReduced|
        condReduced := condition noe: globals.
        condReduced isNil ifFalse: [
            condition := condReduced.
            ^ self.
        ] ifTrue: [
            "we need to make sure we can reduce"
            condition isTrue ifTrue: [
                ^ thenExp
            ].
            condition isFalse ifTrue: [
                ^ elseExp
            ].
            ^nil "stuck"
        ]
    ] 
]

HasntkellError extend [
    substitute: var for: newVar [
        ^self
    ]

    noe: globals [
        ^nil
    ] 

]

HasntkellIOAbs extend [
    substitute: var for: newVar [
        ^self
    ]

    noe: globals [
        ^self
    ]
]

HasntkellIO extend [
    substitute: var for: newVar [
        ^self
    ]

    noe: globals [
        ^nil
    ]

]


Object subclass: Hasntkell [
    |exp globals|
    
    Hasntkell class >> new: exp withGlobals: dict[
        | r |
        r := super new.
        r init: exp andGlobal: dict.
        ^ r
    ]

    init: sexp andGlobal: dict [
        exp := sexp.
        globals := dict.

    ]


    eval [
        |potential|
        " exp := exp noe: globals. "
        exp := (exp reduceWith: [:x | 
            (x noe: globals)
        ]).
        ^exp
    ]
]




Object subclass: HasntkellRefLib [
    HasntkellRefLib class >> globals: dict [
        dict at: 'ref' put: (HasntkellIOAbs withConstructor: HasntkellRef).
        dict at: 'get' put: (HasntkellIOAbs withConstructor: HasntkellGet).
        dict at: 'put' put: (HasntkellIOAbs withConstructor: HasntkellCurryPut).

        ^dict
    ]
]



HasntkellIO subclass: HasntkellRef [
   | exp |

    HasntkellRef class >> new: sexp [
        | r |
        r := super new.
        r init: sexp.
        ^r
    ]

    init: sexp [
        exp := sexp.
    ]

    dup [
        ^HasntkellRef new: exp dup
    ]

    performIO: globals heap: heap [
        | e v key |
        e := Hasntkell new: exp dup withGlobals: globals.
        v := e eval.
        (heap includesKey: '_') ifTrue:[
            key := heap at: '_'.
            heap at: '_' put: (key+1).
        ] ifFalse: [
            key := 0.
            heap at: '_' put: 1.
        ].

        heap at: key put: v.
        ^HasntkellNum withValue: (key asNumber)
    ]
]


HasntkellIO subclass: HasntkellGet [
   | exp |

    HasntkellGet class >> new: sexp [
        | r |
        r := super new.
        r init: sexp.
        ^r
    ]

    init: sexp [
        exp := sexp.
    ]

    dup [
        ^HasntkellGet new: exp dup
    ]

    performIO: globals heap: heap [
        | e v key |
        e := Hasntkell new: exp dup withGlobals: globals.
        v := e eval.
        key := v value.
        ^heap at: key
    ]
]



HasntkellIOAbs subclass: HasntkellCurryPut [
    | left |

    HasntkellCurryPut class >> new: sleft [
        | r |
        r := super new.
        r init: sleft.
        ^r
    ]

    init: sleft [
        left := sleft.
    ]

    dup [
        ^HasntkellCurryPut new: left dup
    ]

    apply: exp [
        ^HasntkellPut withLeft: left right: exp
    ]
]

HasntkellIO subclass: HasntkellPut [
    | left right |

    HasntkellPut class >> withLeft: sleft right: sright [
        | r |
        r := super new.
        r init: sleft right: sright.
        ^r
    ]

    init: sleft right: sright [
        left := sleft.
        right := sright.
    ]

    dup [
        ^HasntkellPut withLeft: left dup right: right dup
    ]

    performIO: globals heap: heap [
        | label value key e |
        e := Hasntkell new: left withGlobals: globals.
        label := e eval.
        key := label value.
        e := Hasntkell new: right withGlobals: globals.
        value := e eval.
        heap at: key put: value.
        ^label
    ]
]

