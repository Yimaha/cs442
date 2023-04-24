LambdaVar extend [
    toDeBruijn: map [
        | hadKey depth |
    "this accept the map of the current state, and modify accordingly
    since this is a variable, it just checks the variable on the map and replace"
        hadKey := map includesKey: name.
        depth := map at: '_'.
        hadKey ifTrue: [name := depth - (map at: name)].
        ^self
    ]

    substitute: var for: newVar [
        name = var ifTrue: [
            ^ newVar
        ].    
        ^self
    ]

    getEta: sdepth [
        ^0
    ]

    eta: depth [
        ^nil "you are never suppose to be here unless no further reduction is possible"
    ]
]

LambdaAbs extend [
    toDeBruijn: map [
        | varRestore |
        varRestore := map at: var ifAbsent: [nil].
        map at: var put: (map at: '_').
        map at: '_' put: ((map at: '_') + 1).
        body := body toDeBruijn: map.
        "restore the map, after you done with it, to ensure table is the same way it came in" 
        map at: '_' put: ((map at: '_') - 1).
        varRestore isNil ifFalse: [
            map at: var put: varRestore.
        ].
        var := nil.
        ^self
    ]


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
                body := body substitute: var for: (LambdaVar withName: nvar).
                var := nvar.
                body := body substitute: oldVar for: (newVar dup).
            ].
        ].
        ^self
    ]

    getBreath: sdepth [
        "retrieve the location of outer most target"
        |depth|
        depth := sdepth.
        body ifVar: [
            ^ 10000000 "unable to reduce"
        ] ifAbs: [
            ^ body getBreath: (sdepth+1)
        ] ifApp: [
            ^ body getBreath: (sdepth+1)
        ].
    ] 


    nor: depth [
        depth = 10000000 ifTrue: [
            ^ nil
        ] ifFalse: [
            depth > 0 ifTrue: [
            "we still havn't hit the reduction layer, thus we are either heading right or left"
            body ifVar: [
                self error: 'traverse failed!'
            ] ifAbs: [
                body := body nor: (depth - 1).
                ^ self
            ] ifApp: [
                body := body nor: (depth - 1).
                ^ self
            ].
            ] ifFalse: [
                ^nil
            ]
        ]
    ]




    tryEta: depth [
        "can eta returns 0 if eta reduction is not possible, depth if possible"
        |expr hadKey free|
        "we are now always on a var"
        body isApp ifFalse: [

            "if the body is not abstraction then nothing you can do"
            ^ 0
        ] ifTrue: [
            expr := body rand.
            expr isVar ifFalse: [
                "the right side of the application must be a var"
                ^ 0
            ] ifTrue: [
                "if the name doesn't match, then not possible to eta reduce"
                expr name = var ifFalse: [
                    ^ 0
                ] ifTrue: [
                    "if the name matches, we need to check if in rest of the body, see if somehow x is a free variable"
                    free := body rator freeVars.
                    hadKey := free includesKey: var.
                    hadKey ifTrue: [
                        "cannot be reduced since it contains something bad"
                        ^ 0
                    ] ifFalse: [
                        ^ depth   
                    ]
                ]
            ]
        ] 
    ]



    getEta: sdepth [
        |depth|
        "goal is to recursively identify the deepest left most eta reduction possible
        if we reach here, we ideally want to dig deeper first before "
        depth := body getEta: (sdepth + 1).
        "if somehowe depth returned a value > 0, it means a possible reduction is found deeper, thus 
        the correct reduction sequence is not the current abstraction"
        depth = 0 ifFalse: [
            ^ depth
        ] ifTrue: [
            "if somehow the above reduction returns 0, the current expression has the potential to be the deepest one, thus we need to verify"
            ^ (self tryEta: sdepth)
        ] 
    ]

    eta: depth [
        depth > 0 ifTrue: [
            "we still havn't hit the reduction layer, thus we are either heading right or left"
            body := body eta: (depth-1).
            ^ self
        ] ifFalse: [
            "the rand should have been rand, left should be the M"
            body isApp ifTrue: [
                body rand isVar ifTrue:[
                    body rand name = var ifTrue:[
                        ^ (body rator)
                    ]
                ]
            ].
            ^ nil
        ]
    ]




]

LambdaApp extend [
    | headLeft |

    toDeBruijn: map [
        rator := rator toDeBruijn: map.
        rand := rand toDeBruijn: map.
        ^self
    ]

    substitute: var for: newVar [
        rator := rator substitute: var for: (newVar dup).
        rand := rand substitute: var for: (newVar dup).
        ^self
    ]

    heading [
        ^ headLeft
    ]

    "get depth function update the left and right depth of all abstraction"
    getDepth: sdepth [
        |depth|
        depth := sdepth.
        rator ifVar: [
            "if rator is a variable, there is nothing you can do to reduce left"
            "if any reduction is possible, we check right,"
            rand ifVar: [
                ^ 0
            ] ifAbs: [
                ^ 0
            ] ifApp: [
                headLeft := false.  
                ^(rand getDepth: (sdepth+1))
            ].
        ] ifAbs: [
            rand ifVar: [
                ^ depth
            ] ifAbs: [
                ^ depth
            ] ifApp: [
                |potentialDepth|
                potentialDepth := (rand getDepth: (sdepth+1)).
                potentialDepth > depth ifTrue: [
                    headLeft := false.
                    ^ potentialDepth
                ] ifFalse: [
                    ^ depth
                ].
            ].
        ] ifApp: [
            "if we have application to the left, we need to check the left application if right is either var or abs (it will never be reduced in this iteration)"
            "if left is also application, then we have a race, who is deeper wins, if same depth, left wins"
            rand ifVar: [
                ^(rator getDepth: (sdepth+1))
            ] ifAbs: [
                ^(rator getDepth: (sdepth+1))
            ] ifApp: [
                |potentialDepthLeft potentialDepthRight|
                potentialDepthLeft := (rator getDepth: (sdepth+1)).
                potentialDepthRight := (rand getDepth: (sdepth+1)).
                potentialDepthLeft >= potentialDepthRight ifTrue: [
                    headLeft := true.
                    ^ potentialDepthLeft
                ] ifFalse: [
                    headLeft := false.
                    ^ potentialDepthRight
                ].
            ].
        ].



    ]

    aoe: depth [
        depth > 0 ifTrue: [
            "we still havn't hit the reduction layer, thus we are either heading right or left"
            rator ifVar: [
                rand ifVar: [
                    self error: 'traverse failed!'
                ] ifAbs: [
                    self error: 'traverse failed!'
                ] ifApp: [
                    rand := rand aoe: (depth - 1).
                    ^ self
                ].
            ] ifAbs: [
                rand ifVar:[
                    self error: 'traverse failed!'
                ] ifAbs: [
                    self error: 'traverse failed!'
                ] ifApp: [
                    rand := rand aoe: (depth - 1).
                    ^ self
                ].
            ] ifApp: [
                rand ifVar:[
                    rator := rator aoe: (depth - 1).
                ] ifAbs: [
                    rator := rator aoe: (depth - 1).
                ] ifApp: [
                    headLeft ifTrue: [
                        rator := rator aoe: (depth - 1). 
                    ] ifFalse: [
                        rand := rand aoe: (depth - 1).
                    ].
                ].
                ^ self

            ].
        ] ifFalse: [
            rator ifVar: [
                "impossible to reduce further, return nil"
                ^nil
            ] ifAbs: [
                | var body |
                var := rator var.
                body := rator body.
                body := body substitute: var for: (rand dup).
                ^ body
            ] ifApp: [
                ^nil
            ].
        ]
    ]

    getBreath: sdepth [
        "retrieve the location of outer most target"
        |depth|
        depth := sdepth.
        rator ifVar: [
            rand ifVar: [
                ^ 10000000 "unable to reduce"
            ] ifAbs: [
                ^(rand getBreath: (sdepth+1))
            ] ifApp: [
                ^(rand getBreath: (sdepth+1))
            ].
        ] ifAbs: [
            "target found"
            ^ depth
        ] ifApp: [
            rand ifVar: [
                ^(rator getBreath: (sdepth+1))
            ] ifAbs: [
                |potentialDepthLeft potentialDepthRight|
                potentialDepthLeft := (rator getBreath: (sdepth+1)).
                potentialDepthRight := (rand getBreath: (sdepth+1)).

                potentialDepthLeft <= potentialDepthRight ifTrue: [
                    headLeft := true.
                    ^ potentialDepthLeft
                ] ifFalse: [
                    headLeft := false.
                    ^ potentialDepthRight
                ].
            ] ifApp: [
                |potentialDepthLeft potentialDepthRight|
                potentialDepthLeft := (rator getBreath: (sdepth+1)).
                potentialDepthRight := (rand getBreath: (sdepth+1)).
                
                potentialDepthLeft <= potentialDepthRight ifTrue: [
                    headLeft := true.
                    ^ potentialDepthLeft
                ] ifFalse: [
                    headLeft := false.
                    ^ potentialDepthRight
                ].
            ].
        ].
    ] 

    nor: depth [
        depth = 10000000 ifTrue: [
            ^ nil
        ] ifFalse: [
            depth > 0 ifTrue: [
                "we still havn't hit the reduction layer, thus we are either heading right or left"
                rator ifVar: [
                    rand ifVar: [
                        self error: 'traverse failed!'
                    ] ifAbs: [
                        rand = rand nor: (depth - 1) 
                    ] ifApp: [
                        rand := rand nor: (depth - 1).
                    ].
                    ^ self

                ] ifAbs: [
                    "this should never happen when depth > 0"
                    self error: 'traverse failed!'
                ] ifApp: [

                    rand ifVar:[
                        rator := rator nor: (depth - 1).
                    ] ifAbs: [
                        headLeft ifTrue: [
                            rator := rator nor: (depth - 1). 
                        ] ifFalse: [
                            rand := rand nor: (depth - 1).
                        ].
                    ] ifApp: [
                        headLeft ifTrue: [
                            rator := rator nor: (depth - 1). 
                        ] ifFalse: [
                            rand := rand nor: (depth - 1).
                        ].
                    ].
                    ^ self

                ].
            ] ifFalse: [
                rator ifVar: [
                    "impossible to reduce further, return nil"
                    ^nil
                ] ifAbs: [
                    | var body |
                    var := rator var.
                    body := rator body.
                    body := body substitute: var for: (rand dup).
                    ^ body
                ] ifApp: [
                    ^nil
                ].
            ].   
        ]
    ]

    getEta: sdepth [
        |potentialDepthLeft potentialDepthRight|
        "in this stage, it is unclear which side has deeper nabla possible, thus youi just call both side and check who is longer"
        potentialDepthLeft := (rator getEta: (sdepth+1)).
        potentialDepthRight := (rand getEta: (sdepth+1)).

        potentialDepthLeft >= potentialDepthRight ifTrue: [
            headLeft := true.
            ^ potentialDepthLeft
        ] ifFalse: [
            headLeft := false.
            ^ potentialDepthRight
        ].
    ]
   
    eta: depth [
        depth > 0 ifTrue: [
            "we still havn't hit the reduction layer, thus we are either heading right or left"
            headLeft ifTrue: [
                rator := rator eta: (depth - 1)
            ] ifFalse: [
                rand := rand eta: (depth - 1)
            ].
            ^ self
        ] ifFalse: [
            ^nil "at depth = 0, you are suppose to be in an abstraction"
        ]
    ]

]


Object subclass: Lambda [
    |exp|
    
    Lambda class >> new: exp [
        | r |
        r := super new.
        r init: exp.
        ^ r
    ]

    init: sexp [
        exp := sexp.
    ]

    toDeBruijn [
        | map |
        "for convinient reason, key _ of the map keep track of the depth"
        map := (Dictionary new).
        map at: '_' put: 1.
        ^ (exp toDeBruijn: map)
    ]

    "idea:
    AOE is a leftmost of the inner most evaluation, each iteration of the tree
    we either receive app, abs, or var
    
    in case of var, we simply return to top level with var
    in case of abs, we will not reduce the body, meaning we simply return as well
    in case of App, we have a problem, cause we don't know how deep the right or left go, thus no idea who is deeper
    this means we need 2 traversal, first time check which of the left or right is deeper
    2nd will allow us to call reduction on the right side"

    aoe [
        exp isNil ifFalse: [
            exp ifVar: [
                "no more reduction possible, return nil"
                exp := nil.
            ] ifAbs: [
                "no more reduction possible, return nil"
                exp := nil.
            ] ifApp: [
                "more reduction might be possible, need to check length"
                exp := (exp aoe: (exp getDepth: 0)).
            ].
        ].
        ^exp


    ]

    aoe: steps [
        ^ (exp reduceWith: [:x | 
            x ifVar: [
                "no more reduction possible, return nil"
                nil
                
            ] ifAbs: [
                "no more reduction possible, return nil"
                nil
            ] ifApp: [
                |potential|
               "more reduction might be possible, need to check length"
                potential := (x aoe: (x getDepth: 0)).
                potential isNil ifFalse: [
                    exp := potential.
                ].
                potential
            ].
        ] steps: steps)
    ]

    nor [
        exp isNil ifFalse: [
            exp ifVar: [
                "no more reduction possible, return nil"
                exp := nil.
            ] ifAbs: [
                "no more reduction possible, return nil"
                exp := (exp nor: (exp getBreath: 0)). 
            ] ifApp: [
                "more reduction might be possible, need to check length"
                exp := (exp nor: (exp getBreath: 0)).
            ].
        ].
        ^exp
    ]
    nor: steps [
        ^ (exp reduceWith: [:x | 
            |potential|

            x ifVar: [
                "no more reduction possible, return nil"
                nil
                
            ] ifAbs: [
               "more reduction might be possible, need to check length"
                potential := (x nor: (x getBreath: 0)).
                potential isNil ifFalse: [
                    exp := potential.
                ].
            ] ifApp: [
               "more reduction might be possible, need to check length"
                potential := (x nor: (x getBreath: 0)).
                potential isNil ifFalse: [
                    exp := potential.
                ].
            ].
            potential
        ] steps: steps)
    ]

    eta [

        "the idea of eta reduction is that you need to dig into the lambda"
        "since you want the left most deepest one, the idea to identify the left most deepest one is keep 
        going down until you found a match"
        exp isNil ifFalse: [
            exp ifVar: [
                "no more reduction possible, return nil"
                exp := nil.
            ] ifAbs: [
                "no more reduction possible, return nil"
                exp := (exp eta: (exp getEta: 0)).
            ] ifApp: [
                "more reduction might be possible, need to check length"
                exp := (exp eta: (exp getEta: 0)).
            ].
        ].
        ^exp
    ]


    eta: steps [
        |potential|
        ^ (exp reduceWith: [:x | 
            x ifVar: [
                "no more reduction possible, return nil"
                nil
                
            ] ifAbs: [
               "more reduction might be possible, need to check length"
                potential := (exp eta: (exp getEta: 0)).
                potential isNil ifFalse: [
                    exp := potential.
                ].
            ] ifApp: [
               "more reduction might be possible, need to check length"
                potential := (exp eta: (exp getEta: 0)).
                potential isNil ifFalse: [
                    exp := potential.
                ].
            ].
            potential
        ] steps: steps)
    ]
]


