Object subclass: RunningSum [
    |lst|
    
    RunningSum class >> new [
        | r |
        r := super new.
        r init.
        ^ r
    ]

    init [
        lst := OrderedCollection new.
    ]

    updateWith: aNumber [
        lst add: aNumber.
    ]
    
    mean [  
        |sum|
        sum := 0.
        lst do: [:v|
            sum := sum + v
        ].
        ^ (sum / (lst size)) asFloat
    ]
    
    stdev [
        |sum m|
        sum := 0.
        m := self mean.
        lst do: [:v|
            sum := sum + (((v - m) abs) raisedTo: 2).
        ].
        ^ ((sum / (lst size)) sqrt) asFloat
    ]
]