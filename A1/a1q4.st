Object subclass: SortedTree [
    | val left right |

    SortedTree class >> new: aNumber [
        | r |
        r := super new.
        r init: aNumber.
        ^ r
    ]

    init: root [
        val := root.
        left := nil.
        right := nil.
    ]

    add: x [
        x < val ifTrue: [
            left = nil ifTrue: [
                left := SortedTree new: x.
            ] ifFalse: [
                left add: x
            ].
        ].
        x > val ifTrue: [
            right = nil ifTrue: [
                right := SortedTree new: x.
            ] ifFalse: [
                right add: x
            ].
        ].
    ]

    displayString [
        |r|
        r := ''.
        left = nil ifFalse: [
            r := r, '(', (left displayString), ')', ' '.
        ].
        r := r, (val displayString).
        right = nil ifFalse: [
            r := r, ' ', '(', (right displayString), ')' .
        ].
        ^ r
    ]

    printString [
        ^ self displayString
    ]

    do: block [
        left = nil ifFalse: [
            left do: block
        ].
        block value: val.
        right = nil ifFalse: [
            right do: block
        ].
    ]

    reduce: block [
        |sum|
        sum := nil.
        self do: [:v|
            (sum = nil) ifTrue: [
                sum := v
            ] ifFalse: [
                sum := block value: sum value: v
            ]
        ].
        ^ sum.
    ]
]