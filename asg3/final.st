#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.

nl := Character nl.
inFile := ''.
outFile := ''.
sortcol := SortedCollection new.
priority := SortedCollection new.
count := 0.

"" 
" FILESTREAM from extendstream.st as is, as was"
""
FileStream extend [
   |bitIndex currentChar|

   nextBit [
      |bit|
      bitIndex isNil ifTrue: [bitIndex := 0].
      bitIndex = 0 ifTrue: [
         bitIndex := 8.
         currentChar := self next.
      ].
      bit := currentChar value bitAt: bitIndex.
      bitIndex := bitIndex - 1.
      ^ bit
   ]

   atBitEnd [
      ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
   ]

]


"" 
" BITSTREAM from bitstream.st "
""
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]


"" 
" OUTBITS outbits.st "
" Opens a file given as the first argument and writes out bits."
""
Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]


"" 
" LEAF & TREE tree.st "
" Opens a file given as the first argument and writes out bits."
""
Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   postorder: visitor prefix: string [
       visitor value: char value: string.
   ]

]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   postorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

]


"" 
" FREQUENCY TABLE from charfreq.st "
" Count frequency of input characters "
"" 
Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

freqtable := ZeroArray new: 257.
freqtable at: 256 put: 1.
binarytable := ZeroArray new: 257.


argCount := 0.
Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |
    argCount = 1 ifTrue: [
        inFile := arg.
        file := FileStream open: arg mode: FileStream read.
    ].
    argCount = 2 ifTrue: [
        outFile := FileStream open: arg mode: FileStream write.
    ].
   argCount := argCount + 1.
].
   
[file atEnd not] whileTrue: [
   |ordChar|
   ordChar := file next asInteger.
   freqtable incr: ordChar.
].

freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
      sortcol add: (Leaf new: key count:value).
      count := count + 1.
   ]
].

[sortcol size > 1] whileTrue: [
    |lNode rNode|
    lNode := sortcol removeFirst.
    rNode := sortcol removeFirst.
    tree := Tree new: lNode char count: lNode count + rNode count left: lNode right: rNode.
    sortcol add: tree. 
].

tree := sortcol removeFirst.
encodedTree := tree.
tree inorder: [:char :string |
        binarytable at: char put: string.
    ] prefix: ''.

encodedTree postorder: [:char :string |
    binarytable at: char put: string.
] prefix: ''.

sortedValsDec := ZeroArray new: 257.

Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |
    
   opt = $d ifTrue: [
            stdout << 'Debug mode' << nl.
    ].
   opt = $t ifTrue: [
            binarytable keysAndValuesDo: [:key :count |
            (count notNil and: [count > 0]) ifTrue: [
                stdout <<  key asCharacter visible << '     '
                       << (freqtable at: key) <<'    '<<count
                       << nl.
         ].
       ].
    ].
   opt = $c ifTrue: [
            outFile ~= '' ifTrue: [
            file := FileStream open: inFile mode: FileStream read.
            compressFile := OutBits new: outFile.
            outFile nextPutAll: count asString.
            outFile cr.
            
            binarytable keysAndValuesDo: [:key :value |
                    (value notNil and: [value > 0]) ifTrue: [

                    outFile nextPutAll: key asString.
                    outFile cr.
                    outFile nextPutAll: (freqtable at: key) asString.
                    outFile cr.
                ]
            ].
            pad := 0.
            [file atEnd not] whileTrue: [
                |ordChar|
               ordChar := file next asInteger.
               ch := (binarytable at: ordChar).
               
               1 to: (ch size) by: 1 do: [:index |
                 outbit := (ch at: index).
                 compressFile writeBit: ch asInteger.
                 pad := pad + 1.
                ].
            ].
            pad := (pad \\ 8).
            pad := 8 - pad.
            pad > 0 ifTrue: [
                1 to: pad by: 1 do: [:index |
                  compressFile writeBit: 0.
                ].
            ].
        ].
        outFile = '' ifTrue: [
            file := FileStream open: inFile mode: FileStream read.
            pad := 0.
            [file atEnd not] whileTrue: [
                |ordChar|
               ordChar := file next asInteger.
               ch := (binarytable at: ordChar).
               
               1 to: (ch size) by: 1 do: [:index |
                 outbit := (ch at: index).
                 stdout << outbit.
                 pad := pad + 1.
                ].
            ].
            pad := (pad \\ 8).
            pad := 8 - pad.
                pad > 0 ifTrue: [
                1 to: pad by: 1 do: [:index |
                  stdout << 0.
                ].
        ].
            stdout << nl.
        ].
    ].
   opt = $u ifTrue: [
       decTable := ZeroArray new: 257.
       linenr := 1.
       inputCount := 0.
       file := FileStream open: inFile mode: FileStream read.
       [file atEnd not] whileTrue: [
               |ordChar|
               linenr = 1 ifTrue: [inputCount := (file nextLine asNumber) * 2].

               ordChar := file nextLine asString.
               linenr < inputCount ifTrue:[
                   freq := file nextLine asString.
                   decTable at: ordChar asInteger put: freq asInteger.
               ].
               linenr := linenr + 2.
           ].
       decTable keysAndValuesDo: [:key :value |
          (value notNil and: [value > 0]) ifTrue: [
              sortcol add: (Leaf new: key count: value).
          ]
        ].
        [sortcol size > 1] whileTrue: [
            |lNode rNode|
            lNode := sortcol removeFirst.
            rNode := sortcol removeFirst.
            tree :=  Tree new: lNode char count: lNode count + rNode count left: lNode right: rNode.
            sortcol add: tree.
        ].
        encodedTree := sortcol removeFirst.
        idx := 1.
        encodedTree postorder: [:char :string |
            treePaths at: idx put: string.
            sortedValsDec at: char put: string.
            idx := idx + 1.
        ] prefix: ''.
        treePaths asSortedCollection: [ :a :b | (a size) < (b size) ].
        file := FileStream open: inFile mode: FileStream read.
        linenr := 1.
        file nextLine.
        1 to: inputCount by: 1 do: [:index |
                 line := file nextLine asString.
           ].
        uncompressFile := BitStream new: file.
        str := ''.
        [uncompressFile atEnd not] whileTrue: [
            bit := uncompressFile nextBit asString.
            str := str, bit.
        ].
        outfile = '' ifTrue: [
        checkStr := ''.
        1 to: (str size) by: 1 do: [:index |
            checkStr := checkStr, (str at: index) asString.
               1 to: (treePaths size) by: 1 do: [:i |
                (treePaths at: i) = checkStr ifTrue: [
                     sortedValsDec keysAndValuesDo: [:key :value |
                           (value notNil and: [key > 31]) ifTrue: [
                            key < 127 ifTrue: [
                            checkStr = value ifTrue: [stdout << key asCharacter.].
                              ].
                           ].
                        ].
                    checkStr := ''.
                ].
            ]. 
        ].
    ].
        outfile ~= '' ifTrue: [
        checkStr := ''.
        1 to: (str size) by: 1 do: [:index |
            checkStr := checkStr, (str at: index) asString.
               1 to: (treePaths size) by: 1 do: [:i |
                (treePaths at: i) = checkStr ifTrue: [
                     sortedValsDec keysAndValuesDo: [:key :value |
                           (value notNil and: [key > 31]) ifTrue: [
                            key < 127 ifTrue: [
                            c  heckStr = value ifTrue: [outfile nextPutAll: (key asCharacter) asString.].
                              ].
                           ].
                        ].
                    checkStr := ''.
                ].
            ]. 
         ].
      ].
   ].
].

