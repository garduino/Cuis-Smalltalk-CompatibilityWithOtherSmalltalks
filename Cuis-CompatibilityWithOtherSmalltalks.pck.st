'From Cuis 4.2 of 25 July 2013 [latest update: #1782] on 1 August 2013 at 11:54:17.513197 am'!
'Description Please enter a description for this package '!
!provides: 'Cuis-CompatibilityWithOtherSmalltalks' 1 0!
!requires: 'Network-Kernel' nil nil nil!
!classDefinition: #GIFReadWriter category: #'Cuis-CompatibilityWithOtherSmalltalks'!
ImageReadWriter subclass: #GIFReadWriter
	instanceVariableNames: 'width height bitsPerPixel colorPalette rowByteSize xpos ypos pass interlace codeSize clearCode eoiCode freeCode maxCode prefixTable suffixTable remainBitCount bufByte bufStream transparentIndex mapOf32 localColorTable delay loopCount offset'
	classVariableNames: 'Extension ImageSeparator Terminator'
	poolDictionaries: ''
	category: 'Cuis-CompatibilityWithOtherSmalltalks'!
!classDefinition: 'GIFReadWriter class' category: #'Cuis-CompatibilityWithOtherSmalltalks'!
GIFReadWriter class
	instanceVariableNames: ''!

!classDefinition: #TimeStamp category: #'Cuis-CompatibilityWithOtherSmalltalks'!
DateAndTime subclass: #TimeStamp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-CompatibilityWithOtherSmalltalks'!
!classDefinition: 'TimeStamp class' category: #'Cuis-CompatibilityWithOtherSmalltalks'!
TimeStamp class
	instanceVariableNames: ''!

!classDefinition: #ScaledDecimal category: #'Cuis-CompatibilityWithOtherSmalltalks'!
Fraction subclass: #ScaledDecimal
	instanceVariableNames: 'scale'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-CompatibilityWithOtherSmalltalks'!
!classDefinition: 'ScaledDecimal class' category: #'Cuis-CompatibilityWithOtherSmalltalks'!
ScaledDecimal class
	instanceVariableNames: ''!

!classDefinition: #Password category: #'Cuis-CompatibilityWithOtherSmalltalks'!
Object subclass: #Password
	instanceVariableNames: 'cache sequence'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-CompatibilityWithOtherSmalltalks'!
!classDefinition: 'Password class' category: #'Cuis-CompatibilityWithOtherSmalltalks'!
Password class
	instanceVariableNames: ''!

!classDefinition: #NullStream category: #'Cuis-CompatibilityWithOtherSmalltalks'!
Stream subclass: #NullStream
	instanceVariableNames: 'binary position'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-CompatibilityWithOtherSmalltalks'!
!classDefinition: 'NullStream class' category: #'Cuis-CompatibilityWithOtherSmalltalks'!
NullStream class
	instanceVariableNames: ''!

!classDefinition: #TimeStampTest category: #'Cuis-CompatibilityWithOtherSmalltalks'!
TestCase subclass: #TimeStampTest
	instanceVariableNames: 'timestamp aTimeStamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-CompatibilityWithOtherSmalltalks'!
!classDefinition: 'TimeStampTest class' category: #'Cuis-CompatibilityWithOtherSmalltalks'!
TimeStampTest class
	instanceVariableNames: ''!


!GIFReadWriter commentStamp: '<historical>' prior: 0!
Copyright (c) Kazuki Yasumatsu, 1995. All rights reserved.

Used with permission.  Modified for use in Squeak.!

!TimeStamp commentStamp: '<historical>' prior: 0!
This represents a duration of 0 length that marks a particular point in time.!

!ScaledDecimal commentStamp: 'nice 5/16/2009 20:45' prior: 0!
ScaledDecimal implement a special kind of Fraction that prints in decimal notation.
It uses a limited number of digits (scale) after the decimal separation dot and round the result.
Note that a ScaledDecimal does not printOn: exactly, however it will storeOn: exactly because the full precision fraction is kept in memory.

This is mostly usefull with denominators being powers of 10.!

!Password commentStamp: '<historical>' prior: 0!
"Hold a password.  There are three ways to get the password.

If there is no password (sequence == nil), ask the user for it.

If the use supplied one during this session, return that.  It is cleared at shutDown.

If sequence is a number, get the server passwords off the disk.  File 'sqk.info' must be in the same folder 'Squeak.sources' file.  Decode the file.  Return the password indexed by sequence."!

!NullStream commentStamp: 'ar 2/25/2010 14:49' prior: 0!
NullStream is a stream generating and consuming an infinite number of elements. It can be used as an equivalent of /dev/null or for performance benchmarks.!

!TimeStampTest commentStamp: 'brp 7/26/2003 22:44' prior: 0!
This is the unit test for the class TimeStamp.!

!Object methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 2/12/2013 16:21'!
isColorForm
	^false! !

!Object methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 19:14'!
readFromString: aString 
	"Create an object based on the contents of aString."
	^ self readFrom: aString readStream! !

!Collection methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 08:00'!
asDictionary

	^ self as: Dictionary! !

!Collection methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:34'!
contains: aBlock
	"VW compatibility"
	^self anySatisfy: aBlock! !

!SequenceableCollection class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/2/2013 21:07'!
new: newSize streamContents: blockWithArg

	| stream |
	stream := WriteStream on: (self new: newSize).
	blockWithArg value: stream.
	stream position = newSize
		ifTrue: [ ^stream originalContents ]
		ifFalse: [ ^stream contents ]! !

!String methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/14/2013 11:59'!
asByteString
	"Convert the receiver into a ByteString, if possible"
	"Do not raise an error if it's not possible, since my use case is usually one in which WideStrings may or may not have been mutated to something representable in a ByteString, and we mostly do this to save space if possible. If the percentage of such cases are small, it may be better to use isOctetString check first to avoid creating String instances"	
	^self asOctetString! !

!String methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/14/2013 11:58'!
asOctetString
	"Convert the receiver into an octet string, if possible"
	"(IE, I am a WideString containing only character with codePoints < 255, so all of them fit in a latin1-string)."
	| string |
	string := String new: self size.
	1 to: self size do: [:i | string at: i put: (self at: i)].
	^string! !

!String methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/10/2013 19:47'!
asTime
	"Many allowed forms, see Time>>readFrom:"

	^ Time fromString: self.! !

!String methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/10/2013 19:49'!
asTimeStamp
	"Convert from obsolete TimeStamp format"

 	^ TimeStamp fromString: self! !

!String methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 18:52'!
charactersExactlyMatching: aString
	"Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly."

	| count |
	count := self size min: aString size.
	1 to: count do: [:i | 
		(self at: i) = (aString at: i) ifFalse: [
			^ i - 1]].
	^ count! !

!String methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/2/2013 20:46'!
encodeForHTTP

	^ self ! !

!String methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/26/2012 08:42'!
subStrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| result sourceStream subStringStream |
	#Collectn.
	"Changed 2000/04/08 For ANSI <readableString> protocol."
	(separators isString or: [ separators allSatisfy: [ :element | element isKindOf: Character ] ])
		ifFalse: [ ^ self error: 'separators must be Characters.' ].
	sourceStream := self readStream.
	result := OrderedCollection new.
	subStringStream := String new writeStream.
	[ sourceStream atEnd ] whileFalse: [
		| char |
		char := sourceStream next.
		(separators includes: char)
			ifTrue: [
				subStringStream isEmpty ifFalse: [
					result add: subStringStream contents.
					subStringStream := String new writeStream ] ]
			ifFalse: [
				subStringStream nextPut: char ] ].
	subStringStream isEmpty ifFalse: [
		result add: subStringStream contents ].
	^ result asArray! !

!String methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/2/2013 20:45'!
unescapePercentsWithTextEncoding: encodingName 
	^ self! !

!String methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/1/2013 20:44'!
withCRs
	"Return a copy of the receiver in which backslash (\) characters have been replaced with carriage returns."

	^ self collect: [ :c | c = $\ ifTrue: [ Character crCharacter ] ifFalse: [ c ]].! !

!String class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/26/2012 08:42'!
cr
	"Answer a string containing a single carriage return character."

	^ self with: Character crCharacter! !

!String class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/26/2012 08:42'!
crlf
	"Answer a string containing a carriage return and a linefeed."

	^ self with: Character crCharacter with: Character lfCharacter
! !

!String class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 12:43'!
crlfcrlf
	^self crlf , self crlf.! !

!String class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/26/2012 08:42'!
lf
	"Answer a string containing a single Lf character."

	^ self with: Character lfCharacter! !

!Set methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 09:14'!
removeAll
	self init: array size.! !

!Dictionary methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/2/2013 18:15'!
isDictionary
	^true! !

!DirectoryEntry class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 14:42'!
name: name0  creationTime: creationTime  modificationTime: modificationTime   isDirectory: isDirectory  fileSize: fileSize
	"This is the legacy creation method we are trying to phase out.  Please use #directory:  name:  creationTime:  modificationTime:  fileSize:." 
	| type |
	type := isDirectory 
		ifTrue: [ DirectoryEntryDirectory ]
		ifFalse: [ DirectoryEntryFile ].
	^ type
		directory: nil
		name: name0  
		creationTime: creationTime  
		modificationTime: modificationTime   
		fileSize: fileSize! !

!ColorForm methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 2/12/2013 16:20'!
isColorForm
	^true! !

!FileDirectory methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 12:48'!
pathFromUrl: aFileUrl
	
	^String streamContents: [ :s | | first |
		first := false.
		aFileUrl path do: [ :p |
			first ifTrue: [ s nextPut: self pathNameDelimiter ].
			first := true.
			s nextPutAll: p ] ].! !

!Character methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/26/2012 08:41'!
sameAs: aCharacter 
	"Answer whether the receiver is equal to aCharacter, ignoring case"
	^ (self asLowercase = aCharacter asLowercase)! !

!Character class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/26/2012 09:16'!
cr
	"Answer the Character representing a carriage return."

	^ self crCharacter! !

!Character class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/26/2012 09:16'!
lf
	"Answer the Character representing a linefeed."

	^ self lfCharacter! !

!DateAndTime methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/10/2013 19:53'!
asDate

	^ Date starting: self! !

!DateAndTime methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/10/2013 20:43'!
asDateAndTime

	^ self
! !

!DateAndTime methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/10/2013 20:05'!
asDuration
	"Answer the duration since midnight."

	^ Duration seconds: seconds nanoSeconds: nanos
! !

!DateAndTime methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 17:05'!
asSeconds
 	"Return the number of seconds since the Squeak epoch"
 	^ (self - (self class epoch)) asSeconds
 ! !

!DateAndTime methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/10/2013 19:59'!
asTime

	^ Time seconds: seconds nanoSeconds: nanos! !

!DateAndTime methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/14/2013 13:34'!
day
 
 	^ self dayOfYear! !

!Duration methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 16:54'!
asSeconds
 	"Answer the number of seconds in the receiver."
 	^ seconds
 ! !

!Time methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:39'!
hours

	^ self hour! !

!Time methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:39'!
minutes

	^ self minute! !

!Time methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:40'!
seconds

	^ self second! !

!Timespan methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/14/2013 13:59'!
asDateAndTime

	^ start ! !

!Timespan methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/21/2013 13:44'!
day
	"Answer the day of the year represented by the receiver."
	^ self dayOfYear! !

!Timespan class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/10/2013 20:02'!
starting: aDateAndTime 

	^ self starting: aDateAndTime duration: Duration zero ! !

!Timespan class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/10/2013 20:42'!
starting: aDateAndTime duration: aDuration

	^ self basicNew
  		start: aDateAndTime asDateAndTime;
 		duration: aDuration;
		yourself.! !

!Date methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 17:08'!
asSeconds
 	"Answer the seconds since the Squeak epoch: 1 January 1901"
 
 	^ start asSeconds! !

!Date methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/14/2013 13:58'!
subtractDays: dayCount 

	^ (self asDateAndTime - (dayCount days)) asDate! !

!SocketStream methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/1/2013 13:48'!
crlf
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPutAll: String newLineString! !

!PositionableStream methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/2/2013 21:03'!
upToAnyOf: aCollection 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of any object in the collection. If 
	no matching object is found, answer the entire rest of the receiver."
	^self upToAnyOf: aCollection do: [:matchingObject | ]! !

!PositionableStream methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/2/2013 21:03'!
upToAnyOf: subcollection do: aBlock
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of any object in the collection.
	Evaluate aBlock with this occurence as argument.
	If no matching object is found, don't evaluate aBlock and answer the entire rest of the receiver."
	
	^self collectionSpecies new: 1000 streamContents: [ :stream |
		| ch |
		[ self atEnd or: [ (subcollection includes: (ch := self next)) and: [aBlock value: ch. true] ] ] 
			whileFalse: [ stream nextPut: ch ] ]! !

!WriteStream methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 11:48'!
cr
	"Append a return character to the receiver."

	self nextPut: Character crCharacter! !

!WriteStream methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/1/2013 19:32'!
crlf
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPutAll: String newLineString! !

!Transcript class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/1/2013 13:46'!
cr
	"WriteStream protocol.
	In the older TranscriptStream, it added a CR character.
	Now, finish the current incomplete entry."

	self finishEntry! !

!UndefinedObject methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 2/12/2013 15:23'!
ifNotNilDo: aBlock
	"Override to do nothing."

	^ self
! !

!GIFReadWriter methodsFor: 'private'!
checkCodeSize
	(freeCode > maxCode and: [codeSize < 12])
		ifTrue:
			[codeSize := codeSize + 1.
			maxCode := (1 bitShift: codeSize) - 1]! !

!GIFReadWriter methodsFor: 'stream access' stamp: 'bf 5/29/2003 01:23'!
close
	"Write terminator"
	self nextPut: Terminator.
	^super close! !

!GIFReadWriter methodsFor: 'accessing' stamp: 'bf 5/29/2003 01:43'!
delay: aNumberOrNil
	"Set delay for next image in hundredth (1/100) of seconds"
	delay := aNumberOrNil! !

!GIFReadWriter methodsFor: 'private-packing'!
fillBuffer
	| packSize |
	packSize := self next.
	bufStream := ReadStream on: (self next: packSize)! !

!GIFReadWriter methodsFor: 'private-bits access'!
flushBits
	remainBitCount = 0 ifFalse:
		[self nextBytePut: bufByte.
		remainBitCount := 0].
	self flushBuffer! !

!GIFReadWriter methodsFor: 'private-packing'!
flushBuffer
	bufStream isEmpty ifTrue: [^self].
	self nextPut: bufStream size.
	self nextPutAll: bufStream contents.
	bufStream := WriteStream on: (ByteArray new: 256)! !

!GIFReadWriter methodsFor: 'private-encoding'!
flushCode
	self flushBits! !

!GIFReadWriter methodsFor: 'accessing' stamp: 'bf 5/29/2003 01:39'!
loopCount: aNumber
	"Set looping. This must be done before any image is written!!"
	loopCount := aNumber! !

!GIFReadWriter methodsFor: 'private-bits access'!
nextBits
	| integer readBitCount shiftCount byte |
	integer := 0.
	remainBitCount = 0
		ifTrue:
			[readBitCount := 8.
			shiftCount := 0]
		ifFalse:
			[readBitCount := remainBitCount.
			shiftCount := remainBitCount - 8].
	[readBitCount < codeSize]
		whileTrue:
			[byte := self nextByte.
			byte == nil ifTrue: [^eoiCode].
			integer := integer + (byte bitShift: shiftCount).
			shiftCount := shiftCount + 8.
			readBitCount := readBitCount + 8].
	(remainBitCount := readBitCount - codeSize) = 0
		ifTrue:	[byte := self nextByte]
		ifFalse:	[byte := self peekByte].
	byte == nil ifTrue: [^eoiCode].
	^(integer + (byte bitShift: shiftCount)) bitAnd: maxCode! !

!GIFReadWriter methodsFor: 'private-bits access'!
nextBitsPut: anInteger
	| integer writeBitCount shiftCount |
	shiftCount := 0.
	remainBitCount = 0
		ifTrue:
			[writeBitCount := 8.
			integer := anInteger]
		ifFalse:
			[writeBitCount := remainBitCount.
			integer := bufByte + (anInteger bitShift: 8 - remainBitCount)].
	[writeBitCount < codeSize]
		whileTrue:
			[self nextBytePut: ((integer bitShift: shiftCount) bitAnd: 255).
			shiftCount := shiftCount - 8.
			writeBitCount := writeBitCount + 8].
	(remainBitCount := writeBitCount - codeSize) = 0
		ifTrue: [self nextBytePut: (integer bitShift: shiftCount)]
		ifFalse: [bufByte := integer bitShift: shiftCount].
	^anInteger! !

!GIFReadWriter methodsFor: 'private-packing'!
nextByte
	bufStream atEnd
		ifTrue:
			[self atEnd ifTrue: [^nil].
			self fillBuffer].
	^bufStream next! !

!GIFReadWriter methodsFor: 'private-packing'!
nextBytePut: aByte
	bufStream nextPut: aByte.
	bufStream size >= 254 ifTrue: [self flushBuffer]! !

!GIFReadWriter methodsFor: 'accessing' stamp: 'sd 1/30/2004 15:18'!
nextImage
	"Read in the next GIF image from the stream. Read it all into
memory first for speed."

	| f thisImageColorTable |
	stream class == ReadWriteStream ifFalse: [
		stream binary.
		self on: (ReadWriteStream with: (stream contentsOfEntireFile))].

	localColorTable := nil.
	self readHeader.
	f := self readBody.
	self close.
	f == nil ifTrue: [^ self error: 'corrupt GIF file'].

	thisImageColorTable := localColorTable ifNil: [colorPalette].
	transparentIndex ifNotNil: [
		transparentIndex + 1 > thisImageColorTable size ifTrue: [
			thisImageColorTable := thisImageColorTable 
				forceTo: transparentIndex + 1 
				paddingWith: Color white
		].
		thisImageColorTable at: transparentIndex + 1 put: Color transparent
	].
	f colors: thisImageColorTable.
	^ f
! !

!GIFReadWriter methodsFor: 'accessing' stamp: 'ar 10/24/2005 22:52'!
nextPutImage: aForm

	| reduced tempForm |
	aForm unhibernate.
	aForm depth > 8 ifTrue:[
		reduced := aForm colorReduced.  "minimize depth"
		reduced depth > 8 ifTrue: [
			"Not enough color space; do it the hard way."
			reduced := reduced asFormOfDepth: 8].
	] ifFalse:[reduced := aForm].
	reduced depth < 8 ifTrue: [
		"writeBitData: expects depth of 8"
		tempForm := reduced class extent: reduced extent depth: 8.
		(reduced isColorForm) ifTrue:[
			tempForm
				copyBits: reduced boundingBox
				from: reduced at: 0@0
				clippingBox: reduced boundingBox
				rule: Form over
				fillColor: nil
				map: nil.
			tempForm colors: reduced colors.
		] ifFalse: [reduced displayOn: tempForm].
		reduced := tempForm.
	].
	(reduced isColorForm) ifTrue:[
		(reduced colorsUsed includes: Color transparent) ifTrue: [
			transparentIndex := (reduced colors indexOf: Color transparent) - 1.
		]
	] ifFalse: [transparentIndex := nil].
	width := reduced width.
	height := reduced height.
	bitsPerPixel := reduced depth.
	colorPalette := reduced colormapIfNeededForDepth: 32.
	interlace := false.
	self writeHeader.
	self writeBitData: reduced bits.
! !

!GIFReadWriter methodsFor: 'private-packing'!
peekByte
	bufStream atEnd
		ifTrue:
			[self atEnd ifTrue: [^nil].
			self fillBuffer].
	^bufStream peek! !

!GIFReadWriter methodsFor: 'private-decoding' stamp: 'mir 11/19/2003 12:19'!
readBitData
	"using modified Lempel-Ziv Welch algorithm."

	| outCodes outCount bitMask initCodeSize code curCode oldCode inCode finChar i bytes f c packedBits hasLocalColor localColorSize maxOutCodes |

	maxOutCodes := 4096.
	offset := self readWord@self readWord. "Image Left@Image Top"
	width := self readWord.
	height := self readWord.

	"---
	Local Color Table Flag        1 Bit
	Interlace Flag                1 Bit
	Sort Flag                     1 Bit
	Reserved                      2 Bits
	Size of Local Color Table     3 Bits
	----"
	packedBits := self next.
	interlace := (packedBits bitAnd: 16r40) ~= 0.
	hasLocalColor := (packedBits bitAnd: 16r80) ~= 0.
	localColorSize := 1 bitShift: ((packedBits bitAnd: 16r7) + 1).
	hasLocalColor ifTrue: [localColorTable := self readColorTable: localColorSize].

	pass := 0.
	xpos := 0.
	ypos := 0.
	rowByteSize := ((width + 3) // 4) * 4.
	remainBitCount := 0.
	bufByte := 0.
	bufStream := ReadStream on: ByteArray new.

	outCodes := ByteArray new: maxOutCodes + 1.
	outCount := 0.
	bitMask := (1 bitShift: bitsPerPixel) - 1.
	prefixTable := Array new: 4096.
	suffixTable := Array new: 4096.

	initCodeSize := self next.

	self setParameters: initCodeSize.
	bitsPerPixel > 8 ifTrue: [^self error: 'never heard of a GIF that deep'].
	bytes := ByteArray new: rowByteSize * height.
	[(code := self readCode) = eoiCode] whileFalse:
		[code = clearCode
			ifTrue:
				[self setParameters: initCodeSize.
				curCode := oldCode := code := self readCode.
				finChar := curCode bitAnd: bitMask.
				"Horrible hack to avoid running off the end of the bitmap.  Seems to cure problem reading some gifs!!? tk 6/24/97 20:16"
				xpos = 0 ifTrue: [
						ypos < height ifTrue: [
							bytes at: (ypos * rowByteSize) + xpos + 1 put: finChar]]
					ifFalse: [bytes at: (ypos * rowByteSize) + xpos + 1 put: finChar].
				self updatePixelPosition]
			ifFalse:
				[curCode := inCode := code.
				curCode >= freeCode ifTrue:
					[curCode := oldCode.
					outCodes at: (outCount := outCount + 1) put: finChar].
				[curCode > bitMask] whileTrue:
					[outCount > maxOutCodes
						ifTrue: [^self error: 'corrupt GIF file (OutCount)'].
					outCodes at: (outCount := outCount + 1)
						put: (suffixTable at: curCode + 1).
					curCode := prefixTable at: curCode + 1].
				finChar := curCode bitAnd: bitMask.
				outCodes at: (outCount := outCount + 1) put: finChar.
				i := outCount.
				[i > 0] whileTrue:
					["self writePixel: (outCodes at: i) to: bits"
					bytes at: (ypos * rowByteSize) + xpos + 1 put: (outCodes at: i).
					self updatePixelPosition.
					i := i - 1].
				outCount := 0.
				prefixTable at: freeCode + 1 put: oldCode.
				suffixTable at: freeCode + 1 put: finChar.
				oldCode := inCode.
				freeCode := freeCode + 1.
				self checkCodeSize]].
	prefixTable := suffixTable := nil.

	f := ColorForm extent: width@height depth: 8.
	f bits copyFromByteArray: bytes.
	"Squeak can handle depths 1, 2, 4, and 8"
	bitsPerPixel > 4 ifTrue: [^ f].
	"reduce depth to save space"
	c := ColorForm extent: width@height
		depth: (bitsPerPixel = 3 ifTrue: [4] ifFalse: [bitsPerPixel]).
	f displayOn: c.
	^ c
! !

!GIFReadWriter methodsFor: 'private-decoding' stamp: 'KLC 1/25/2004 14:04'!
readBody
	"Read the GIF blocks. Modified to return a form.  "

	| form extype block blocksize packedFields delay1 |
	form := nil.
	[stream atEnd] whileFalse: [
		block := self next.
		block = Terminator ifTrue: [^ form].
		block = ImageSeparator ifTrue: [
			form isNil
				ifTrue: [form := self readBitData]
				ifFalse: [self skipBitData].
		] ifFalse: [
			block = Extension
				ifFalse: [^ form "^ self error: 'Unknown block type'"].
			"Extension block"
			extype := self next.	"extension type"
			extype = 16rF9 ifTrue: [  "graphics control"
				self next = 4 ifFalse: [^ form "^ self error: 'corrupt GIF file'"].
				"====
				Reserved                      3 Bits
				Disposal Method               3 Bits
				User Input Flag               1 Bit
				Transparent Color Flag        1 Bit
				==="
 
				packedFields := self next.
				delay1 := self next.	"delay time 1"
				delay := (self next*256 + delay1) *10.	 "delay time 2"
				transparentIndex := self next.
				(packedFields bitAnd: 1) = 0 ifTrue: [transparentIndex := nil].
				self next = 0 ifFalse: [^ form "^ self error: 'corrupt GIF file'"].
			] ifFalse: [
				"Skip blocks"
				[(blocksize := self next) > 0]
					whileTrue: [
						"Read the block and ignore it and eat the block terminator"
						self next: blocksize]]]]! !

!GIFReadWriter methodsFor: 'private-decoding'!
readCode
	^self nextBits! !

!GIFReadWriter methodsFor: 'private-decoding' stamp: 'RAA 4/25/2001 08:48'!
readColorTable: numberOfEntries

	| array r g b |

	array := Array new: numberOfEntries.
	1 to: array size do: [ :i |
		r := self next.  
		g := self next.  
		b := self next.
		array at: i put: (Color r: r g: g b: b range: 255)
	].
	^array! !

!GIFReadWriter methodsFor: 'private-decoding' stamp: 'RAA 4/25/2001 08:49'!
readHeader
	| is89 byte hasColorMap |
	(self hasMagicNumber: 'GIF87a' asByteArray)
		ifTrue: [is89 := false]
		ifFalse: [(self hasMagicNumber: 'GIF89a' asByteArray)
			ifTrue: [is89 := true]
			ifFalse: [^ self error: 'This does not appear to be a GIF file']].
	self readWord.	"skip Screen Width"
	self readWord.	"skip Screen Height"
	byte := self next.
	hasColorMap := (byte bitAnd: 16r80) ~= 0.
	bitsPerPixel := (byte bitAnd: 7) + 1.
	byte := self next.	"skip background color."
	self next ~= 0
		ifTrue: [is89
			ifFalse: [^self error: 'corrupt GIF file (screen descriptor)']].
	hasColorMap
		ifTrue:
			[colorPalette := self readColorTable: (1 bitShift: bitsPerPixel)]
		ifFalse:
			["Transcript cr; show: 'GIF file does not have a color map.'."
			colorPalette := nil "Palette monochromeDefault"].! !

!GIFReadWriter methodsFor: 'private-encoding' stamp: 'tk 9/14/97 16:25'!
readPixelFrom: bits
	"Since bits is a Bitmap with 32 bit values, watch out for the
padding at the end of each row.  But, GIF format already wants padding to
32 bit boundary!!  OK as is.  tk 9/14/97"

	| pixel |
	ypos >= height ifTrue: [^nil].
	pixel := bits byteAt: (ypos * rowByteSize + xpos + 1).
	self updatePixelPosition.
	^pixel! !

!GIFReadWriter methodsFor: 'private-decoding'!
readWord
	^self next + (self next bitShift: 8)! !

!GIFReadWriter methodsFor: 'private'!
setParameters: initCodeSize
	clearCode := 1 bitShift: initCodeSize.
	eoiCode := clearCode + 1.
	freeCode := clearCode + 2.
	codeSize := initCodeSize + 1.
	maxCode := (1 bitShift: codeSize) - 1! !

!GIFReadWriter methodsFor: 'accessing' stamp: '6/18/97 13:18 '!
setStream: aStream
	"Feed it in from an existing source"
	stream := aStream! !

!GIFReadWriter methodsFor: 'private-decoding'!
skipBitData
	| misc blocksize |
	self readWord.  "skip Image Left"
	self readWord.  "skip Image Top"
	self readWord.  "width"
	self readWord.  "height"
	misc := self next.
	(misc bitAnd: 16r80) = 0 ifFalse: [ "skip colormap"
		1 to: (1 bitShift: (misc bitAnd: 7) + 1) do: [:i |
			self next; next; next]].
	self next.  "minimum code size"
	[(blocksize := self next) > 0]
		whileTrue: [self next: blocksize]! !

!GIFReadWriter methodsFor: 'accessing' stamp: 'di 9/15/1998 09:53'!
understandsImageFormat
	^('abc' collect: [:x | stream next asCharacter]) = 'GIF'! !

!GIFReadWriter methodsFor: 'private'!
updatePixelPosition
	(xpos := xpos + 1) >= width ifFalse: [^self].
	xpos := 0.
	interlace
		ifFalse: [ypos := ypos + 1. ^self].
	pass = 0 ifTrue:
		[(ypos := ypos + 8) >= height
			ifTrue:
				[pass := pass + 1.
				ypos := 4].
		^self].
	pass = 1 ifTrue:
		[(ypos := ypos + 8) >= height
			ifTrue:
				[pass := pass + 1.
				ypos := 2].
		^self].
	pass = 2 ifTrue:
		[(ypos := ypos + 4) >= height
			ifTrue:
				[pass := pass + 1.
				ypos := 1].
		^self].
	pass = 3 ifTrue:
		[ypos := ypos + 2.
		^self].

	^self error: 'can''t happen'! !

!GIFReadWriter methodsFor: 'private-encoding' stamp: 'bf 5/29/2003 01:21'!
writeBitData: bits
	"using modified Lempel-Ziv Welch algorithm."

	| maxBits maxMaxCode tSize initCodeSize ent tShift fCode pixel index disp nomatch |
	pass := 0.
	xpos := 0.
	ypos := 0.
	rowByteSize := width * 8 + 31 // 32 * 4.
	remainBitCount := 0.
	bufByte := 0.
	bufStream := WriteStream on: (ByteArray new: 256).

	maxBits := 12.
	maxMaxCode := 1 bitShift: maxBits.
	tSize := 5003.
	prefixTable := Array new: tSize.
	suffixTable := Array new: tSize.

	initCodeSize := bitsPerPixel <= 1 ifTrue: [2] ifFalse: [bitsPerPixel].
	self nextPut: initCodeSize.
	self setParameters: initCodeSize.

	tShift := 0.
	fCode := tSize.
	[fCode < 65536] whileTrue:
		[tShift := tShift + 1.
		fCode := fCode * 2].
	tShift := 8 - tShift.
	1 to: tSize do: [:i | suffixTable at: i put: -1].

	self writeCodeAndCheckCodeSize: clearCode.
	ent := self readPixelFrom: bits.
	[(pixel := self readPixelFrom: bits) == nil] whileFalse:
		[
		fCode := (pixel bitShift: maxBits) + ent.
		index := ((pixel bitShift: tShift) bitXor: ent) + 1.
		(suffixTable at: index) = fCode
			ifTrue: [ent := prefixTable at: index]
			ifFalse:
				[nomatch := true.
				(suffixTable at: index) >= 0
					ifTrue:
						[disp := tSize - index + 1.
						index = 1 ifTrue: [disp := 1].
						"probe"
						[(index := index - disp) < 1 ifTrue: [index := index + tSize].
						(suffixTable at: index) = fCode
							ifTrue:
								[ent := prefixTable at: index.
								nomatch := false.
								"continue whileFalse:"].
						nomatch and: [(suffixTable at: index) > 0]]
							whileTrue: ["probe"]].
				"nomatch"
				nomatch ifTrue:
					[self writeCodeAndCheckCodeSize: ent.
					ent := pixel.
					freeCode < maxMaxCode
						ifTrue:
							[prefixTable at: index put: freeCode.
							suffixTable at: index put: fCode.
							freeCode := freeCode + 1]
						ifFalse:
							[self writeCodeAndCheckCodeSize: clearCode.
							1 to: tSize do: [:i | suffixTable at: i put: -1].
							self setParameters: initCodeSize]]]].
	prefixTable := suffixTable := nil.
	self writeCodeAndCheckCodeSize: ent.
	self writeCodeAndCheckCodeSize: eoiCode.
	self flushCode.

	self nextPut: 0.	"zero-length packet"
! !

!GIFReadWriter methodsFor: 'private-encoding'!
writeCode: aCode
	self nextBitsPut: aCode! !

!GIFReadWriter methodsFor: 'private-encoding'!
writeCodeAndCheckCodeSize: aCode
	self writeCode: aCode.
	self checkCodeSize! !

!GIFReadWriter methodsFor: 'private-encoding' stamp: 'bf 5/29/2003 01:38'!
writeHeader

	| byte |
	stream position = 0 ifTrue: [
		"For first image only"
		self nextPutAll: 'GIF89a' asByteArray.
		self writeWord: width.	"Screen Width"
		self writeWord: height.	"Screen Height"
		byte := 16r80.  "has color map"
		byte := byte bitOr: ((bitsPerPixel - 1) bitShift: 5).  "color resolution"
		byte := byte bitOr: bitsPerPixel - 1.  "bits per pixel"
		self nextPut: byte.
		self nextPut: 0.		"background color."
		self nextPut: 0.		"reserved"
		colorPalette do: [:pixelValue |
			self	nextPut: ((pixelValue bitShift: -16) bitAnd: 255);
				nextPut: ((pixelValue bitShift: -8) bitAnd: 255);
				nextPut: (pixelValue bitAnd: 255)].
		loopCount notNil ifTrue: [
			"Write a Netscape loop chunk"
			self nextPut: Extension.
			self nextPutAll: #(255 11 78 69 84 83 67 65 80 69 50 46 48 3 1) asByteArray.
			self writeWord: loopCount.
			self nextPut: 0]].

	delay notNil | transparentIndex notNil ifTrue: [
		self nextPut: Extension;
			nextPutAll: #(16rF9 4) asByteArray;
			nextPut: (transparentIndex isNil ifTrue: [0] ifFalse: [9]);
			writeWord: (delay isNil ifTrue: [0] ifFalse: [delay]);
			nextPut: (transparentIndex isNil ifTrue: [0] ifFalse: [transparentIndex]);
			nextPut: 0].

	self nextPut: ImageSeparator.
	self writeWord: 0.		"Image Left"
	self writeWord: 0.		"Image Top"
	self writeWord: width.	"Image Width"
	self writeWord: height.	"Image Height"
	byte := interlace ifTrue: [16r40] ifFalse: [0].
	self nextPut: byte.
! !

!GIFReadWriter methodsFor: 'private-encoding'!
writeWord: aWord
	self nextPut: (aWord bitAnd: 255).
	self nextPut: ((aWord bitShift: -8) bitAnd: 255).
	^aWord! !

!GIFReadWriter class methodsFor: 'examples' stamp: 'bf 5/29/2003 01:56'!
exampleAnim
	"GIFReadWriter exampleAnim"

	| writer extent center |
	writer := GIFReadWriter on: (FileStream newFileNamed: 'anim.gif').
	writer loopCount: 20.		"Repeat 20 times"
	writer delay: 10.		"Wait 10/100 seconds"
	extent := 42@42.
	center := extent / 2.
	Cursor write showWhile: [
		[2 to: center x - 1 by: 2 do: [:r |
			"Make a fancy anim without using Canvas - inefficient as hell"
			| image |
			image := ColorForm extent: extent depth: 8.
			0.0 to: 359.0 do: [:theta | image colorAt: (center + (Point r: r degrees: theta)) rounded put: Color red].
			writer nextPutImage: image]
		]	ensure: [writer close]].! !

!GIFReadWriter class methodsFor: 'examples' stamp: 'nk 7/30/2004 21:40'!
grabScreenAndSaveOnDisk
	"GIFReaderWriter grabScreenAndSaveOnDisk"

	| form fileName |
	form := Form fromUser.
	form bits size = 0 ifTrue: [^Beeper beep].
	fileName := FileDirectory default nextNameFor: 'Squeak' extension: 'gif'.
	Utilities informUser: 'Writing ' , fileName
		during: [GIFReadWriter putForm: form onFileNamed: fileName]! !

!GIFReadWriter class methodsFor: 'class initialization'!
initialize
	"GIFReadWriter initialize"

	ImageSeparator := $, asInteger.
	Extension := $!! asInteger.
	Terminator := $; asInteger.
! !

!GIFReadWriter class methodsFor: 'image reading/writing' stamp: 'asm 12/11/2003 21:29'!
typicalFileExtensions
	"Answer a collection of file extensions (lowercase) which files that I can 
	read might commonly have"

	self
		allSubclasses detect: [:cls | cls wantsToHandleGIFs ]
					 ifNone: ["if none of my subclasses wants , then i''ll have to do"
							^ #('gif' )].
	^ #( )! !

!GIFReadWriter class methodsFor: 'image reading/writing' stamp: 'asm 12/11/2003 21:29'!
wantsToHandleGIFs
	^ false! !

!TimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:13'!
asTimeStamp
	"Answer the receiver as an instance of TimeStamp."

	^ self! !

!TimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:14'!
date
	"Answer the date of the receiver."

	^ self asDate! !

!TimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:17'!
dateAndTime
	"Answer a two element Array containing the receiver's date and time."

	^ Array with: self date with: self time! !

!TimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:19'!
minusDays: anInteger
	"Answer a TimeStamp which is anInteger days before the receiver."

	^ self - (anInteger days)! !

!TimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:19'!
minusSeconds: anInteger
	"Answer a TimeStamp which is anInteger number of seconds before the receiver."

	^ self - (anInteger seconds)! !

!TimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:18'!
plusDays: anInteger
	"Answer a TimeStamp which is anInteger days after the receiver."

	^ self + (anInteger days)! !

!TimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:19'!
plusSeconds: anInteger
	"Answer a TimeStamp which is anInteger number of seconds after the receiver."

	^ self + (anInteger seconds)! !

!TimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:17'!
printOn: aStream 
	"Print receiver's date and time on aStream."

	aStream 
		nextPutAll: self date printString;
		space;
		nextPutAll: self time printString.! !

!TimeStamp methodsFor: 'squeak protocol'!
species

	^ DateAndTime! !

!TimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:17'!
storeOn: aStream 

	aStream 
		print: self printString;
		nextPutAll: ' asTimeStamp'! !

!TimeStamp methodsFor: 'squeak protocol' stamp: 'brp 7/27/2003 17:15'!
time
	"Answer the time of the receiver."

	^ self asTime! !

!TimeStamp class methodsFor: 'squeak protocol' stamp: 'fbs 4/20/2004 14:21'!
current

	| ts ticks |
	ts := super now.
	
	ticks := ts ticks.
	ticks at: 3 put: 0.
	ts ticks: ticks offset: ts offset.
	
	^ ts
		
! !

!TimeStamp class methodsFor: 'instance creation' stamp: 'PeterHugossonMiller 9/2/2009 16:03'!
fromMethodTimeStamp: aString
	| stream |
	(stream := aString readStream)
		skipSeparators;
		skipTo: Character space.
	^self readFrom: stream.! !

!TimeStamp class methodsFor: 'ansi protocol' stamp: 'fbs 4/20/2004 14:22'!
now
	"Answer the current date and time as a TimeStamp."

	^self current! !

!TimeStamp class methodsFor: 'instance creation' stamp: 'stephaneducasse 2/4/2006 20:47'!
readFrom: stream
	| date time |
	stream skipSeparators.
	date := Date readFrom: stream.
	stream skipSeparators.
	time := Time readFrom: stream.
	^self 
		date: date
		time: time! !

!ScaledDecimal methodsFor: 'arithmetic' stamp: 'nice 5/17/2009 00:20'!
* aNumber
	aNumber class = self class ifTrue: [^self asFraction * aNumber asFraction asScaledDecimal: (scale max: aNumber scale)].
	^self coerce: self asFraction * aNumber! !

!ScaledDecimal methodsFor: 'arithmetic' stamp: 'nice 5/17/2009 00:21'!
+ aNumber
	aNumber class = self class ifTrue: [^self asFraction + aNumber asFraction asScaledDecimal: (scale max: aNumber scale)].
	^self coerce: self asFraction + aNumber! !

!ScaledDecimal methodsFor: 'arithmetic' stamp: 'nice 5/17/2009 00:21'!
- aNumber
	aNumber class = self class ifTrue: [^self asFraction - aNumber asFraction asScaledDecimal: (scale max: aNumber scale)].
	^self coerce: self asFraction - aNumber! !

!ScaledDecimal methodsFor: 'arithmetic' stamp: 'nice 5/17/2009 00:21'!
/ aNumber
	aNumber class = self class ifTrue: [^self asFraction / aNumber asFraction asScaledDecimal: (scale max: aNumber scale)].
	^self coerce: self asFraction / aNumber! !

!ScaledDecimal methodsFor: 'comparing' stamp: 'nice 5/17/2009 00:25'!
< aNumber
	aNumber class = self class ifTrue: [^self asFraction < aNumber asFraction].
	^self asFraction < aNumber! !

!ScaledDecimal methodsFor: 'comparing' stamp: 'nice 9/25/2009 23:03'!
<= aNumber
	aNumber class = self class ifTrue: [^self asFraction <= aNumber asFraction].
	^self asFraction <= aNumber! !

!ScaledDecimal methodsFor: 'comparing' stamp: 'nice 5/17/2009 00:25'!
= aNumber
	aNumber class = self class ifTrue: [^self asFraction = aNumber asFraction].
	^self asFraction = aNumber! !

!ScaledDecimal methodsFor: 'comparing' stamp: 'nice 9/25/2009 23:04'!
> aNumber
	aNumber class = self class ifTrue: [^self asFraction > aNumber asFraction].
	^self asFraction > aNumber! !

!ScaledDecimal methodsFor: 'comparing' stamp: 'nice 9/25/2009 23:04'!
>= aNumber
	aNumber class = self class ifTrue: [^self asFraction >= aNumber asFraction].
	^self asFraction >= aNumber! !

!ScaledDecimal methodsFor: 'converting' stamp: 'nice 5/16/2009 23:53'!
adaptToFraction: rcvr andSend: selector
	"If I am involved in arithmetic with a Fraction, convert it to a ScaledDecimal."

	^(rcvr asScaledDecimal: scale) perform: selector with: self! !

!ScaledDecimal methodsFor: 'converting' stamp: 'nice 5/16/2009 23:42'!
adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with an Integer, convert it to a ScaledDecimal."

	^(rcvr asScaledDecimal: scale) perform: selector with: self! !

!ScaledDecimal methodsFor: 'converting' stamp: 'nice 5/17/2009 00:19'!
asFraction	
	"Convert the receiver to a Fraction.
	Avoid using numerator / denominator to save a useless and costly gcd: computation"

	^denominator = 1
		ifTrue: [numerator]
		ifFalse: [Fraction numerator: numerator denominator: denominator]! !

!ScaledDecimal methodsFor: 'private' stamp: 'nice 5/17/2009 00:21'!
coerce: aNumber
	"Note: this quick hack could be replaced by double dispatching"
	
	aNumber class = self class ifTrue: [^self class newFromNumber: aNumber scale: (scale max: aNumber scale)].
	(aNumber isFraction or: [aNumber isInteger]) ifTrue: [^self class newFromNumber: aNumber scale: scale].
	^aNumber! !

!ScaledDecimal methodsFor: 'testing' stamp: 'nice 5/17/2009 00:01'!
isFraction
	"Though kind of Fraction, pretend we are not a Fraction to let coercion works correctly"
	
	^false! !

!ScaledDecimal methodsFor: 'testing' stamp: 'nice 5/16/2009 21:11'!
isLiteral
	"Answer if this number could be a well behaved literal.
	Well, it would only if evaluating back to self.
	This is not the case of all ScaledDecimals.
	Some have an infinite precision and would need an infinite number of digits to print literally.
	Try for example (3.00s2 reciprocal)."
	
	^denominator = 1 "first test trivial case before engaging arithmetic"
		or: ["Exactly we should test:
				(numerator * (10 raisedTo; scale)) \\ denominator = 0.
				But since we can assume fraction is reduced already this will be simply:"
			(10 raisedTo: scale) \\ denominator = 0]! !

!ScaledDecimal methodsFor: 'accessing' stamp: 'StephaneDucasse 10/15/2011 20:37'!
isSelfEvaluating
    "Not all scaled decimal are self evaluating, because they print rounded digits."
    ^self isLiteral! !

!ScaledDecimal methodsFor: 'comparing' stamp: 'nice 8/28/2008 19:18'!
literalEqual: other
	"Testing equality is not enough.
	It is also necessary to test number of decimal places (scale).
	Otherwise we cannot compile both literals 0.5s1 and 0.50s2 in the same method"
	
	^(super literalEqual: other) and: [self scale = other scale]! !

!ScaledDecimal methodsFor: 'arithmetic' stamp: 'nice 5/16/2009 21:01'!
negated
	^self class newFromNumber: super negated scale: scale! !

!ScaledDecimal methodsFor: 'mathematical functions' stamp: 'nice 10/14/2011 22:21'!
nthRoot: anInteger
	"Answer the nth root of the receiver.
	Preserve receiver class and scale if answer is exact.
	Otherwise, answer a Float to denote inexactness."
	| nthRoot |
	nthRoot := self asFraction nthRoot: anInteger.
	^nthRoot isFloat
		ifTrue: [nthRoot]
		ifFalse: [nthRoot asScaledDecimal: scale]! !

!ScaledDecimal methodsFor: 'printing' stamp: 'nice 3/29/2011 22:39'!
printOn: aStream
	"Append an approximated representation of the receiver on aStream.
	Use prescribed number of digits after decimal point (the scale) using a rounding operation if not exact"
	
	self printOn: aStream showingDecimalPlaces: scale.

	"Append a scale specification so that the number can be recognized as a ScaledDecimal"
	aStream nextPut: $s; print: scale.! !

!ScaledDecimal methodsFor: 'printing' stamp: 'nice 5/16/2009 21:31'!
printOn: aStream base: base
	base = 10 ifFalse: [self error: 'ScaledDecimals should be printed only in base 10'].
	^self printOn: aStream! !

!ScaledDecimal methodsFor: 'mathematical functions' stamp: 'nice 5/16/2009 21:06'!
raisedTo: aNumber
	^self coerce: (super raisedTo: aNumber)! !

!ScaledDecimal methodsFor: 'mathematical functions' stamp: 'nice 5/16/2009 21:15'!
raisedToInteger: aNumber
	^self class newFromNumber: (super raisedToInteger: aNumber) scale: scale! !

!ScaledDecimal methodsFor: 'arithmetic' stamp: 'nice 5/16/2009 21:01'!
reciprocal
	^self class newFromNumber: super reciprocal scale: scale! !

!ScaledDecimal methodsFor: 'accessing' stamp: 'nice 5/16/2009 20:56'!
scale
	^scale! !

!ScaledDecimal methodsFor: 'private' stamp: 'nice 5/16/2009 20:54'!
setNumerator: n denominator: d scale: s

	self setNumerator: n denominator: d.
	scale := s! !

!ScaledDecimal methodsFor: 'mathematical functions' stamp: 'nice 10/14/2011 22:02'!
sqrt
	"Answer the square root of the receiver.
	Preserve receiver class and scale if answer is exact.
	Otherwise, answer a Float to denote inexactness."
	| squareRoot |
	squareRoot := self asFraction sqrt.
	^squareRoot isFloat
		ifTrue: [squareRoot]
		ifFalse: [squareRoot asScaledDecimal: scale]! !

!ScaledDecimal methodsFor: 'arithmetic' stamp: 'nice 5/16/2009 21:01'!
squared
	^self class newFromNumber: super squared scale: scale! !

!ScaledDecimal methodsFor: 'printing' stamp: 'BernardoContreras 11/6/2011 17:02'!
storeOn: aStream 
	"ScaledDecimal sometimes have more digits than they print (potentially an infinity).
	In this case, do not use printOn: because it would loose some extra digits"
	
	self shouldBePrintedAsLiteral
		ifTrue: [self printOn: aStream]
		ifFalse: [aStream
			nextPut: $(;
		 	store: numerator;
			nextPut: $/;
			store: denominator;
			nextPut: $s;
			store: scale;
			nextPut: $)]! !

!ScaledDecimal class methodsFor: 'instance creation' stamp: 'nice 5/16/2009 22:23'!
newFromNumber: aNumber scale: anInteger
	| aFraction |
	aFraction := aNumber asFraction.
	^aFraction isFraction
		ifTrue: [self new setNumerator: aFraction numerator denominator: aFraction denominator scale: anInteger]
		ifFalse: [self new setNumerator: aFraction denominator: 1 scale: anInteger]! !

!ScaledDecimal class methodsFor: 'instance creation' stamp: 'nice 5/16/2009 22:36'!
readFrom: stringOrStream 
	"Answer a decimal number as described on stringOrStream.
	The number may not include a leading radix specification, as in 16rFADE,
	nor an exponent like 1.0e-3
	It might have a scale specification at end or not like 10.3s2
	If not, number of digits after decimal point will be used as scale"
	
	^(SqNumberParser on: stringOrStream) nextScaledDecimal! !

!Password methodsFor: 'accessing' stamp: 'tk 1/3/98 21:36'!
cache: anObject
	cache := anObject! !

!Password methodsFor: 'as yet unclassified' stamp: 'tk 1/5/98 21:08'!
decode: string
	"Xor with secret number -- just so file won't have raw password in it"
	| kk rand |
	rand := Random new seed: 234237.
	kk := (ByteArray new: string size) collect: [:bb | (rand next * 255) asInteger].
	1 to: kk size do: [:ii |
		kk at: ii put: ((kk at: ii) bitXor: (string at: ii) asciiValue)].
	^ kk asString! !

!Password methodsFor: 'accessing' stamp: 'gsa 1/6/2013 18:39'!
passwordFor: serverDir

"gsa 6/1/2013 Replaced UIManager for FillInTheBlankMorph. We should add #requestPassword to this morph"

	"Returned the password from one of many sources.  OK if send in a nil arg."

	| sp msg |
	cache ifNotNil: [^ cache].
	sequence ifNotNil: [
		(sp := self serverPasswords) ifNotNil: [
			sequence <= sp size ifTrue: [^ sp at: sequence]]].
	msg := serverDir isRemoteDirectory
		ifTrue: [serverDir moniker]
		ifFalse: ['this directory'].
	(serverDir user = 'anonymous') & (serverDir typeWithDefault == #ftp) ifTrue: [
			"^ cache := UIManager default request: 'Please let this anonymous ftp\server know your email address.\This is the polite thing to do.' withCRs
			initialAnswer: 'yourName@company.com'"
			
			^ cache := FillInTheBlankMorph request: 'Please let this anonymous ftp\server know your email address.\This is the polite thing to do.' withCRs
			initialAnswer: 'yourName@company.com' 
			
			].

	"^ cache := UIManager default requestPassword: 'Password for ', serverDir user, ' at ', msg, ':'."
	
	^ cache := FillInTheBlankMorph request: 'Password for ', serverDir user, ' at ', msg, ':'.
	
		"Diff between empty string and abort?"! !

!Password methodsFor: 'accessing' stamp: 'mir 6/29/2001 01:01'!
sequence
	^sequence! !

!Password methodsFor: 'accessing' stamp: 'tk 1/5/98 21:14'!
sequence: anNumber
	sequence := anNumber! !

!Password methodsFor: 'as yet unclassified' stamp: 'HenrikSperreJohansen 6/12/2010 02:37'!
serverPasswords
	"Get the server passwords off the disk and decode them. The file 'sqk.info' must be in some folder that Squeak thinks is special (vm folder, or default directory).  (Note: This code works even if you are running with no system sources file.)"

	| sfile |
	(sfile := FileDirectory lookInUsualPlaces: 'sqk.info') ifNil: [^ nil].
		"If not there, Caller will ask user for password"
		"If you don't have this file, and you really do want to release an update, 
		 contact Ted Kaehler."
	^ (self decode: sfile contentsOfEntireFile) lines
! !

!Password class methodsFor: 'as yet unclassified' stamp: 'tk 6/24/1999 11:36'!
shutDown
	"Forget all cached passwords, so they won't stay in the image"

	self allSubInstancesDo: [:each | each cache: nil].! !

!NullStream methodsFor: 'accessing' stamp: 'ar 2/25/2010 04:18'!
ascii
	"Switches the stream to ascii mode"

	binary := false.! !

!NullStream methodsFor: 'testing' stamp: 'ar 2/25/2010 04:19'!
atEnd
	"Answer whether the receiver can access any more objects."

	^false! !

!NullStream methodsFor: 'accessing' stamp: 'ar 2/25/2010 04:18'!
binary
	"Switches the stream to binary mode"

	binary := true! !

!NullStream methodsFor: 'accessing' stamp: 'gsa 1/2/2013 13:38'!
collectionSpecies
	"The type of collection returned by the stream"

	" TO-DO GSA 2/1/2013 As ByteString doesn't exist in Cuis, used String"
	"^binary ifTrue:[ByteArray] ifFalse:[ByteString]"
	^binary ifTrue:[ByteArray] ifFalse:[String]! !

!NullStream methodsFor: 'accessing' stamp: 'ar 2/25/2010 03:35'!
contents
	"Answer all of the contents of the receiver."

	self shouldNotImplement! !

!NullStream methodsFor: 'accessing' stamp: 'ar 2/25/2010 04:18'!
element
	"The element returned by the stream"

	^binary ifTrue:[0] ifFalse:[Character value: 0]! !

!NullStream methodsFor: 'initialize' stamp: 'ar 2/25/2010 14:45'!
initialize
	"Initialize the receiver"

	binary := false.
	position := 0.! !

!NullStream methodsFor: 'testing' stamp: 'ar 2/25/2010 04:19'!
isBinary
	"Return true if the receiver is a binary byte stream"

	^binary! !

!NullStream methodsFor: 'testing' stamp: 'ar 2/25/2010 04:19'!
isEmpty
	"Answer whether the receiver's contents has no elements."

	^false
! !

!NullStream methodsFor: 'reading' stamp: 'ar 2/25/2010 14:48'!
next
	"Answer the next object accessible by the receiver."

	position := position +1.
	^self element! !

!NullStream methodsFor: 'reading' stamp: 'ar 2/25/2010 14:48'!
next: anInteger 
	"Answer the next anInteger elements of my collection. Must override 
	because default uses self contents species, which might involve a large 
	collection."

	position := position +anInteger.
	^self collectionSpecies new: anInteger! !

!NullStream methodsFor: 'reading' stamp: 'ar 2/25/2010 04:19'!
next: n into: aCollection
	"Read n objects into the given collection.
	Return aCollection or a partial copy if less than
	n elements have been read."

	^self next: n into: aCollection startingAt: 1! !

!NullStream methodsFor: 'reading' stamp: 'ar 2/25/2010 14:48'!
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."

	position := position +n.
	^aCollection! !

!NullStream methodsFor: 'writing' stamp: 'ar 2/25/2010 04:19'!
next: anInteger putAll: aCollection
	"Store the next anInteger elements from the given collection."

	^self next: anInteger putAll: aCollection startingAt: 1! !

!NullStream methodsFor: 'writing' stamp: 'ar 2/25/2010 14:48'!
next: anInteger putAll: aCollection startingAt: startIndex
	"Store the next anInteger elements from the given collection."

	position := position + anInteger.
	^aCollection! !

!NullStream methodsFor: 'reading' stamp: 'ar 2/25/2010 04:19'!
nextInto: aCollection
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."

	^self next: aCollection size into: aCollection startingAt: 1.! !

!NullStream methodsFor: 'reading' stamp: 'ar 2/25/2010 04:19'!
nextInto: aCollection startingAt: startIndex
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."

	^self next: (aCollection size - startIndex+1) into: aCollection startingAt: startIndex.! !

!NullStream methodsFor: 'writing' stamp: 'ar 2/25/2010 14:48'!
nextPut: anObject 
	"Insert the argument, anObject, as the next object accessible by the 
	receiver. Answer anObject."

	position := position +1.
	^anObject! !

!NullStream methodsFor: 'writing' stamp: 'ar 2/25/2010 14:48'!
nextPutAll: aCollection 
	"Append the elements of aCollection to the sequence of objects accessible 
	by the receiver. Answer aCollection."

	position := position + aCollection size.
	^aCollection! !

!NullStream methodsFor: 'writing' stamp: 'ar 2/25/2010 04:20'!
peek
	"Answer what would be returned if the message next were sent to the 
	receiver. If the receiver is at the end, answer nil."

	^self element! !

!NullStream methodsFor: 'positioning' stamp: 'ar 2/25/2010 14:47'!
position
	"Answer the current position of accessing the sequence of objects."

	^position! !

!NullStream methodsFor: 'positioning' stamp: 'ar 2/25/2010 14:47'!
position: anInteger 
	"Set the current position for accessing the objects to be anInteger, as long 
	as anInteger is within the bounds of the receiver's contents. If it is not, 
	create an error notification."

	(anInteger >= 0)
		ifTrue: [position := anInteger]
		ifFalse: [self positionError]! !

!NullStream methodsFor: 'reading' stamp: 'nice 3/16/2010 23:04'!
readInto: aCollection startingAt: startIndex count: n
	"Read n objects into the given collection. 
	Return number of elements that have been read."

	position := position + n.
	^n! !

!NullStream methodsFor: 'positioning' stamp: 'ar 2/25/2010 14:47'!
reset
	"Set the receiver's position to the beginning of the sequence of objects."

	position := 0! !

!NullStream methodsFor: 'positioning' stamp: 'ar 2/25/2010 14:47'!
skip: anInteger 
	"Set the receiver's position to be the current position+anInteger. A 
	subclass might choose to be more helpful and select the minimum of the 
	receiver's size and position+anInteger, or the maximum of 1 and 
	position+anInteger for the repositioning."

	self position: position + anInteger! !

!NullStream class methodsFor: 'instance creation' stamp: 'ar 2/25/2010 04:20'!
new
	"Creates a new instance"

	^self basicNew initialize! !

!TimeStampTest methodsFor: 'Coverage' stamp: 'brp 7/27/2003 13:50'!
classToBeTested

	^ self timestampClass! !

!TimeStampTest methodsFor: 'Coverage' stamp: 'brp 1/30/2005 09:13'!
selectorsToBeIgnored

	| deprecated private special |

	deprecated := #().
	private := #( #printOn: ).
	special := #().

	^ super selectorsToBeIgnored, deprecated, private, special.! !

!TimeStampTest methodsFor: 'Running' stamp: 'brp 1/21/2004 18:41'!
setUp

	timestamp := self timestampClass date: ('1-10-2000' asDate) time: ('11:55:00 am' asTime).

	aTimeStamp := TimeStamp readFrom: '1-02-2004 12:34:56 am' readStream! !

!TimeStampTest methodsFor: 'Running' stamp: 'brp 7/26/2003 21:53'!
tearDown

	timestamp := nil.! !

!TimeStampTest methodsFor: 'Tests' stamp: 'StephaneDucasse 10/6/2010 20:31'!
testAccessing

	| d t |
	d := '1-10-2000' asDate.
	t := '11:55:00 am' asTime.

	self
		assert: timestamp date asString = d asString;
		assert: timestamp time asString = t asString.
! !

!TimeStampTest methodsFor: 'Tests' stamp: 'brp 7/26/2003 22:05'!
testArithmetic

	| ts |
	ts := timestamp minusDays: 123.  	"9 September 1999, 11:55 am"
	ts := ts minusSeconds: 1056.			"9 September 1999, 11:37:24 am"
	ts := ts plusDays: 123.				"10 January 2000, 11:37:24 am"
	ts := ts plusSeconds: 1056.			"10 January 2000, 11:55 am"
	self
		assert: ts  = timestamp.

	! !

!TimeStampTest methodsFor: 'Tests' stamp: 'brp 1/9/2004 06:34'!
testArithmeticAcrossDateBoundary

	| ts |
	ts := timestamp minusSeconds: ((11*3600) + (55*60) + 1).
	self
		assert: ts = ('1-9-2000 11:59:59 pm' asTimeStamp).

	! !

!TimeStampTest methodsFor: 'Tests' stamp: 'brp 3/12/2004 15:54'!
testComparing

	| ts1 ts2 ts3 c1 c2 le |
	ts1 := self timestampClass date: ('01-10-2000' asDate) time: ('11:55:00 am' asTime).
	ts2 := self timestampClass date: ('07-26-2003' asDate) time: ('22:09:45 am' asTime).
	ts3 := self timestampClass date: ('05-28-1972' asDate) time: ('04:31:14 pm' asTime).

	self
		assert: ts1 = timestamp;
		assert: ts1 hash = timestamp hash;
		assert: timestamp = timestamp copy;
		assert: ts1 < ts2;
		deny: ts1 < ts3.

	c1 := self timestampClass current.
	c2 := self timestampClass current.
	le := (c1 <= c2).
	self assert: le.

! !

!TimeStampTest methodsFor: 'Tests' stamp: 'StephaneDucasse 10/6/2010 20:33'!
testConverting

	| d t |
	d := '1-10-2000' asDate.
	t := '11:55:00 am' asTime.

	self
		assert: timestamp asSeconds = (d asSeconds + t asSeconds);
		assert: timestamp asDate asString = d asString;
		assert: timestamp asTime = t;
		assert: timestamp asTimeStamp == timestamp;
		assert: timestamp dateAndTime first asString =  d asString;
		assert: timestamp dateAndTime second asString =  t asString.
! !

!TimeStampTest methodsFor: 'testing' stamp: 'StephaneDucasse 10/6/2010 20:33'!
testDate
	self assert: aTimeStamp date asString = '01-02-2004' asDate asString! !

!TimeStampTest methodsFor: 'testing' stamp: 'StephaneDucasse 10/6/2010 20:34'!
testDateAndTime
	self assert: aTimeStamp dateAndTime first asString =  '01-02-2004' asDate asString.
	self assert: aTimeStamp dateAndTime second asString =   '00:34:56' asTime asString! !

!TimeStampTest methodsFor: 'Tests' stamp: 'brp 7/27/2003 13:55'!
testFromSeconds

	self
		assert: (self timestampClass fromSeconds: 3124958100) = timestamp.! !

!TimeStampTest methodsFor: 'Tests' stamp: 'brp 8/23/2003 15:02'!
testFromString
	"This should signal an exception in 3.6beta as Time>>fromString: does not exist."

	self should: [ timestamp = (self timestampClass fromString: '1-10-2000 11:55:00 am') ] 

! !

!TimeStampTest methodsFor: 'Tests' stamp: 'brp 1/30/2005 09:12'!
testInstanceCreation

	self 
		should: [ self timestampClass midnight asDuration = (0 hours) ];
		should: [ self timestampClass noon asDuration = (12 hours) ].
! !

!TimeStampTest methodsFor: 'testing' stamp: 'StephaneDucasse 10/6/2010 20:35'!
testMinusDays
	self assert: (aTimeStamp minusDays: 5) dateAndTime first asString = 
			'12-28-2003' asDate asString.
	self assert: (aTimeStamp minusDays: 5) dateAndTime second asString = 
		 '00:34:56' asTime asString! !

!TimeStampTest methodsFor: 'testing' stamp: 'StephaneDucasse 10/6/2010 20:37'!
testMinusSeconds

	| dAndt |
	dAndt := (aTimeStamp minusSeconds: 34 * 60 + 56) dateAndTime.
	self assert: dAndt first asString =  '01-02-2004' asDate asString.
	self assert: dAndt second asString =  '00:00:00' asTime asString! !

!TimeStampTest methodsFor: 'testing' stamp: 'StephaneDucasse 10/6/2010 20:39'!
testMinusSecondsOverMidnight
	
	| dAndt |
	dAndt := (aTimeStamp minusSeconds: 34 * 60 + 57) dateAndTime.
	self assert: dAndt first asString =  '01-01-2004' asDate asString.
	self assert: dAndt second asString =  '23:59:59' asTime asString
	"Bug The results are actual results are: #(1 January 2005 11:25:03 pm)"! !

!TimeStampTest methodsFor: 'testing' stamp: 'StephaneDucasse 10/6/2010 20:40'!
testPlusDays
	
	| dAndt |
	dAndt := (aTimeStamp plusDays: 366) dateAndTime.
	self assert: dAndt first asString =  '01-02-2005' asDate asString.
	self assert: dAndt second asString =  '00:34:56' asTime asString		
	! !

!TimeStampTest methodsFor: 'testing' stamp: 'StephaneDucasse 10/6/2010 20:41'!
testPlusSeconds
			
	| dAndt |
	dAndt := (aTimeStamp plusSeconds: 60 * 60) dateAndTime.
	self assert: dAndt first asString =  '01-02-2004' asDate asString.
	self assert: dAndt second asString =  '01:34:56' asTime asString! !

!TimeStampTest methodsFor: 'testing' stamp: 'StephaneDucasse 10/6/2010 20:44'!
testPlusSecondsOverMidnight
			
	| dAndt |
	dAndt := (aTimeStamp plusSeconds: 24 * 60 * 60 + 1) dateAndTime.
	self assert: dAndt first asString =  '01-03-2004' asDate asString.
	self assert: dAndt second asString =  '00:34:57' asTime asString! !

!TimeStampTest methodsFor: 'testing' stamp: 'damiencassou 5/30/2008 11:09'!
testPrintOn
	| cs rw |
	cs := '2 January 2004 12:34:56 am' readStream.
	rw := ReadWriteStream on: ''.
	aTimeStamp printOn: rw.
	self assert: rw contents = cs contents! !

!TimeStampTest methodsFor: 'Tests' stamp: 'brp 8/23/2003 15:02'!
testPrinting

	self	
		assert: timestamp printString = '10 January 2000 11:55 am'.
! !

!TimeStampTest methodsFor: 'testing' stamp: 'cbc 2/4/2004 21:18'!
testReadFromA1
	|ts|
	ts := TimeStamp current.
 	self assert: (ts = (TimeStamp fromString: ts asString)).! !

!TimeStampTest methodsFor: 'Tests' stamp: 'brp 8/23/2003 17:47'!
testSorting

	| c1 c2 |
	c1 := self timestampClass current.
	c2 := self timestampClass current.

	self
		assert: (self timestampClass current) <= (self timestampClass current);
		assert: (c1 <= c2).


! !

!TimeStampTest methodsFor: 'testing' stamp: 'damiencassou 5/30/2008 11:09'!
testStoreOn
	| cs rw |
	cs := '''2 January 2004 12:34:56 am'' asTimeStamp' readStream.
	rw := ReadWriteStream on: ''.
	aTimeStamp storeOn: rw.
	self assert: rw contents = cs contents! !

!TimeStampTest methodsFor: 'testing' stamp: 'brp 1/21/2004 18:41'!
testTime
	self assert: aTimeStamp time =  '00:34:56' asTime! !

!TimeStampTest methodsFor: 'testing' stamp: 'brp 1/21/2004 18:41'!
testTimeStamp
	self assert: aTimeStamp = aTimeStamp asTimeStamp
! !

!TimeStampTest methodsFor: 'Private' stamp: 'brp 7/27/2003 13:50'!
timestampClass

	^ TimeStamp! !
GIFReadWriter initialize!
