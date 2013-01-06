'From Cuis 4.1 of 12 December 2012 [latest update: #1549] on 6 January 2013 at 5:09:34 pm'!
'Description Please enter a description for this package '!
!classDefinition: #NullStream category: #'Cuis-CompatibilityWithOtherSmalltalks'!
Stream subclass: #NullStream
	instanceVariableNames: 'binary position'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cuis-CompatibilityWithOtherSmalltalks'!
!classDefinition: 'NullStream class' category: #'Cuis-CompatibilityWithOtherSmalltalks'!
NullStream class
	instanceVariableNames: ''!


!NullStream commentStamp: 'ar 2/25/2010 14:49' prior: 0!
NullStream is a stream generating and consuming an infinite number of elements. It can be used as an equivalent of /dev/null or for performance benchmarks.!

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

!Collection methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 08:00'!
asDictionary

	^ self as: Dictionary! !

!Collection methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:34'!
contains: aBlock
	"VW compatibility"
	^self anySatisfy: aBlock! !

!Date methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 17:08'!
asSeconds
 	"Answer the seconds since the Squeak epoch: 1 January 1901"
 
 	^ start asSeconds! !

!DateAndTime methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 17:05'!
asSeconds
 	"Return the number of seconds since the Squeak epoch"
 	^ (self - (self class epoch)) asSeconds
 ! !

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

!Duration methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 16:54'!
asSeconds
 	"Answer the number of seconds in the receiver."
 	^ seconds
 ! !

!FileDirectory methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 12:48'!
pathFromUrl: aFileUrl
	
	^String streamContents: [ :s | | first |
		first := false.
		aFileUrl path do: [ :p |
			first ifTrue: [ s nextPut: self pathNameDelimiter ].
			first := true.
			s nextPutAll: p ] ].! !

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

!SequenceableCollection class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/2/2013 21:07'!
new: newSize streamContents: blockWithArg

	| stream |
	stream := WriteStream on: (self new: newSize).
	blockWithArg value: stream.
	stream position = newSize
		ifTrue: [ ^stream originalContents ]
		ifFalse: [ ^stream contents ]! !

!Set methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 09:14'!
removeAll
	self init: array size.! !

!SocketStream methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/1/2013 13:48'!
crlf
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPutAll: String newLineString! !

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

!Time methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:39'!
hours

	^ self hour! !

!Time methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:39'!
minutes

	^ self minute! !

!Time methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:40'!
seconds

	^ self second! !

!Transcript class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/1/2013 13:46'!
cr
	"WriteStream protocol.
	In the older TranscriptStream, it added a CR character.
	Now, finish the current incomplete entry."

	self finishEntry! !

!WriteStream methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/6/2013 11:48'!
cr
	"Append a return character to the receiver."

	self nextPut: Character crCharacter! !

!WriteStream methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 1/1/2013 19:32'!
crlf
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPutAll: String newLineString! !
