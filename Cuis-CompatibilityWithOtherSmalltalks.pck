'From Cuis 4.1 of 12 December 2012 [latest update: #1517] on 20 December 2012 at 7:21:25 pm'!
'Description Please enter a description for this package '!

!Character methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:36'!
sameAs: aCharacter 
	"Answer whether the receiver is equal to aCharacter, ignoring case"
	^ (self asLowercase = aCharacter asLowercase)! !

!Character class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 14:53'!
cr
	"Answer the Character representing a carriage return."

	^ self with: Character crCharacter! !

!Character class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 14:53'!
lf
	"Answer the Character representing a linefeed."

	^ self with: Character lfCharacter! !

!Collection methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 08:00'!
asDictionary

	^ self as: Dictionary! !

!Collection methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:34'!
contains: aBlock
	"VW compatibility"
	^self anySatisfy: aBlock! !

!Set methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 09:14'!
removeAll
	self init: array size.! !

!String class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:26'!
cr
	"Answer a string containing a single carriage return character."

	^ self with: Character crCharacter! !

!String class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:26'!
crlf
	"Answer a string containing a carriage return and a linefeed."

	^ self with: Character crCharacter with: Character lfCharacter
! !

!String class methodsFor: '*Cuis-CompatibilityWithOtherSmalltalks' stamp: 'gsa 12/20/2012 12:27'!
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
