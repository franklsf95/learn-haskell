#!/usr/bin/python
#WUJ Sysem
Q = [] #queue
head = 0
tail = -1
Step = 0

def enq( s ):
	global head, tail
	for t in Q[ :head]:
		if( s==t[0] ):
			return
	Q.append( [ s, Step ] )
	tail += 1

def deq():
	global head, tail
	t = Q[head]
	head += 1
	print t
	return t

def conv1( s ):
	'''*J -> *JU'''
	if s[ -1: ]=='J':
		s += 'U'
	enq( s )

def conv2( s ):
	'''W* -> W**'''
	ss = s[ 1: ]
	ns = 'W'+ss+ss
	enq( ns )

def conv3( s ):
	'''JJJ -> U'''
	for i in range( 1, len(s)-3 ):
		if s[i:i+3]=='JJJ':
			ns = s[ :i]+'U'+s[i+3: ]
			enq( ns )

def conv4( s ):
	'''UU -> /'''
	for i in range( 1, len(s)-2 ):
		if s[i:i+2]=='UU':
			ns = s[ :i]+s[i+2: ]
			enq( ns )

if __name__ == '__main__':
	enq( 'WJ' )
	while head <= tail:
		t = deq()
		cStr = t[0]
		cStep = t[1]
		if cStr=='WU':
			break
		if cStep==6:
			break

		Step = cStep+1
		conv1(cStr)
		conv2(cStr)
		conv3(cStr)
		conv4(cStr)



