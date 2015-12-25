'===============================================================================
' BARIX Barionet BCL Applications (C)2010 BARIX, Written by DH
'===============================================================================

' ------------------------------------------------------------------------------
' INIT: Version handling
' ------------------------------------------------------------------------------

 DIM VERS$(15)
 VERS$="01.23 20101129"   ' Version of Main Application
 SYSLOG "BARIX Barionet BCL Application Digital I/O & Serial Tunnel Version "+VERS$, 1

' ------------------------------------------------------------------------------
' INFO: STP (setup) memory usage
' ------------------------------------------------------------------------------
' offset   parameter
' ------------------------------------------------------------------------------
' 018..019 RS-232 Local port (W)
' 512..513 I/O Tunnel UDP port (W)
' 514..517 Remote IP address (4B)
' 518..525 Output actions (8B)
' 526      Connection loss action (B)
' 527..529 Resend interval (W)
' 532..533 RS-232 Tunnel TCP port (W)
' 534..537 Remote IP address (4B)
' 538      Serial Tunnel reconnect interval (B)

' ------------------------------------------------------------------------------
' INFO: File handle usage
' ------------------------------------------------------------------------------
' handle	type	application
' ------------------------------------------------------------------------------
'   0     TCP   #2 Serial Tunnel
'   1     STP   Read SETUP
'   1     UDP   #1 Digital I/O Tunnel UDP LISTENER
'   2     UDP   #1 Digital I/O Tunnel UDP SEND TO IP
'   3     RS232 #2 Serial Tunnel
'   4     -     not used

' ------------------------------------------------------------------------------
' INFO: Syslog Debug Level usage
' ------------------------------------------------------------------------------
'     level display
' ------------------------------------------------------------------------------
' 0 System messages
' 1 + Version info
' 2 + Setup data info
' 3 + Application # enabled info
' 4 + Application start info
' 5 + Application detail 1 info
' 6 + Timer Gosub/Return info
' 7 + Application detail 2 info
' 8 + Application detail 3 info
' 9 + Application detailed data info

' ------------------------------------------------------------------------------
' INIT: Declarations
' ------------------------------------------------------------------------------

 DIM set$(256)    ' String FOR SETUP content
 DIM ip1$(16)     ' String for IP address
 DIM ios(8)     	' 8 inputs to watch
 DIM _M_rs$(4097)		' 4K buffer for serial tunnel

' ------/------------------------------------------------------------------------
' INIT: Get setup variables
' ------------------------------------------------------------------------------
 
 COM$= "STP:18"
 OPEN COM$ as 1
 READ 1, set$
 Close 1
 lport= MIDGET(set$,  1, 2)  ' 18 RS-232 Local port

 COM$= "STP:512"
 OPEN COM$ as 1
 READ 1, set$
 Close 1
 port1= MIDGET(set$,  1, 2)  ' 512 I/O Tunnel UDP port
 ipa1 = MIDGET(set$,  3, 1)  ' 514 I/O Tunnel remote IP address byte 1
 ipb1 = MIDGET(set$,  4, 1)  ' 515 I/O Tunnel remote IP address byte 2
 ipc1 = MIDGET(set$,  5, 1)  ' 516 I/O Tunnel remote IP address byte 3
 ipd1 = MIDGET(set$,  6, 1)  ' 517 I/O Tunnel remote IP address byte 4
 ip1$ = STR$(ipa1)+"."+STR$(ipb1)+"."+STR$(ipc1)+"."+STR$(ipd1)
 sint1= MIDGET(set$, 16, 2)  ' 548 I/O Tunnel status resend interval
 SYSLOG "Setup READ OK", 2
 SYSLOG "I/O Tunnel    "+ip1$+":"+STR$(port1), 2
 SYSLOG "Send interval "+STR$(sint1), 2
 FOR i=1 TO 8
  SYSLOG "Input "+STR$(i)+" Action "+STR$(MIDGET(set$, 6 + i, 1)), 2
 NEXT i
 SYSLOG "Connection loss action "+STR$(MIDGET(set$, 15, 1)), 2

 port2= MIDGET(set$, 21, 2)		' 532 Serial Tunnel remote TCP port
 ipa2	= MIDGET(set$, 23, 1)		' 534 Serial Tunnel remote IP address byte 1
 ipb2	= MIDGET(set$, 24, 1)		' 535 Serial Tunnel remote IP address byte 2
 ipc2	= MIDGET(set$, 25, 1)		' 536 Serial Tunnel remote IP address byte 3
 ipd2	= MIDGET(set$, 26, 1)		' 537 Serial Tunnel remote IP address byte 4
 ip2$	= STR$(ipa2)+"."+STR$(ipb2)+"."+STR$(ipc2)+"."+STR$(ipd2)
 reci = MIDGET(set$, 27, 1)		' 538 Serial Tunnel reconnect interval
 IF reci<1 THEN reci=1				' minimum interval 1 sec

	SYSLOG "Setup READ OK", 2
	SYSLOG "Serial Tunnel to "+ip2$+":"+STR$(port2), 2

' ------------------------------------------------------------------------------
' INIT: Application enabling
' ------------------------------------------------------------------------------
 IF AND(ipa1>0, port1>0) THEN			' Check IF application 1's IP(A) and port are not zero 
  apen1 = 1 t1 = 1					' Enable app 1 and TIMER 1 
  COM$ = "UDP:0.0.0.0:"+STR$(port1)	' UDP listening port for data reception
  OPEN COM$ as 1
  COM$ = "UDP:0.0.0.0:"+STR$(port1)	' UDP listening port for data sending
  OPEN COM$ as 2
  COM$ = "COM:19200,E,8,1,485:2"					' RS-485 interface for relay extension
  OPEN COM$ as 4
  FOR i = 1 TO 8					' Preset I/O states with 99
   ios(i) = 99 
  NEXT i
  tov = 1							' Preset to invoke conn. loss at start up
  trcv = 9999						' Preset receive interval to invoke conn. loss at start up
  SYSLOG "Digital I/O Tunnel enabled", 3
 ELSE
  SYSLOG "Digital I/O Tunnel disabled", 3
 ENDIF

 IF lport>0 THEN				' if RS-232 Local port is set then Server enabled
 	SYSLOG "#2 Serial Tunnel Server enabled", 3
 ELSE
 	SYSLOG "#2 Serial Tunnel Server disabled", 3
	IF AND(ipa2>0, port2>0) THEN			' Check IF application 2's IP(A) and port are not zero 
		apen2=1 t1=1				' Enable app 2 and TIMER 1
		COM$ = "COM::1"			' Open RS-232 interface FOR Serial Tunnel
								' parameters are ignored, so can leave them out
		OPEN COM$ as 3
		recon = 1										' preset reconnect flag to open TCP connection immediately
		ct = reci										' preset reconnect interval counter to open TCP connection immediately
		SYSLOG "#2 Serial Tunnel Client enabled", 3
	ELSE
		SYSLOG "#2 Serial Tunnel Client disabled", 3
	ENDIF
 ENDIF 
 IF t1 THEN
 	ON TIMER1 GOSUB 9000				' no timer function at the beginning, don't know interval
  TIMER 1, 1000					' TIMER 1 set to 1000 msec = 1 sec
		SYSLOG "1sec Timer enabled", 3  
 ENDIF
 
'===============================================================================
' MAIN: Main program loop
'===============================================================================

100	IF apen1 THEN GOSUB 1000			' Call APP1 when enabled
	IF apen2 THEN
		IF recon THEN GOSUB 2900
		GOSUB 2000			' Call APP2 when enabled
	ENDIF
	'SYSLOG "Main program looped", 9
	GOTO 100      ' Loop main program

'===============================================================================
' SUB: Digital I/O Tunnel
'===============================================================================

1000 SYSLOG "APP1: Digital I/O Tunnel", 9 
 IF Lastlen(1) < 0 THEN				' Receive
  IF tov = 1 THEN					' was in alarm condition
   SYSLOG "APP1: Reset CLA",5
   tov = 0
   ioa = 9 iov = 0 GOSUB 1100
  ENDIF

  READ 1, s$, 0					' got message
  rint1 = VAL(MID$(s$,1,5))			' got send interval form other station
  p$ = ""
  SYSLOG "APP1: Received data : " + s$, 7
  
  FOR i = 1 TO 8    				' set I/O
           ioa = i
           iov = Val(mid$(s$, 6 + i, 1))
           GOSUB 1100
  NEXT i
  trcv = 0						' reset timeout
 ELSE							' no message, Connection loss handling
  IF rint1 > 0 THEN					' check only if remote station has sent his send interval
     IF trcv > rint1 * 2 + 1 THEN		' wait remote send interval *2 plus 1 sec. for connection loss check
    trcv = 0
    ioa = 9 iov = 1 GOSUB 1100		' activate connection loss relays
    tov = 1						' tov indicates connection loss condition
   ENDIF
  ENDIF
 ENDIF

 flag = 0							' Send I/O data
 IF sint1 > 0 THEN 
	 IF tsnd >= sint1 THEN flag = 1 tsnd = 0
 ENDIF
 p$ = SPRINTF$("%05u,", sint1)
 FOR i = 1 to 8
  IF ios(i) <> IOSTATE(200 + i) THEN ios(i) = IOSTATE(200 + i) flag = 1
  p$ = p$ + SPRINTF$("%u", ios(i))
 NEXT i
 IF flag = 1 THEN					' need to send data
  SYSLOG "Send: " + p$, 8
  WRITE 2, p$, 0, ip1$, port1
 ENDIF
 RETURN

' ------------------------------------------------------------------------------
' SUB: Set I/O input is ioa 1...11 (1..4 relay, 9 alert, 10..15 exp. R6 relay)
' ------------------------------------------------------------------------------
1100 ioa = MIDGET(set$, 6 + ioa, 1)
 IF ioa = 0 THEN RETURN
 IF and(ioa > 9, ioa < 101) THEN
  SYSLOG "Relay expansion " + SPRINTF$("%u", ioa - 10), 8
  ioa = ioa - 10
  GOSUB 1200
 ELSE
          ioctl ioa, iov
 ENDIF
 RETURN          					' use table in setup

' ------------------------------------------------------------------------------
' SUB: Send data to relay unit ioa, iov
' ------------------------------------------------------------------------------
1200 
  midset s$,1,1,0				' address of module (broadcast)
  midset s$,2,1,5				' Set coil
  midset s$,3,1,0
  Midset s$,4,1,ioa				' coil address
  If iov<>0 Then sr=255 Else sr=0
  Midset s$,5,1,sr				' hi value
  midset s$,6,1,0				' lo val
  tlen=6 
  GoSub 20000
  Write 4,s$,tlen
  DELAY 3
  RETURN
  
'===============================================================================
' APP2: Serial Tunnel
'===============================================================================
DIM rmt_connected
2000	'SYSLOG "APP2: Serial Tunnel", 4
	IF CONNECTED(0) THEN			' tunnel data in both directions when connected
        IF rmt_connected = 0 THEN
            syslog "APP2: Conneted to remote:"+rmthost$(0) + ":" + str$(rmtport(0))
        	rmt_connected = 1
        ENDIF

		READ 0, _M_rs$				' READ from TCP

		IF LASTLEN(0) > 0 THEN 
            WRITE 3, _M_rs$, LASTLEN(0)
         	SYSLOG "APP2: received TCP data bytes: "+STR$(LEN(_M_rs$)), 4
        ENDIF

        READ 3, _M_rs$				' READ from RS-232
		IF LASTLEN(3) > 0 THEN 
            WRITE 0, _M_rs$, LASTLEN(3)
         	SYSLOG "APP2: received RS-232 data bytes: "+STR$(LEN(_M_rs$)), 4
        ENDIF
	ELSE
       if rmt_connected then 
		 SYSLOG "APP2: RS-232 Serial Tunnel lost TCP connection"
         rmt_connected = 0
       endif
	ENDIF
	RETURN 

' ------------------------------------------------------------------------------
' APP2SUB: Open TCP
' ------------------------------------------------------------------------------
2900	IF CONNECTED(0) THEN recon=0 : RETURN 		' Open active TCP IF not connected < called by TIMER1 subroutine
	IF ct >= reci THEN				' Wait "reconnect interval" seconds before reopening TCP connection
		'Example: COM$ = "TCP:192.168.0.200:10001"
		COM$ = "TCP:"+ip2$+":"+STR$(port2)
		SYSLOG "Open remote connection: "+COM$,3
		IF MEDIATYPE(0) THEN CLOSE(0)
		OPEN COM$ as 0
		IF CONNECTED(0) THEN 
			SYSLOG "Remote connection established",3
		ELSE
			SYSLOG "Remote connection NOT established",3
		ENDIF
		ct = 0
	ELSE 
		ct = ct + 1 
		SYSLOG "APP2: TCP reconnect wait cycle "+STR$(ct),3
	ENDIF
	recon=0
	RETURN
	

'===============================================================================
' TIMER1 subroutine
'===============================================================================
9000 SYSLOG "TIMER1 Gosub", 6
 trcv = trcv + 1
 tsnd = tsnd + 1
 recon = 1
 SYSLOG "TIMER1 Return", 6
 RETURN

20000 ' *************** CRC calculation, string s$, don't touch *****************************
      ' tlen must be #chars, crc is included after them in the string
      	
	temp2=&HFFFF				' calculate modbus CRC
	For temp1=1 to tlen
	  temp2=xor(temp2,midget(s$,temp1,1))
	  If and(temp2,1) Then temp2=xor(temp2,&H14002)
	  If and(temp2,2) Then temp2=xor(temp2,&H28004)
	  If and(temp2,4) Then temp2=xor(temp2,&H50008)
	  If and(temp2,8) Then temp2=xor(temp2,&HA0010)
	  If and(temp2,16) Then temp2=xor(temp2,&H140020)
	  If and(temp2,32) Then temp2=xor(temp2,&H280040)
	  If and(temp2,64) Then temp2=xor(temp2,&H500080)
	  If and(temp2,128) Then temp2=xor(temp2,&HA00100)
	  temp2=shr(temp2,8)
	Next temp1	
	midset s$,tlen+1,2,temp2			' store crc lowbyte first
	tlen=tlen+2
	Return

End
' EOF
