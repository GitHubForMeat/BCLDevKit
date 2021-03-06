CoB160�   barionet.bas��0  barionet.tok��>  barionetbcl.cob
���  errors.hlpD w README
}� README.txteL8 uiappsetup1.html!�g uihappsetup1.html��x VERSION '===============================================================================
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
' 6 + Timer GOSUB/RETURN info
' 7 + Application detail 2 info
' 8 + Application detail 3 info
' 9 + Application detailed data info

' ------------------------------------------------------------------------------
' INIT: Declarations
' ------------------------------------------------------------------------------

 DIM set$(256)    	' String FOR SETUP content
 DIM ip1$(16)     	' String for IP address
 DIM ios(8)     	' 8 inputs to watch
 DIM _M_rs$(4097)	' 4K buffer for serial tunnel

' ------/------------------------------------------------------------------------
' INIT: Get setup variables
' ------------------------------------------------------------------------------
 
 COM$= "STP:18"
 OPEN COM$ AS 1
 READ 1, set$
 CLOSE 1
 lport= MIDGET(set$,  1, 2)  ' 18 RS-232 Local port

 COM$= "STP:512"
 OPEN COM$ AS 1
 READ 1, set$
 CLOSE 1
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

 port2= MIDGET(set$, 21, 2)			' 532 Serial Tunnel remote TCP port
 ipa2	= MIDGET(set$, 23, 1)		' 534 Serial Tunnel remote IP address byte 1
 ipb2	= MIDGET(set$, 24, 1)		' 535 Serial Tunnel remote IP address byte 2
 ipc2	= MIDGET(set$, 25, 1)		' 536 Serial Tunnel remote IP address byte 3
 ipd2	= MIDGET(set$, 26, 1)		' 537 Serial Tunnel remote IP address byte 4
 ip2$	= STR$(ipa2)+"."+STR$(ipb2)+"."+STR$(ipc2)+"."+STR$(ipd2)
 reci = MIDGET(set$, 27, 1)			' 538 Serial Tunnel reconnect interval
 IF reci<1 THEN reci=1				' minimum interval 1 sec

	SYSLOG "Setup READ OK", 2
	SYSLOG "Serial Tunnel to "+ip2$+":"+STR$(port2), 2

' ------------------------------------------------------------------------------
' INIT: Application enabling
' ------------------------------------------------------------------------------
 IF AND(ipa1>0, port1>0) THEN		' Check IF application 1's IP(A) and port are not zero 
  apen1 = 1 t1 = 1					' Enable app 1 and TIMER 1 
  COM$ = "UDP:0.0.0.0:"+STR$(port1)	' UDP listening port for data reception
  OPEN COM$ AS 1
  COM$ = "UDP:0.0.0.0:"+STR$(port1)	' UDP listening port for data sending
  OPEN COM$ AS 2
  COM$ = "COM:19200,E,8,1,485:2"					' RS-485 interface for relay extension
  OPEN COM$ AS 4
  FOR i = 1 TO 8					' Preset I/O states with 99
	ios(i) = 99 
  NEXT i
  tov = 1							' Preset to invoke conn. loss at start up
  trcv = 9999						' Preset receive interval to invoke conn. loss at start up
  SYSLOG "Digital I/O Tunnel enabled", 3
 ELSE
  SYSLOG "Digital I/O Tunnel disabled", 3
 ENDIF

 IF lport>0 THEN					' If RS-232 Local port is set then Server enabled
 	SYSLOG "#2 Serial Tunnel Server enabled", 3
 ELSE
 	SYSLOG "#2 Serial Tunnel Server disabled", 3
	IF AND(ipa2>0, port2>0) THEN	' Check IF application 2's IP(A) and port are not zero 
		apen2=1 t1=1				' Enable app 2 and TIMER 1
		COM$ = "COM::1"				' Open RS-232 interface FOR Serial Tunnel
									' parameters are ignored, so can leave them out
		OPEN COM$ AS 3
		recon = 1					' preset reconnect flag to open TCP connection immediately
		ct = reci					' preset reconnect interval counter to open TCP connection immediately
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

100	IF apen1 THEN GOSUB 1000		' Call APP1 when enabled
	IF apen2 THEN
		IF recon THEN GOSUB 2900
		GOSUB 2000					' Call APP2 when enabled
	ENDIF
	'SYSLOG "Main program looped", 9
	GOTO 100      					' Loop main program

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

  READ 1, s$, 0						' got message
  rint1 = VAL(MID$(s$,1,5))			' got send interval form other station
  p$ = ""
  SYSLOG "APP1: Received data : " + s$, 7
  
  FOR i = 1 TO 8    				' set I/O
           ioa = i
           iov = Val(mid$(s$, 6 + i, 1))
           GOSUB 1100
  NEXT i
  trcv = 0							' reset timeout
 ELSE								' no message, Connection loss handling
  IF rint1 > 0 THEN					' check only if remote station has sent his send interval
     IF trcv > rint1 * 2 + 1 THEN	' wait remote send interval *2 plus 1 sec. for connection loss check
    trcv = 0
    ioa = 9 iov = 1 GOSUB 1100		' activate connection loss relays
    tov = 1							' tov indicates connection loss condition
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
 IF AND(ioa > 9, ioa < 101) THEN
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
  MIDSET s$,1,1,0				' address of module (broadcast)
  MIDSET s$,2,1,5				' Set coil
  MIDSET s$,3,1,0
  MIDSET s$,4,1,ioa				' coil address
  IF iov<>0 THEN sr=255 ELSE sr=0
  MIDSET s$,5,1,sr				' hi value
  MIDSET s$,6,1,0				' lo val
  tlen=6 
  GOSUB 20000
  WRITE 4,s$,tlen
  DELAY 3
  RETURN
  
'===============================================================================
' APP2: Serial Tunnel
'===============================================================================
DIM rmt_connected
2000	'SYSLOG "APP2: Serial Tunnel", 4
	IF CONNECTED(0) THEN			' tunnel data in both directions when connected
        IF rmt_connected = 0 THEN
            SYSLOG "APP2: Conneted to remote:"+rmthost$(0) + ":" + STR$(rmtport(0))
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
       IF rmt_connected THEN 
		 SYSLOG "APP2: RS-232 Serial Tunnel lost TCP connection"
         rmt_connected = 0
       ENDIF
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
		OPEN COM$ AS 0
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
9000 SYSLOG "TIMER1 GOSUB", 6
 trcv = trcv + 1
 tsnd = tsnd + 1
 recon = 1
 SYSLOG "TIMER1 RETURN", 6
 RETURN

20000 ' *************** CRC calculation, string s$, don't touch *****************************
      ' tlen must be #chars, crc is included after them in the string
      	
	temp2=&HFFFF				' calculate modbus CRC
	FOR temp1=1 TO tlen
	  temp2=XOR(temp2,midget(s$,temp1,1))
	  IF AND(temp2,1) THEN temp2=XOR(temp2,&H14002)
	  IF AND(temp2,2) THEN temp2=XOR(temp2,&H28004)
	  IF AND(temp2,4) THEN temp2=XOR(temp2,&H50008)
	  IF AND(temp2,8) THEN temp2=XOR(temp2,&HA0010)
	  IF AND(temp2,16) THEN temp2=XOR(temp2,&H140020)
	  IF AND(temp2,32) THEN temp2=XOR(temp2,&H280040)
	  IF AND(temp2,64) THEN temp2=XOR(temp2,&H500080)
	  IF AND(temp2,128) THEN temp2=XOR(temp2,&HA00100)
	  temp2=SHR(temp2,8)
	NEXT temp1	
	MIDSET s$,tlen+1,2,temp2			' store crc lowbyte first
	tlen=tlen+2
	RETURN

End
' EOF
TB!��w       }     3�      X�      \�      `�      d�      h�      l�      p�      t�      x�      |�      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      �      �      �	      �      �      �#    )   # .   #3  W:   X?   �D   �G   �� 	01.23 20101129 ?BARIX Barionet BCL Application Digital I/O & Serial Tunnel Version  � �	STP:18 -�.0�/`	%��	STP:512 -�.0�/`	%�`	%�`	%�`	%�`	%��	X`. X`. X`. X``	%�?Setup READ OK ?I/O Tunnel     �: X`?Send interval  X``		?Input  X`	 Action  X%�`	`	?Connection loss action  X%�`
	%�`	%�`	%�`	%�`	%��	X`. X`. X`. X``	%�` `	?Setup READ OK ?Serial Tunnel to  �: X`
H``� `	`	�	UDP:0.0.0.0: X`-�.�	UDP:0.0.0.0: X`-�.�	COM:19200,E,8,1,485:2 -�.`		``		c`	`	`	'?Digital I/O Tunnel enabled E& ?Digital I/O Tunnel disabled F`, ?#2 Serial Tunnel Server enabled E� ?#2 Serial Tunnel Server disabled H``
T `	`	�	COM::1 -�.`	`	`?#2 Serial Tunnel Client enabled E+ ?#2 Serial Tunnel Client disabled FF`- = 
>�?1sec Timer enabled F`  ` `  � �F�?APP1: Digital I/O Tunnel 	2� `	- ?APP1: Reset CLA `	`		`	 �F0�`	)Z��	 ?APP1: Received data :  �`		`	`	`	)Z�`	 �`	`	E8 `- `` `	`		`	 �`	FFF`	` `	` `	`	F�	\%05u, ``		``	;� `	 ``		;� `	`	�	�\%u ``	`	`	# ?Send:  �1��`F!`	%�``	 !H`	`e9 ?Relay expansion  \%u `
`	`
 HE <``F!D�D�D�D�`` `	� E `	D�`D�`	 G
1�`+!3� `	7 ?APP2: Conneted to remote: ]: X5`	F0�2? 1�2?APP2: received TCP data bytes:  X#�F0�2B 1�2?APP2: received RS-232 data bytes:  X#�FED `; ?APP2: RS-232 Serial Tunnel lost TCP connection `	FF!3 `	!`	`� �	TCP: �: X`
?Open remote connection:  �4 /-�.3* ?Remote connection established E, ?Remote connection NOT established F`	E8 `	`?APP2: TCP reconnect wait cycle  X`F`	!?TIMER1 GOSUB `	``	``	?TIMER1 RETURN !`	��  ` 	``	J`%�` H` `	J`@ H` `	J`� H` `	J`  H` `	J` 
 H` `	J`   H`  `	J`@ ( H`@ `	J`� P H`�  `	J` � `	7`` D�```	`!"_TMR_ IOS LPORT PORT1 IPA1 IPB1 IPC1 IPD1 SINT1 I PORT2 IPA2 IPB2 IPC2 IPD2 RECI APEN1 T1 TOV TRCV APEN2 RECON CT IOA IOV RINT1 FLAG TSND SR TLEN RMT_CONNECTED TEMP2 TEMP1 VERS$ SET$ IP1$ _M_RS$ COM$ IP2$ S$ P$ CoB160�   barionet.bas��0  barionet.tok
��>  errors.hlpD FO  README
}�O  README.txteL^  uiappsetup1.html!l�  uihappsetup1.html���  VERSION '===============================================================================
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
TB!��w       }     3�      X�      \�      `�      d�      h�      l�      p�      t�      x�      |�      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      ��      �      �      �	      �      �      �#    )   # .   #3  W:   X?   �D   �G   �� 	01.23 20101129 ?BARIX Barionet BCL Application Digital I/O & Serial Tunnel Version  � �	STP:18 -�.0�/`	%��	STP:512 -�.0�/`	%�`	%�`	%�`	%�`	%��	X`. X`. X`. X``	%�?Setup READ OK ?I/O Tunnel     �: X`?Send interval  X``		?Input  X`	 Action  X%�`	`	?Connection loss action  X%�`
	%�`	%�`	%�`	%�`	%��	X`. X`. X`. X``	%�` `	?Setup READ OK ?Serial Tunnel to  �: X`
H``� `	`	�	UDP:0.0.0.0: X`-�.�	UDP:0.0.0.0: X`-�.�	COM:19200,E,8,1,485:2 -�.`		``		c`	`	`	'?Digital I/O Tunnel enabled E& ?Digital I/O Tunnel disabled F`, ?#2 Serial Tunnel Server enabled E� ?#2 Serial Tunnel Server disabled H``
T `	`	�	COM::1 -�.`	`	`?#2 Serial Tunnel Client enabled E+ ?#2 Serial Tunnel Client disabled FF`- = 
>�?1sec Timer enabled F`  ` `  � �F�?APP1: Digital I/O Tunnel 	2� `	- ?APP1: Reset CLA `	`		`	 �F0�`	)Z��	 ?APP1: Received data :  �`		`	`	`	)Z�`	 �`	`	E8 `- `` `	`		`	 �`	FFF`	` `	` `	`	F�	\%05u, ``		``	;� `	 ``		;� `	`	�	�\%u ``	`	`	# ?Send:  �1��`F!`	%�``	 !H`	`e9 ?Relay expansion  \%u `
`	`
 HE <``F!D�D�D�D�`` `	� E `	D�`D�`	 G
1�`+!3� `	7 ?APP2: Conneted to remote: ]: X5`	F0�2? 1�2?APP2: received TCP data bytes:  X#�F0�2B 1�2?APP2: received RS-232 data bytes:  X#�FED `; ?APP2: RS-232 Serial Tunnel lost TCP connection `	FF!3 `	!`	`� �	TCP: �: X`
?Open remote connection:  �4 /-�.3* ?Remote connection established E, ?Remote connection NOT established F`	E8 `	`?APP2: TCP reconnect wait cycle  X`F`	!?TIMER1 Gosub `	``	``	?TIMER1 Return !`	��  ` 	``	J`%�` H` `	J`@ H` `	J`� H` `	J`  H` `	J` 
 H` `	J`   H`  `	J`@ ( H`@ `	J`� P H`�  `	J` � `	7`` D�```	`!"_TMR_ IOS LPORT PORT1 IPA1 IPB1 IPC1 IPD1 SINT1 I PORT2 IPA2 IPB2 IPC2 IPD2 RECI APEN1 T1 TOV TRCV APEN2 RECON CT IOA IOV RINT1 FLAG TSND SR TLEN RMT_CONNECTED TEMP2 TEMP1 VERS$ SET$ IP1$ _M_RS$ COM$ IP2$ S$ P$ 0  BCL file not exisiting or invalid tokencode version (use correct tokenizer version)
1  PRINT was not last statement in line or wrong delimiter used (allowed ',' or ';')
2  Wrong logical operator in IF statement (allowed '=','>','>=','<','<=','<>')
3  ONLY String VARIABLE can be used as parameter in OPEN,READ,MIDxxx,EXEC
4  Wrong delimiter/parameter is used in list of parameters for this statement/function
5  ON statement must be followed by GOTO/GOSUB statement
6  First parameter of TIMER statement must be 1..4 (# for ON TIMER# GOSUB...)
7  Wrong element is used in this string/numeric expression, maybe a type mismatch
8  Divided by Zero
9  Wrong label is used in GOTO/GOSUB statement (allowed only a numeric constant)
10 Wrong symbol is used in source code, syntax error, tokenization is impossible
11 Wrong size of string/array is used in DIM (allowed only a numeric constant)
12 Wrong type in DIM statement used (only string variable or long variable/array allowed)
13 DIM was not last statement in line or wrong delimiter used (allowed only ',')
14 Missing bracket in expression or missing quote in string constant
15 Maximum nesting of calculations exceeded (too many brackets)
16 Assignment assumed (missing equal sign)
17 Wrong size of external tokenized TOK file (file might be corrupt)
18 Too many labels needed, tokenization is impossible
19 Identical labels in source code found, tokenization is impossible
20 Undefined label in GOTO/GOSUB statement found, tokenization is impossible
21 Missing THEN in IF/THEN statement
22 Missing TO in FOR/TO statement
23 Run-time warning: Possibly, maximum nesting of FOR-NEXT loops exceeded
24 NEXT statement without FOR statement or wrong index variable in NEXT statement
25 Maximum nesting of GOSUB-RETURN calls exceeded
26 RETURN statement without proper GOSUB statement
27 Lack of memory for dynamic memory allocation
28 String variable name conflict or too many string variables used
29 Long variable name conflict or too many long variables used
30 Insufficient space in memory for temp string, variable or program allocation
31 Current Array index bigger then maximal defined index in DIM statement
32 Wrong current number of file/stream handler (allowed only 0..4)
33 Wrong file/stream type/type name or file/stream is already closed
34 This file/stream handler is already used or file/stream already opened
35 Missing AS statement in OPEN AS statement
36 Wrong address in IOCTL or IOSTATE
37 Wrong serial port number in OPEN statement
38 Wrong baudrate parameter for serial port in OPEN statement
39 Wrong parity parameter for serial port in OPEN statement
40 Wrong data bits parameter for serial port in OPEN statement
41 Wrong stop bits parameter for serial port in OPEN statement
42 Wrong serial port type parameter in OPEN statement
43 Run-time warning: You lost data during PLAY -- Please, increase string size
44 For TCP/CIFS file/stream only handler with number 0..5 are allowed
45 Only standard size (256 bytes) string variable allowed for READ and WRITE in STP file
46 Wrong or out of string range parameters in MID$ or MIDxxx
47 Only one STP/F_C file can be opened at a time
48 '&' can be used ONLY at the end of a line
49 Syntax error in multiline IF...ENDIF (maybe wrong nesting)
50 Length of Search Tag must not exceed size of target String Variable for READ
51 DIM string/array variable name already used
52 Wrong user function name or array declaration missing
53 General syntax error: wrong or not allowed delimiter or statement at this position
54 Run-time warning: Lost data during UDP READ -- Please, increase string size
55 Run-time warning: Lost data during UDP receiving -- 1k buffer limit
56 Run-time warning: Impossible to allocate 6 TCP handles, if 6 are needed free up TCP command port and/or serial local ports
57 Run-time warning: Lost data during concatenation of strings -- Please, increase target string size (DIM statement)
58 Run-time warning: Lost data during assignment of string -- Please, increase target string size (DIM statement)
59 Indicated flash page (WEBx) is out of range for this HW
60 COB file (F_C type) exceeds 64k limit
61 Token size too long
62 Unrecognized token type
Combined Digital IO and Serial Tunnel Application for the Barionet
HTTP/1.0 200
Content-type: text/plain

Combined Digital IO and Serial Tunnel Application for the Barionet
Load into WEB4 of your Barionet (100) or Barionet 50.

Help


Serial Tunnel
This Application permits to tunnel RS-232 serial data from one Barionet RS-232 port to another Barionet RS-232 port.
The Barionet that will act as a client (initiating the serial tunnel) must be configured by disabling its server setting (Local port = 0) and setting the client settings according to the remote Barionet's server settings.
Serial settings like speed, databits etc. must be configured the same on both Barionets under the"SERIAL" tab on the "SETTINGS" page.

Server settings

Local port
Define the Port on which the serial gateway can be accessed using TCP.
Default: "10001" for RS-232
Enter "0" to disable the serial server and to prepare for the use as a client (see below "Client settings")

Disconnect Tout
Defines the time in seconds after which the TCP connection to the local serial port will be closed due to inactivity.
Enter "0" to disable the Disconnect Timeout (default).
Enter "1" to "255" to enable the Disconnect Timeout.

Client settings

Remote IP Address
Enter the 4 values of the remote Barionet's IP address.
To disable the Serial Tunnel enter "0.0.0.0".

Remote TCP Port
Remote Barionet's TCP listen port (see remote Barionet's server settings "Local port").
Default port is "0" (disabled).
Barix default on the remote Barionet is "10001".

Reconnect interval
Set the time interval (between 1 to 255 seconds) which will be used to try to reconnect to the remote Barionet once the connection is lost.
Default interval is "0" (0 and 1 both equal to try every second).


Digital I/O Tunnel
This Application permits to tunnel digital inputs from one Barionet to outputs of another Barionet and vice versa. Both Devices have to be set up in order to do tunneling correctly.
One or both Barionets can be extended by connecting the Barix R6 Relay Extension module to the RS-485 port. Serial settings of the second port should then be set to "RS-485,19200,8,E,1" to support a Barix R6 Relay Extension module with default settings.
This I/O Tunnel application supports both the Barionet 50 and the legacy Barionet and is backwards compatible with version B1.10. This allows for mixing hardware and software. The Barionet 50 however is supported in the I/O Tunnel version B1.20 and later.

Remote IP Address
Input the 4 values of the desired remote Barionet's IP address.
To disable the tunnel enter 0.0.0.0.

Tunneling UDP Port
UDP port number used for tunnel data communication. Use the same port on the remote Barionet. To disable the tunnel enter 0.

Send Interval
Input the time in seconds between two status messages. In case no inputs have changed (changes are sent immediately) a status message is sent to permit the other side to detect a communication loss. The same value should be set on the remote Barionet. Recommended value: 2..65535. To disable Loss checking enter 0.

The comunication loss will be triggered 1 second after the double time of the remote stations send interval to allow 1 lost UDP packet eg. when interval is 5 seconds the com. loss will be triggered 11 seconds after the last received I/O status.

Output Action
Choose an output for each remote input and the communication loss. Eight inputs are featured on a legacy Barionet, four on the Barionet 50 (input 5 to 8 are always inactive). Choose Relay 1 or 2 on a legacy Barionet and Relay 1 to 4 on a Barionet 50, Output 1 to 4 (legacy Barionet only) or Extension Relay 1 to 6 (only if a single Relay Extension module Barix R6 is attached to the RS-485 bus). HTTP/1.0 200
Content-type: text/html

<html>
  <!-- &L(0,"*"); -->
  <head>
    <script language=JavaScript src=util.js></script>
    <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
  </head>
  <body bgcolor=#FFFFFF link=#CCCCCC vlink=#CCCCCC alink="#FFFFFF"><br>
    <p><font size="3" face="Arial"><b>APPLICATION SETUP: Digital I/O &amp; Serial Tunnel Version &LBAS(1,"%fs",vers$);</b></font><br>
    <br>                                                                            
    <img src="col0.gif" width="650" height="1"><br>                                 
    <table>                                                                         
       <form action=setup.cgi method=get><input type="hidden" name=L value=uisaved.html>
       <tr>                                                                         
            <td><font size="3" face="Arial, Helvetica, sans-serif"><b>RS-232 TUNNEL</b></font></td>
       </tr>
       <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b><u>Server settings</u></b></font></td>
       </tr>
       <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Local port</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=W18 size=5 maxlength=5 value=&LSetup(1,"%u",18,W); onChange=PortCheck(this)></font></td>
       </tr>
       <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Disconnect Tout</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=B26 size=3 maxlength=3 value=&LSetup(1,"%u",26,B); onChange=ByteCheck(this)> seconds</font></td>
       </tr>
       <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b><u>Client settings</u></b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif">(disable Server first! See help.)</font></td>
       </tr>
       <tr>                                                                         
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Remote IP Address</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif">
                        <input name=B534 size=3 maxlength=3 value=&LSetup(1,"%u",534); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B535 size=3 maxlength=3 value=&LSetup(1,"%u",535); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B536 size=3 maxlength=3 value=&LSetup(1,"%u",536); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B537 size=3 maxlength=3 value=&LSetup(1,"%u",537); onChange=IPCheck(this)></font>
            </td>
        </tr>
        <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Remote TCP Port</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=W532 size=5 maxlength=5 value=&LSetup(1,"%u",532,W); onChange=PortCheck(this)></font></td>
        </tr>
       <tr>                                                                         
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Reconnect interval</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=B538 size=3 maxlength=3 value=&LSetup(1,"%u",538); onChange=IPCheck(this)> seconds</font></td>
        </tr>
        <tr>
          <td>&nbsp;</td>
        </tr>
       <tr>                                                                         
            <td><font size="3" face="Arial, Helvetica, sans-serif"><b>I/O TUNNEL</b></font></td>
        </tr>
       <tr>                                                                         
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Remote IP Address</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif">
                        <input name=B514 size=3 maxlength=3 value=&LSetup(1,"%u",514); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B515 size=3 maxlength=3 value=&LSetup(1,"%u",515); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B516 size=3 maxlength=3 value=&LSetup(1,"%u",516); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B517 size=3 maxlength=3 value=&LSetup(1,"%u",517); onChange=IPCheck(this)></font>
            </td>
        </tr>
        <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Tunneling UDP Port</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=W512 size=5 maxlength=5 value=&LSetup(1,"%u",512,W); onChange=PortCheck(this)></font></td>
        </tr>
        <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Send Interval</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=W527 size=5 maxlength=5 value=&LSetup(1,"%u",527,W); onChange=PortCheck(this)> seconds</font></td>
        </tr>
<!--        <tr>
          <td><font size="1" face="Arial, Helvetica, sans-serif">&nbsp;</font></td>
        </tr>-->
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b><u>Remote Input</u></b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b><u>Local Output Action</u></b></font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 1</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B518>
           <option value=0&LSetup(3,"%s",518,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",518,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",518,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",518,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",518,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",518,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",518,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",518,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",518,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",518,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",518,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",518,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",518,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",518,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",518,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 2</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B519>
           <option value=0&LSetup(3,"%s",519,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",519,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",519,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",519,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",519,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",519,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",519,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",519,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",519,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",519,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",519,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",519,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",519,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",519,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",519,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 3</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B520>
           <option value=0&LSetup(3,"%s",520,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",520,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",520,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",520,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",520,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",520,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",520,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",520,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",520,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",520,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",520,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",520,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",520,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",520,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",520,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 4</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B521>
           <option value=0&LSetup(3,"%s",521,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",521,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",521,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",521,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",521,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",521,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",521,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",521,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",521,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",521,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",521,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",521,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",521,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",521,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",521,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 5</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B522>
           <option value=0&LSetup(3,"%s",522,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",522,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",522,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",522,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",522,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",522,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",522,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",522,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",522,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",522,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",522,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",522,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",522,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",522,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",522,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 6</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B523>
           <option value=0&LSetup(3,"%s",523,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",523,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",523,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",523,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",523,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",523,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",523,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",523,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",523,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",523,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",523,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",523,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",523,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",523,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",523,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 7</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B524>
           <option value=0&LSetup(3,"%s",524,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",524,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",524,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",524,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",524,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",524,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",524,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",524,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",524,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",524,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",524,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",524,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",524,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",524,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",524,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 8</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B525>
           <option value=0&LSetup(3,"%s",525,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",525,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",525,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",525,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",525,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",525,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",525,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",525,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",525,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",525,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",525,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",525,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",525,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",525,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",525,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Communication loss</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B526>
           <option value=0&LSetup(3,"%s",526,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",526,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",526,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",526,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",526,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",526,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",526,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",526,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",526,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",526,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",526,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",526,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",526,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",526,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",526,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
        <tr>
          <td>&nbsp;</td>
         </tr>
         <tr>
        <td>
          <input type="submit" value=" Save "><input type="reset" value=" Cancel ">
       </td>
      </tr>
      </form>
    </table>
  </body>
</html>HTTP/1.0 200
Content-type: text/html

<html>
<body bgcolor="#CCCCCC">
<p><font face="Arial, Helvetica, sans-serif"><b>Help</b><br></font>  <font size="2" face="Arial, Helvetica, sans-serif"><br><br>
<font size="3"><b>Serial Tunnel</b></font><br>
This Application permits to tunnel RS-232 serial data from one Barionet RS-232 port to another Barionet RS-232 port.<br>
The Barionet that will act as a client (initiating the serial tunnel) must be configured by disabling its server setting (Local port = 0) and setting the client settings according to the remote Barionet's server settings.<br>
Serial settings like speed, databits etc. must be configured the same on both Barionets under the&quot;SERIAL&quot; tab on the &quot;SETTINGS&quot; page.<br>
<br>
<b><u>Server settings</u></b><br>
<br>
<b>Local port</b><br>
Define the Port on which the serial gateway can be accessed using TCP.<br>
Default: <em>&quot;10001&quot;</em> for RS-232<br>
Enter <em>&quot;0&quot;</em> to disable the serial server and to prepare for the use as a client (see below &quot;Client settings&quot;)<br>
 <br>
<b>Disconnect Tout</b><br>
Defines the time in seconds after which the TCP connection to the local serial port will be closed due to inactivity.<br>
Enter <em>&quot;0&quot;</em> to disable the Disconnect Timeout (default).</font>
<br>  
<font size="2" face="Arial">Enter <em>&quot;1&quot;</em> to <em>&quot;255&quot;</em> to enable the Disconnect Timeout.<br>
<br>
<b><u>Client settings</u></b><br>
<br>
<b>Remote IP Address</b><br>
Enter the 4 values of the  remote Barionet's IP address.<br>
To disable the Serial Tunnel enter <em>&quot;0.0.0.0&quot;</em>.<br>
<br>
<b>Remote TCP Port</b><br>
Remote Barionet's TCP listen port (see remote Barionet's server settings &quot;Local port&quot;).<br>
Default port is <em>&quot;0&quot;</em> (disabled).<br>
Barix default on the remote Barionet is <em>&quot;10001&quot;</em>.<br>
<br>
<b>Reconnect interval</b><br>
Set the time interval (between 1 to 255 seconds) which will be used to try to reconnect to the remote Barionet once the connection is lost.<br>
Default interval is <em>&quot;0&quot;</em> (0 and 1 both equal to try every second).<br><br>
<br>
<font size="3"><b>Digital I/O Tunnel</b></font><br>
This Application permits to tunnel digital inputs from one Barionet to outputs of another Barionet and vice versa. Both Devices have to be set up in order to do tunneling correctly.<br>
One or both Barionets can be extended by connecting the Barix R6 Relay Extension module to the RS-485 port. Serial settings of the second port should then be set to <em>&quot;RS-485,19200,8,E,1&quot;</em> to support a Barix R6 Relay Extension module with default settings.<br>
This I/O Tunnel application supports both the Barionet 50 and the legacy Barionet and is backwards compatible with version B1.10. This allows for mixing hardware and software. The Barionet 50 however is supported in the I/O Tunnel version B1.20 and later.<br><br>
<b>Remote IP Address</b><br>
Input the 4 values of the desired remote Barionet's IP address.<br>
To disable the tunnel enter 0.0.0.0.<br><br>
<b>Tunneling UDP Port</b><br>
UDP port number used for tunnel data communication. Use the same port on the remote Barionet. To disable the tunnel enter 0.<br>
<br>
<b>Send Interval</b><br>
Input the time in seconds between two status messages. In case no inputs have changed (changes are sent immediately) a status message is sent to permit the other side to detect a communication loss. The same value should be set on the remote Barionet. Recommended value: 2..65535. To disable Loss checking enter 0.<br>
<br>
The comunication loss will be triggered 1 second after the double time of the remote stations send interval to allow 1 lost UDP packet eg. when interval is 5 seconds the com. loss will be triggered 11 seconds after the last received I/O status.<br>
<br>
<b>Output Action</b><br>
Choose an output for each remote input and the communication loss. Eight inputs are featured on a legacy Barionet, four on the Barionet 50 (input 5 to 8 are always inactive). Choose Relay
1 or 2 on a legacy Barionet and Relay 1 to 4 on a Barionet 50, Output 1 to 4 (legacy Barionet only) or Extension Relay 1 to 6 (only if a single Relay Extension module Barix R6 is attached to the RS-485 bus). </font>

</body>
</html>
0123
/* * * * * * * * * * * * * * * * * * * * * * * * * * *
Vers  Date     Who  What
0120  06.10.09 PK - started version history
0121  15.10.09 PK - fixed bug in interval: if interval set to 0 does not send at all
0122  17.05.10 DB - merged Digital I/O Tunnel with Serial Tunnel
0123  29.11.10 PK - removed "main program looped" message, which was sent 2500x per second on debug level 9
*/
0  BCL file not exisiting or invalid tokencode version (use correct tokenizer version)
1  PRINT was not last statement in line or wrong delimiter used (allowed ',' or ';')
2  Wrong logical operator in IF statement (allowed '=','>','>=','<','<=','<>')
3  ONLY String VARIABLE can be used as parameter in OPEN,READ,MIDxxx,EXEC
4  Wrong delimiter/parameter is used in list of parameters for this statement/function
5  ON statement must be followed by GOTO/GOSUB statement
6  First parameter of TIMER statement must be 1..4 (# for ON TIMER# GOSUB...)
7  Wrong element is used in this string/numeric expression, maybe a type mismatch
8  Divided by Zero
9  Wrong label is used in GOTO/GOSUB statement (allowed only a numeric constant)
10 Wrong symbol is used in source code, syntax error, tokenization is impossible
11 Wrong size of string/array is used in DIM (allowed only a numeric constant)
12 Wrong type in DIM statement used (only string variable or long variable/array allowed)
13 DIM was not last statement in line or wrong delimiter used (allowed only ',')
14 Missing bracket in expression or missing quote in string constant
15 Maximum nesting of calculations exceeded (too many brackets)
16 Assignment assumed (missing equal sign)
17 Wrong size of external tokenized TOK file (file might be corrupt)
18 Too many labels needed, tokenization is impossible
19 Identical labels in source code found, tokenization is impossible
20 Undefined label in GOTO/GOSUB statement found, tokenization is impossible
21 Missing THEN in IF/THEN statement
22 Missing TO in FOR/TO statement
23 Run-time warning: Possibly, maximum nesting of FOR-NEXT loops exceeded
24 NEXT statement without FOR statement or wrong index variable in NEXT statement
25 Maximum nesting of GOSUB-RETURN calls exceeded
26 RETURN statement without proper GOSUB statement
27 Lack of memory for dynamic memory allocation
28 String variable name conflict or too many string variables used
29 Long variable name conflict or too many long variables used
30 Insufficient space in memory for temp string, variable or program allocation
31 Current Array index bigger then maximal defined index in DIM statement
32 Wrong current number of file/stream handler (allowed only 0..4)
33 Wrong file/stream type/type name or file/stream is already closed
34 This file/stream handler is already used or file/stream already opened
35 Missing AS statement in OPEN AS statement
36 Wrong address in IOCTL or IOSTATE
37 Wrong serial port number in OPEN statement
38 Wrong baudrate parameter for serial port in OPEN statement
39 Wrong parity parameter for serial port in OPEN statement
40 Wrong data bits parameter for serial port in OPEN statement
41 Wrong stop bits parameter for serial port in OPEN statement
42 Wrong serial port type parameter in OPEN statement
43 Run-time warning: You lost data during PLAY -- Please, increase string size
44 For TCP/CIFS file/stream only handler with number 0..5 are allowed
45 Only standard size (256 bytes) string variable allowed for READ and WRITE in STP file
46 Wrong or out of string range parameters in MID$ or MIDxxx
47 Only one STP/F_C file can be opened at a time
48 '&' can be used ONLY at the end of a line
49 Syntax error in multiline IF...ENDIF (maybe wrong nesting)
50 Length of Search Tag must not exceed size of target String Variable for READ
51 DIM string/array variable name already used
52 Wrong user function name or array declaration missing
53 General syntax error: wrong or not allowed delimiter or statement at this position
54 Run-time warning: Lost data during UDP READ -- Please, increase string size
55 Run-time warning: Lost data during UDP receiving -- 1k buffer limit
56 Run-time warning: Impossible to allocate 6 TCP handles, if 6 are needed free up TCP command port and/or serial local ports
57 Run-time warning: Lost data during concatenation of strings -- Please, increase target string size (DIM statement)
58 Run-time warning: Lost data during assignment of string -- Please, increase target string size (DIM statement)
59 Indicated flash page (WEBx) is out of range for this HW
60 COB file (F_C type) exceeds 64k limit
61 Token size too long
62 Unrecognized token type
Combined Digital IO and Serial Tunnel Application for the Barionet
HTTP/1.0 200
Content-type: text/plain

Combined Digital IO and Serial Tunnel Application for the Barionet
Load into WEB4 of your Barionet (100) or Barionet 50.

Help


Serial Tunnel
This Application permits to tunnel RS-232 serial data from one Barionet RS-232 port to another Barionet RS-232 port.
The Barionet that will act as a client (initiating the serial tunnel) must be configured by disabling its server setting (Local port = 0) and setting the client settings according to the remote Barionet's server settings.
Serial settings like speed, databits etc. must be configured the same on both Barionets under the"SERIAL" tab on the "SETTINGS" page.

Server settings

Local port
Define the Port on which the serial gateway can be accessed using TCP.
Default: "10001" for RS-232
Enter "0" to disable the serial server and to prepare for the use as a client (see below "Client settings")

Disconnect Tout
Defines the time in seconds after which the TCP connection to the local serial port will be closed due to inactivity.
Enter "0" to disable the Disconnect Timeout (default).
Enter "1" to "255" to enable the Disconnect Timeout.

Client settings

Remote IP Address
Enter the 4 values of the remote Barionet's IP address.
To disable the Serial Tunnel enter "0.0.0.0".

Remote TCP Port
Remote Barionet's TCP listen port (see remote Barionet's server settings "Local port").
Default port is "0" (disabled).
Barix default on the remote Barionet is "10001".

Reconnect interval
Set the time interval (between 1 to 255 seconds) which will be used to try to reconnect to the remote Barionet once the connection is lost.
Default interval is "0" (0 and 1 both equal to try every second).


Digital I/O Tunnel
This Application permits to tunnel digital inputs from one Barionet to outputs of another Barionet and vice versa. Both Devices have to be set up in order to do tunneling correctly.
One or both Barionets can be extended by connecting the Barix R6 Relay Extension module to the RS-485 port. Serial settings of the second port should then be set to "RS-485,19200,8,E,1" to support a Barix R6 Relay Extension module with default settings.
This I/O Tunnel application supports both the Barionet 50 and the legacy Barionet and is backwards compatible with version B1.10. This allows for mixing hardware and software. The Barionet 50 however is supported in the I/O Tunnel version B1.20 and later.

Remote IP Address
Input the 4 values of the desired remote Barionet's IP address.
To disable the tunnel enter 0.0.0.0.

Tunneling UDP Port
UDP port number used for tunnel data communication. Use the same port on the remote Barionet. To disable the tunnel enter 0.

Send Interval
Input the time in seconds between two status messages. In case no inputs have changed (changes are sent immediately) a status message is sent to permit the other side to detect a communication loss. The same value should be set on the remote Barionet. Recommended value: 2..65535. To disable Loss checking enter 0.

The comunication loss will be triggered 1 second after the double time of the remote stations send interval to allow 1 lost UDP packet eg. when interval is 5 seconds the com. loss will be triggered 11 seconds after the last received I/O status.

Output Action
Choose an output for each remote input and the communication loss. Eight inputs are featured on a legacy Barionet, four on the Barionet 50 (input 5 to 8 are always inactive). Choose Relay 1 or 2 on a legacy Barionet and Relay 1 to 4 on a Barionet 50, Output 1 to 4 (legacy Barionet only) or Extension Relay 1 to 6 (only if a single Relay Extension module Barix R6 is attached to the RS-485 bus). HTTP/1.0 200
Content-type: text/html

<html>
  <!-- &L(0,"*"); -->
  <head>
    <script language=JavaScript src=util.js></script>
    <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
  </head>
  <body bgcolor=#FFFFFF link=#CCCCCC vlink=#CCCCCC alink="#FFFFFF"><br>
    <p><font size="3" face="Arial"><b>APPLICATION SETUP: Digital I/O &amp; Serial Tunnel Version &LBAS(1,"%fs",vers$);</b></font><br>
    <br>                                                                            
    <img src="col0.gif" width="650" height="1"><br>                                 
    <table>                                                                         
       <form action=setup.cgi method=get><input type="hidden" name=L value=uisaved.html>
       <tr>                                                                         
            <td><font size="3" face="Arial, Helvetica, sans-serif"><b>RS-232 TUNNEL</b></font></td>
       </tr>
       <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b><u>Server settings</u></b></font></td>
       </tr>
       <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Local port</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=W18 size=5 maxlength=5 value=&LSetup(1,"%u",18,W); onChange=PortCheck(this)></font></td>
       </tr>
       <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Disconnect Tout</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=B26 size=3 maxlength=3 value=&LSetup(1,"%u",26,B); onChange=ByteCheck(this)> seconds</font></td>
       </tr>
       <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b><u>Client settings</u></b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif">(disable Server first! See help.)</font></td>
       </tr>
       <tr>                                                                         
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Remote IP Address</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif">
                        <input name=B534 size=3 maxlength=3 value=&LSetup(1,"%u",534); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B535 size=3 maxlength=3 value=&LSetup(1,"%u",535); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B536 size=3 maxlength=3 value=&LSetup(1,"%u",536); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B537 size=3 maxlength=3 value=&LSetup(1,"%u",537); onChange=IPCheck(this)></font>
            </td>
        </tr>
        <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Remote TCP Port</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=W532 size=5 maxlength=5 value=&LSetup(1,"%u",532,W); onChange=PortCheck(this)></font></td>
        </tr>
       <tr>                                                                         
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Reconnect interval</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=B538 size=3 maxlength=3 value=&LSetup(1,"%u",538); onChange=IPCheck(this)> seconds</font></td>
        </tr>
        <tr>
          <td>&nbsp;</td>
        </tr>
       <tr>                                                                         
            <td><font size="3" face="Arial, Helvetica, sans-serif"><b>I/O TUNNEL</b></font></td>
        </tr>
       <tr>                                                                         
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Remote IP Address</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif">
                        <input name=B514 size=3 maxlength=3 value=&LSetup(1,"%u",514); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B515 size=3 maxlength=3 value=&LSetup(1,"%u",515); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B516 size=3 maxlength=3 value=&LSetup(1,"%u",516); onChange=IPCheck(this)><b>&nbsp;.&nbsp;</b>
                        <input name=B517 size=3 maxlength=3 value=&LSetup(1,"%u",517); onChange=IPCheck(this)></font>
            </td>
        </tr>
        <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Tunneling UDP Port</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=W512 size=5 maxlength=5 value=&LSetup(1,"%u",512,W); onChange=PortCheck(this)></font></td>
        </tr>
        <tr>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Send Interval</b></font></td>
            <td><font size="2" face="Arial, Helvetica, sans-serif"><input name=W527 size=5 maxlength=5 value=&LSetup(1,"%u",527,W); onChange=PortCheck(this)> seconds</font></td>
        </tr>
<!--        <tr>
          <td><font size="1" face="Arial, Helvetica, sans-serif">&nbsp;</font></td>
        </tr>-->
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b><u>Remote Input</u></b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b><u>Local Output Action</u></b></font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 1</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B518>
           <option value=0&LSetup(3,"%s",518,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",518,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",518,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",518,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",518,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",518,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",518,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",518,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",518,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",518,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",518,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",518,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",518,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",518,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",518,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 2</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B519>
           <option value=0&LSetup(3,"%s",519,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",519,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",519,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",519,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",519,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",519,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",519,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",519,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",519,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",519,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",519,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",519,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",519,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",519,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",519,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 3</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B520>
           <option value=0&LSetup(3,"%s",520,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",520,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",520,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",520,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",520,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",520,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",520,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",520,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",520,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",520,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",520,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",520,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",520,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",520,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",520,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 4</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B521>
           <option value=0&LSetup(3,"%s",521,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",521,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",521,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",521,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",521,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",521,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",521,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",521,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",521,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",521,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",521,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",521,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",521,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",521,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",521,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 5</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B522>
           <option value=0&LSetup(3,"%s",522,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",522,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",522,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",522,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",522,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",522,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",522,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",522,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",522,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",522,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",522,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",522,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",522,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",522,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",522,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 6</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B523>
           <option value=0&LSetup(3,"%s",523,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",523,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",523,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",523,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",523,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",523,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",523,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",523,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",523,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",523,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",523,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",523,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",523,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",523,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",523,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 7</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B524>
           <option value=0&LSetup(3,"%s",524,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",524,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",524,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",524,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",524,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",524,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",524,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",524,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",524,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",524,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",524,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",524,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",524,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",524,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",524,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Input 8</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B525>
           <option value=0&LSetup(3,"%s",525,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",525,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",525,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",525,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",525,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",525,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",525,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",525,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",525,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",525,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",525,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",525,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",525,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",525,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",525,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
      <tr>
        <td><font size="2" face="Arial, Helvetica, sans-serif"><b>Communication loss</b></font></td>
        <td><font size="2" face="Arial, Helvetica, sans-serif">
         <select size=1 name=B526>
           <option value=0&LSetup(3,"%s",526,B,0," selected");>none</option>
           <option value=1&LSetup(3,"%s",526,B,1," selected");>Relay 1</option>
           <option value=2&LSetup(3,"%s",526,B,2," selected");>Relay 2</option>
           <option value=3&LSetup(3,"%s",526,B,3," selected");>Relay 3</option>
           <option value=4&LSetup(3,"%s",526,B,4," selected");>Relay 4</option>
           <option value=101&LSetup(3,"%s",526,B,101," selected");>Output 1</option>
           <option value=102&LSetup(3,"%s",526,B,102," selected");>Output 2</option>
           <option value=103&LSetup(3,"%s",526,B,103," selected");>Output 3</option>
           <option value=104&LSetup(3,"%s",526,B,104," selected");>Output 4</option>
           <option value=10&LSetup(3,"%s",526,B,10," selected");>Ext. Relay 1</option>
           <option value=11&LSetup(3,"%s",526,B,11," selected");>Ext. Relay 2</option>
           <option value=12&LSetup(3,"%s",526,B,12," selected");>Ext. Relay 3</option>
           <option value=13&LSetup(3,"%s",526,B,13," selected");>Ext. Relay 4</option>
           <option value=14&LSetup(3,"%s",526,B,14," selected");>Ext. Relay 5</option>
           <option value=15&LSetup(3,"%s",526,B,15," selected");>Ext. Relay 6</option>
         </select>
        </font></td>
      </tr>
        <tr>
          <td>&nbsp;</td>
         </tr>
         <tr>
        <td>
          <input type="submit" value=" Save "><input type="reset" value=" Cancel ">
       </td>
      </tr>
      </form>
    </table>
  </body>
</html>HTTP/1.0 200
Content-type: text/html

<html>
<body bgcolor="#CCCCCC">
<p><font face="Arial, Helvetica, sans-serif"><b>Help</b><br></font>  <font size="2" face="Arial, Helvetica, sans-serif"><br><br>
<font size="3"><b>Serial Tunnel</b></font><br>
This Application permits to tunnel RS-232 serial data from one Barionet RS-232 port to another Barionet RS-232 port.<br>
The Barionet that will act as a client (initiating the serial tunnel) must be configured by disabling its server setting (Local port = 0) and setting the client settings according to the remote Barionet's server settings.<br>
Serial settings like speed, databits etc. must be configured the same on both Barionets under the&quot;SERIAL&quot; tab on the &quot;SETTINGS&quot; page.<br>
<br>
<b><u>Server settings</u></b><br>
<br>
<b>Local port</b><br>
Define the Port on which the serial gateway can be accessed using TCP.<br>
Default: <em>&quot;10001&quot;</em> for RS-232<br>
Enter <em>&quot;0&quot;</em> to disable the serial server and to prepare for the use as a client (see below &quot;Client settings&quot;)<br>
 <br>
<b>Disconnect Tout</b><br>
Defines the time in seconds after which the TCP connection to the local serial port will be closed due to inactivity.<br>
Enter <em>&quot;0&quot;</em> to disable the Disconnect Timeout (default).</font>
<br>  
<font size="2" face="Arial">Enter <em>&quot;1&quot;</em> to <em>&quot;255&quot;</em> to enable the Disconnect Timeout.<br>
<br>
<b><u>Client settings</u></b><br>
<br>
<b>Remote IP Address</b><br>
Enter the 4 values of the  remote Barionet's IP address.<br>
To disable the Serial Tunnel enter <em>&quot;0.0.0.0&quot;</em>.<br>
<br>
<b>Remote TCP Port</b><br>
Remote Barionet's TCP listen port (see remote Barionet's server settings &quot;Local port&quot;).<br>
Default port is <em>&quot;0&quot;</em> (disabled).<br>
Barix default on the remote Barionet is <em>&quot;10001&quot;</em>.<br>
<br>
<b>Reconnect interval</b><br>
Set the time interval (between 1 to 255 seconds) which will be used to try to reconnect to the remote Barionet once the connection is lost.<br>
Default interval is <em>&quot;0&quot;</em> (0 and 1 both equal to try every second).<br><br>
<br>
<font size="3"><b>Digital I/O Tunnel</b></font><br>
This Application permits to tunnel digital inputs from one Barionet to outputs of another Barionet and vice versa. Both Devices have to be set up in order to do tunneling correctly.<br>
One or both Barionets can be extended by connecting the Barix R6 Relay Extension module to the RS-485 port. Serial settings of the second port should then be set to <em>&quot;RS-485,19200,8,E,1&quot;</em> to support a Barix R6 Relay Extension module with default settings.<br>
This I/O Tunnel application supports both the Barionet 50 and the legacy Barionet and is backwards compatible with version B1.10. This allows for mixing hardware and software. The Barionet 50 however is supported in the I/O Tunnel version B1.20 and later.<br><br>
<b>Remote IP Address</b><br>
Input the 4 values of the desired remote Barionet's IP address.<br>
To disable the tunnel enter 0.0.0.0.<br><br>
<b>Tunneling UDP Port</b><br>
UDP port number used for tunnel data communication. Use the same port on the remote Barionet. To disable the tunnel enter 0.<br>
<br>
<b>Send Interval</b><br>
Input the time in seconds between two status messages. In case no inputs have changed (changes are sent immediately) a status message is sent to permit the other side to detect a communication loss. The same value should be set on the remote Barionet. Recommended value: 2..65535. To disable Loss checking enter 0.<br>
<br>
The comunication loss will be triggered 1 second after the double time of the remote stations send interval to allow 1 lost UDP packet eg. when interval is 5 seconds the com. loss will be triggered 11 seconds after the last received I/O status.<br>
<br>
<b>Output Action</b><br>
Choose an output for each remote input and the communication loss. Eight inputs are featured on a legacy Barionet, four on the Barionet 50 (input 5 to 8 are always inactive). Choose Relay
1 or 2 on a legacy Barionet and Relay 1 to 4 on a Barionet 50, Output 1 to 4 (legacy Barionet only) or Extension Relay 1 to 6 (only if a single Relay Extension module Barix R6 is attached to the RS-485 bus). </font>

</body>
</html>
0123
/* * * * * * * * * * * * * * * * * * * * * * * * * * *
Vers  Date     Who  What
0120  06.10.09 PK - started version history
0121  15.10.09 PK - fixed bug in interval: if interval set to 0 does not send at all
0122  17.05.10 DB - merged Digital I/O Tunnel with Serial Tunnel
0123  29.11.10 PK - removed "main program looped" message, which was sent 2500x per second on debug level 9
*/
