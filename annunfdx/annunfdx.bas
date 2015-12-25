'------------------------------------------------------------------------------
' "annunfdx.bas", Door intercom (outside station)
'------------------------------------------------------------------------------
    DIM ver$(18)

' V00.09  11.11.06 AK    - added serial gateway function
' V00.09  12.01.07 PK+JR - buffer ballancing
' V00.09  24.01.07 PK    - send on level
'                        - serial settings for serial GW
' V00.09  14.02.07 PK    - configurable keepalive messages
' V00.09  20.02.07 PK    - simple RTP implementation, bitrate and packet size are not according to RFCs
' V00.09  21.02.07 PK    - configurable UDP/RTP mode
' V00.10  05.03.07 PK    - version string
' V00.11  05.03.07 PK    - RTP payload 20ms for 8kHz sampling frequencies
' V00.12  06.03.07 PK    - RTP payload 20ms
'                        - serial gateway send only if there's any data
'                        - Barix dynamic RTP payload types
'                        - speed optimization - 8kHz works perfectly, PCM/24 is still too slow
' V00.13  07.03.07 PK    - RTP implemented in firmware
' V00.14  10.01.08 SH/PK - implemented ABCL v0.12 IO map; button 0 (I0) now used instead of I1 to be compatible with Ann1000
' V00.15  02.01.08 PK    - txsize ignored in RTP mode
' V00.16  23.09.08 PK    - added receiving IP address
' V00.17  06.12.08 PK    - using uncompressed audio mode 11
' V00.18  12.03.09 PK    - fixed bug #057.01: broken RTP
' V00.19  16.04.09 JP    - added serial gateway functionality configurable.
'			 - implemented active TCP connection support
'                        - Relay Off delay made configurable from UI
' V00.20  16.09.09 KK    - Fixed syntax error stray : on line 232
'                        - Fixed syntax error stray : on line 246
' V00.21  03.05.10 PK    - implemented frame based buffering; delay now in ms instead of bytes
' V00.22  03.06.10 PK    - #057.08: proper defaults for Delay and Frame Duration in Annunfdx
' V00.23  16.03.11 PK    - #057.10: fixed wrong baudrate 230400
' V01.00  15.11.12 ASI   - converted to the new webUI
' V01.01  10.10.13 ASI   - fixed to display mono input and mono output peak levels on the webUI instead of 
'                          stereo input peak levels

    ver$="V1.01 10.10.2013"

    Dim _Mr$(1401)                                ' receiver audio buffer
    Dim l1k,x1k                                   ' routines for subroutine at 10000
    
    DIM _Ms$(1400)
    Dim _Mx$(1500)                                 ' for send buffer, no more than 480 bytes of audio plus header

    Dim _Mt$(100)

    Dim rf
    rf=0
    Dim ad_gain
    Dim mic_gain
    Dim rho$(32)
    dim delms						' RTP delay in milliseconds
    dim com$(50)
    dim tmout 

    'parameters for gateway
    dim gwip,gwip$,conty  'ip address of gateway
    dim gwtyp 'UDP or TCP
    DIM  _Mb$(1024),lb,lb2,gwprt
    DIM cnwai 'timout for active tcp connection

    gwip = 0
    gwtyp = 0
    tmout = 0 ' relay timeout
    ' input levels
    dim peak_in,peak_out,levth,sndtout
    peak_in=0
    peak_out=0

    'ioctl 1,0                                     ' relay off
    
    For i=1 to 50 
        midset _Mt$,2*i-1,2,&HFFFF 
    Next i     ' fill "empty" buffer with FF, this allows to fill rcv buf with val

    Open "STP:0" as 2
    read 2,set$
    Close 2

    com$="COM:"
    comb=midget(set$,82,1)
    if comb=0 then com$=com$+"38400,"
    if comb=1 then com$=com$+"19200,"
    if comb=2 then com$=com$+"9600,"
    if comb=3 then com$=com$+"4800,"
    if comb=4 then com$=com$+"2400,"
    if comb=5 then com$=com$+"1200,"
    if comb=6 then com$=com$+"600,"
    if comb=7 then com$=com$+"300,"
    if comb=8 then com$=com$+"115200,"
    if comb=9 then com$=com$+"57600,"
    if comb=12 then com$=com$+"230400,"
    if comb=11 then com$=com$+"76800,"

    comb=midget(set$,81,1)
    if and(comb,&H30)=0 then com$=com$+"N,"
    if and(comb,&H30)=&H30 then com$=com$+"E,"
    if and(comb,&H30)=&H10 then com$=com$+"O,"
    
    if and(comb,4) then com$=com$+"8," else com$=com$+"7,"

    if and(comb,128) then com$=com$+"2," else com$=com$+"1,"
    
    comb=midget(set$,83,1)
    if comb=0 then com$=com$+"NON:"
    if comb=1 then com$=com$+"SFW:"
    if comb=2 then com$=com$+"HDW:"
    if comb=8 then com$=com$+"485:"

    com$=com$+"1"   ' serial 1

    Open "STP:500" as 2
    read 2,set$
    Close 2

    vol=midget(set$,1,1)
    If vol>20 Then vol=20

    ad_gain=midget(set$,2,1)
    If ad_gain>15 Then ad_gain=15

    mic_gain=midget(set$,3,1)
    If mic_gain>15 Then mic_gain=15

    If midget(set$,11,1)=129 Then      ' input (129=line, 130=mic)
        inp=1
        stereo=0
    Else
        inp=2
        stereo=0
    endif

    enc=midget(set$,12,1)                      ' encoding

    mod=midget(set$,21,1)                        ' playback mode

    levth=midget(set$,22,2)         ' trigger level for send on level

    sndtout=midget(set$,24,2) / 100         ' inactivity timeout - stop "send on level" after this number of milliseconds

    frmsz=midget(set$,28,2)

    kplv=and(1,midget(set$,30,1))                        ' send keepalives
    rtp=and(1,shr(midget(set$,30,1),1))                        ' rtp mode

    delms=midget(set$,26,2)			' RTP delay in milliseconds
  
    l=midget(set$,31,4)                       ' dst IP
    If l Then                                   ' if nonzero, set it
      rho$=sprintf$("%lA",l)
      rpo=midget(set$,35,2)                    ' dst port
    Else
      rho$=""
    endif                                       ' otherwise, in learn mode
    rcvaddr=midget(set$,41,4)                  ' receiving IP
    rcvip$=sprintf$("%lA",rcvaddr)
    syslog "destination IP:"+rho$

    syslog "receving IP:"+rcvip$

    port=midget(set$,37,2)                     ' listen port here (local)
    
    'gateway    parameters
    gwprt=midget(set$,109,2)
    gwtyp=midget(set$,102,1)                 'tcp or UDP  
    conty=midget(set$,103,1)                 'connection type for tcp gateway
    gwip=midget(set$,105,4)
    gwip$=sprintf$("%lA",gwip)
    'relay timout
    tmout=midget(set$,111,2)
    
    ' initialize RTP block
    midset _Mx$,1,2,&H80                        ' 0x8000 - init tx RTP block
    midset _Mx$,9,-4,1                          ' SSRC field to one


    qual=&H0c08							' enc=7 for this one, but this is default (uLaw/8kHz)
    sr=8							' samples per milliseconds
    If enc=6 Then               'uLaw/24kHz
        qual=&H0c18
        midset _Mx$,2,1,97          ' RTP payload type uLAW/24 (Barix)
        sr=24							' samples per milliseconds
    endif
    If enc=8 Then               'aLaw/24kHz
        qual=&H0d18
        midset _Mx$,2,1,98          ' RTP payload type ALAW/24 (Barix)
        sr=24							' samples per milliseconds
    endif
    If enc=9 Then               'aLaw/8kHz
        qual=&H0d08
        midset _Mx$,2,1,8           ' RTP payload type PCMA
    endif
    If enc=10 Then              'PCM/24kHz
        qual=&H0e18
        midset _Mx$,2,1,99          ' RTP payload type PCM/24 (Barix)
        sr=24							' samples per milliseconds
    endif
    If enc=11 Then              'PCM/8kHz
        qual=&H0e08
        midset _Mx$,2,1,96          ' RTP payload type PCM/8 (Barix)
   endif
        
    'AUD:mode,flags,quality,delay,frame size in samples,ssrc"
    Set$="AUD:11,"+str$(1+128*(1-rtp))+","+str$(qual)+","+str$(delms)+","+str$(frmsz*sr)+",1"      ' last parameter SSRC
    
    Open Set$ as 7
    Write 7,str$(ad_gain),-2                                    'set A/D Gain
    Write 7,str$(mic_gain),-1                                   'set Mic Gain
    Write 7,str$(inp),-3                                'set input source to mic/line
    Write 7,str$(stereo),-4                             'set mono/stereo
    Write 7,str$(vol),-12                                       'set volume to vol


    Open "UDP:"+rcvip$+":"+str$(port) as 4         ' audio (listen) port
    
    on TIMER1 GOSUB 20000                       ' timing, I/O, management
    TIMER 1,100                                 ' every 100ms i/O handling

    '''on TIMER2 GOSUB 30000                       ' timing, I/O, management
    '''TIMER 2,30                                  ' every 30ms serial gateway (sending part)

    '''On UDP GoSub 10000                          ' process audio input HERE

    '--- serial gateway, bgn --------------------------------------------------
    'disable gateway function if port is improper
    if gwprt = 0 then gwtyp = 0

    if gwtyp = 1 then  
        open "UDP:0.0.0.0:"+str$(gwprt) as 1 : syslog "Serial Gateway: UDP connection opened" + str$(gwprt)  'open passive UDP gateway immediately
    endif

    if gwtyp = 2 then
            cnwai = _TMR_(0) + 10000
            if conty = 1 then open "TCP:0.0.0.0:"+str$(gwprt) as 5 : syslog "Serial Gateway: Passive TCP connection opened " + str$(gwprt)
            if and(conty = 0,gwip <> 0) then open "TCN:"+gwip$+":" + str$(gwprt) as 5 : syslog "Serial Gateway: Active TCP connection opening with " + gwip$ + str$(gwprt)
    endif

    'open "COM:76800,N,8,1,NON:1" as 2 'open serial port for gateway
    open com$ as 2 'open serial port for the gateway

    '--- serial gateway, end --------------------------------------------------

100
    'in case of tcp active connection
    if and(gwtyp = 2,conty = 0,gwip <> 0,connected(5) = 0, cnwai < _TMR_(0) ) then
        if mediatype(5) then close 5 
        open "TCN:"+gwip$+":"+ str$(gwprt) as 5 
        syslog "Serial Gateway: Active TCP connection reopening with " + gwip$ + str$(gwprt)
        cnwai = _TMR_(0) + 10000
    endif

    if gwtyp > 0 then gosub 5000

                                             ' loop here, output audio done in timer routine
    'get input level 
    read 7,_Ms$,-1  'left input peak level (mono)
    peak_in=lastlen(7)
    read 7,_Ms$,-3  'left output peak level (mono)
    peak_out=lastlen(7)
    
    'if and(mod=3,or(levr>=levth,levl>=levth)) then rf=sndtout
    if and(mod=3,peak_in>=levth) then rf=sndtout	' we use just mono input (peak_in) 

    'syslog "peak_in="+str$(peak_in)+" peak_out="+str$(peak_out)

    l1k=lastlen(4)
    if l1k<0 then gosub 10000
    

    read 7,_Ms$                                 ' read one frame - always - if availabe (to keep buf empty)

    l=lastlen(7)
    if l=0 then goto 100            ' no data
    
    
    If Len(rho$)=0 Then GoTo 100                ' cannot send if no address
   
    If rf>0 Then                                ' send audio
      write 4,_Ms$,l,rho$,rpo
      cnt=cnt+l
      seq=and(65535,seq+1)
      goto 100
    endif

    ' send keepalive

    if kplv=0 then goto 100     ' keepalives disabled in setup, jump to main loop again
    If _TMR_(0)-rip<=4000 Then goto 100

    rip=_TMR_(0)
    Write 4,_Mx$,1,rho$,rpo
    syslog "keepalive to "+rho$+" port "+str$(rpo)
    GoTo 100



10000
      If rto Then rto=1000                                      ' reset timeout for dropping partner info (10s)
      If Len(rho$)=0 Then gosub 11000                      ' learn remote partner
      
      read 4,_Mr$
      If l1k>-12 Then return                                      ' presumably keepalive, not enough data

     'set relay  on audio   
      rl = _TMR_(0)
      if tmout > 0 then ioctl 1,1

      If mod=2 Then rf=5
      Write 7,_Mr$,-l1k                          ' output audio
      return

11000       ' learn new remote partner
      rho$=rmthost$(4)
      rpo=rmtport(4)
      syslog "new remote partner is ip "+rho$+sprintf$(" port %u",rpo)
      rto=1000                                ' 10 second timeout
      return


20000

    If mod=0 Then rf=9999
    If mod=1 Then
      If OR(iostate(201)=2,iostate(201)=0) Then rf=5 Else rf=0
    endif

    If rf>0 Then rf=rf-1

    'if relay timeout is configured, timeout > 0    
    if tmout > 0 then If _TMR_(0) - rl > tmout Then ioctl 1,0                    ' relay off

    If rto>0 Then
      rto=rto-1
      If rto=0 Then rho$="" : syslog "clear remote info"
    endif
    Return

30000
    '--- serial gateway, bgn --------------------------------------------------
    read 2,_Mb$
    lb=lastlen(2)
    if and (lb,len(rho$)>0) then write 1,_Mb$,lb,rho$,gwprt
    return
30010
    read 1,_Mb$
    write 2,_Mb$,-lb2
    return
    '--- serial gateway, end --------------------------------------------------
4000 
    'TCP Gateway begin-------------------------------------------------
    read 2, _Mb$
    if not(connected(5)) then return
    write 5,_Mb$,lastlen(2)
    return
4010
    if not(connected(5)) then return
    read 5,_Mb$
    write 2,_Mb$,lastlen(5)
    return  
'TCP Gateway ends --------------------------------------------------
5000
    'data over gateway
    if gwtyp = 1   then
        if filesize(2) then gosub 30000 
        lb2=lastlen(1)              ' UDP serial gateway receiver
        if lb2<0 then gosub 30010
    else
        if connected(5) then 
            cnwai = _TMR_(0)
            if filesize(2) then gosub 4000
            if filesize(5)  then gosub 4010 
       endif
    endif
return

