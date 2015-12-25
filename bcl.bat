set TARGET=%1.bas
set IS_BCL=0

cd %1
del *.bak
if exist %1.bcl (
	mkdir bcl
	copy *.bcl bcl
	set TARGET=%1.bcl
	set IS_BCL=1
)

C:\barix\tools\tokenizer.exe barionet100 %TARGET%
if errorlevel 1 goto quit
if "%IS_BCL%"=="1" del %1.bas
cd ..
C:\barix\tools\web2cob /o %1.cob /d %1
if !%2==! goto endit
tftp -i 192.168.1.24 put %1.cob WEB4
goto endit
:quit
echo "ERROR - TOKENIZER REPORTS FAILURE"
cd ..
:endit
