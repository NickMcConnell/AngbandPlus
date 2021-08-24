SET DIR_PYTHON=..\..\Python27
SET DIR_HELP=..\lib\help

REM Manual.html
python %DIR_PYTHON%\Scripts\rst2html.py %DIR_HELP%\index-html.txt --stylesheet-path angband.css ..\Manual.html
pause

REM Manual.pdf
%DIR_PYTHON%\Scripts\rst2pdf %DIR_HELP%\index.txt -o %DIR_HELP%\Manual.pdf
move %DIR_HELP%\Manual.pdf ..\Manual.pdf
pause
