SET DIR_PYTHON=..\..\..\Python27
SET DIR_HELP=..\..\lib\help

REM manual.html
python %DIR_PYTHON%\Scripts\rst2html.py %DIR_HELP%\index.txt --stylesheet-path %DIR_HELP%\angband.css %DIR_HELP%\manual.html
pause

REM manual.pdf
%DIR_PYTHON%\Scripts\rst2pdf %DIR_HELP%\index.txt -o %DIR_HELP%\manual.pdf
pause
