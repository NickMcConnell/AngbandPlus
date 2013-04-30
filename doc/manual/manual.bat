SET DIR_PYTHON=..\..\..\python-2.7.3
SET DIR_TOOLS=..\..\..\docutils-0.9.1\tools
SET DIR_PDF=..\..\..\rst2pdf-0.92\bin
SET DIR_HELP=..\..\lib\help

REM manual.html
%DIR_PYTHON%\python %DIR_TOOLS%\rst2html.py %DIR_HELP%\index.txt --stylesheet-path %DIR_HELP%\angband.css %DIR_HELP%\manual.html
pause

REM manual.pdf
%DIR_PDF%\rst2pdf %DIR_HELP%\index.txt -o %DIR_HELP%\manual.pdf
pause
