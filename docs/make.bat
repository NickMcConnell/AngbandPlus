SET DIR_PYTHON=..\..\Python27

python %DIR_PYTHON%\Scripts\rst2html.py index.rst --stylesheet-path .\_static\style.css ..\Manual.html
pause