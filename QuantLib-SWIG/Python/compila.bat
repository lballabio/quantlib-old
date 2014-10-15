rem aggiungere --debug a build e bdist per compilare la versione debug (serve python compilato in debug!)
python setup.py wrap
if not errorlevel 1 python setup.py build --static
if not errorlevel 1 python setup.py bdist --formats=wininst
if not errorlevel 1 python setup.py bdist --formats=msi

pause