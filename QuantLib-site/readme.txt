
$Id$

check links:
    python webchecker.py -v ../QuantLib-site/
    python webchecker.py -v http://quantlib.org/

ssh login:
  ssh -l username quantlib.org

scp upload:
  scp ..\QL.zip username@shell.sourceforge.net:/home/groups/q/qu/quantlib/htdocs

