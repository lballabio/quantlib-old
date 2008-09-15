
README.TXT file for the binary installation of the QuantLib Addin for
OpenOffice.org Calc.

- This build requires OpenOffice.org version 2.4 on Windows XP.  The Addin has
  a dependency on the VC7 runtime DLLs MSVCR71.DLL and MSVCP71.DLL, these
  should be available on Windows XP.

- Identify the program directory for your installation of Calc, e.g.

    C:\Program Files\OpenOffice.org 2.4\program

- Copy the Addin files QuantLibAddinCalc-vc71-mt-0_9_0.rdb and
  QuantLibAddinCalc-vc71-mt-0_9_0.dll to the program directory.

- In the program directory, edit file uno.ini as explained at
  http://www.quantlibaddin.org/calc.html.  Or make a backup of your uno.ini
  file and try using the modified uno.ini file that is included with this
  release.

- If Calc and/or OpenOffice is currently running, stop it.  If it's loaded in
  the Quick Launch section of the Windows Task Bar, unload it.  Now start Calc
  and hit Ctrl-F2.  If the Addin has loaded successfully then the QuantLib
  functions should appear in the drop down list.

- The example workbook is Option.ods.

- For more information, visit http://www.quantlibaddin.org/calc.html or send email
  to quantlib-users@lists.sf.net.

