import os, shutil, string, sys

phpFlag = 0

def walking(skip, dirname, names):
  print
  if dirname in skip:
    print 'skipping', dirname
  else:
    print 'working in', dirname
    for name in names:
      if dirname!=os.curdir:
          filename = os.path.join(dirname, name)
      else:
          filename = name
      if os.path.isfile(filename)==1:
        if string.find(filename, ".htm")<>-1:
          print 'file:', filename, '  ----  ',
          # include navigation bar, etc
          print 'includeHTML,',
          os.system('includeHTML -i '+filename+ ' -o ' + filename + ' -b * -s')
          # fix and validate xhtml
          print 'Tidy,'
          os.system('tidy -q -m ' + filename)
          # to be added: linbot link check
          # to be added: bobby accessibility check
        elif string.find(filename, ".css")<>-1:
          #w3c css validator
          classpath = ' E:\\lib\\validator.zip org.w3c.css.css.StyleSheetCom '
          os.system('java -classpath' + classpath + filename)
        else:
          print 'file:', filename, '  ----  ',
          print 'no processing'


def webclean():
  import time, os
  sourcedir = os.curdir
  targetdir = os.curdir
  sitename="QL"
  donotpublish = ['common', 'template', 'tidy', 'quep\quep03']

  print 'sourcedir', sourcedir

  skip = []
  for z in donotpublish:
    skip.append('"' + targetdir+'\\'+z+'"')

  os.path.walk(targetdir, walking, skip)

  #oneDirAtTime = targetdir+'\\styles'
  #walking(skip, oneDirAtTime, os.listdir(oneDirAtTime))

  #linkCheck = raw_input('check links (y/n) ?')
  #if linkCheck=='y':
  #  pass

  #bobbyCheck = raw_input('check with Bobby (y/n) ?')
  #if bobbyCheck=='y':
  #  pass

  cmd = 'del ..\\' + sitename + '.tar.bz2'
  print '\nplease wait while performing:',
  print cmd
  cmd = 'tar cjf ..\\' + sitename + '.tar.bz2 .'
  print '\nplease wait while performing:',
  print cmd
  os.system(cmd)


# when executed, just run webclean():
if __name__ == '__main__':
    webclean()
    raw_input('press any key to continue')
