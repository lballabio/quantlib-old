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


def webpublish():
  import time, os
  sourcedir = os.curdir
  targetdir = os.curdir
  sitename="QL"
  donotpublish = ['common', 'template', 'tidy']

  print 'sourcedir', sourcedir

  if 1:
    cmd = 'zip -q -r '
    cmd = cmd + '..\\' + sitename +time.strftime('%Y%m%d%H%M', time.localtime())
    cmd = cmd +'.zip *.*'
    print 'performing:'
    print cmd
    print 'please wait ....',
    os.system(cmd)
    print 'done'
  else:
    targetdir = 'V:\\enotebook\\myweb\\publish'
    print 'targetdir',targetdir
    if os.path.exists(targetdir):
        shutil.rmtree(targetdir)
        print 'old', targetdir, 'removed'
    print 'copying', sourcedir,  'to', targetdir, '....',
    shutil.copytree(sourcedir, targetdir)
    print 'done'

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

  cmd = 'zip -q -r ..\\' + sitename + '.zip *.*'
  print 'performing:'
  print cmd
  print 'please wait ....',
  os.system(cmd)
  print 'done'


# when executed, just run webpublish():
if __name__ == '__main__':
    webpublish()
