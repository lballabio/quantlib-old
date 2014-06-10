
import sys
import os
import shutil
import datetime
import glob
import zipfile
import argparse
import re

QLXL = "QuantLibXL"
VERSION = "1.4.0"
QLXL_VERSION = QLXL + "-" + VERSION
ROOT_DIR = QLXL_VERSION + "/"

class Selector:

    zFile = None
    zipRoot = None
    inputPath = None
    incDirs = None
    excDirs = None
    incFiles = None
    excFiles = None

    def __init__(self, zFile, zipRoot, inputPath, incDirs=None, excDirs=None, incFiles=None, excFiles=None):
        self.zFile = zFile
        self.zipRoot = zipRoot + "\\"
        self.inputPath = inputPath
        self.incDirs = incDirs
        self.excDirs = excDirs
        self.incFiles = incFiles
        self.excFiles = excFiles
        self.process()

    def process(self):
        for root, dirs, files in os.walk(self.inputPath):
            root += "\\"
            for d in dirs:
                if self.excludeDir(d):
                    dirs.remove(d)
            for f in files:
                if self.includeFile(f):
                    print root + f
                    self.zFile.write(root + f, self.zipRoot + root + f)

    def excludeDir(self, d):
        if self.excDirs is None:
            return False
        if self.incDirs is not None:
            for r in self.incDirs:
                if r.match(d):
                    return False
        for r in self.excDirs:
            if r.match(d):
                return True
        return False

    def includeFile(self, f):
        if self.excFiles is None:
            return True
        if self.incFiles is not None:
            for r in self.incFiles:
                if r.match(f):
                    return True
        for r in self.excFiles:
            if r.match(f):
                return False
        return True

def prompt_exit(msg='', status=0):
    if msg:
        print msg
    if sys.platform == 'win32':
        raw_input('press any key to exit')
    sys.exit(status)

#DELETEME
def visit(params, dirname, names):
    zfile = params[0]
    exclude = params[1]
    strip = params[2]
    if strip:
        rootDir = dirname[len(strip):]
    else:
        rootDir = dirname
    for name in names:
        if exclude == name: continue
        sourcePath = dirname + "/" + name
        targetPath = rootDir + "/" + name
        zfile.write(sourcePath, ROOT_DIR + targetPath)

def make_zip_nando():
    zipFilePath = "zip/%s-%s.zip" % (QLXL_VERSION, datetime.datetime.now().strftime("%Y%m%d%H%M"))
    zfile = zipfile.ZipFile(zipFilePath, "w", zipfile.ZIP_DEFLATED)

    # Zip up some specific files from the QuantLibXL directory.
    zfile.write("Docs/QuantLibXL-docs-1.4.0.chm", ROOT_DIR + "Docs/QuantLibXL-docs-1.4.0.chm")
    zfile.write("xll/QuantLibXLDynamic-vc90-mt-1_4_0.xll", ROOT_DIR + "xll/QuantLibXLDynamic-vc90-mt-1_4_0.xll")
    zfile.write("zip/README.txt", ROOT_DIR + "README.txt")
    # Recursively zip some subdirectories of the QuantLibXL directory.
    #os.path.walk("Data", visit, (zfile, ".gitignore", None))
    os.path.walk("Data2/XLS", visit, (zfile, ".gitignore", None))
    os.path.walk("framework", visit, (zfile, "ReadMe.txt", None))
    #os.path.walk("Workbooks", visit, (zfile, None, None))
    # Zip up some files from other projects in the repo.
    zfile.write("../ObjectHandler/xll/ObjectHandler-xll-vc90-mt-1_4_0.xll", ROOT_DIR + "xll/ObjectHandler-xll-vc90-mt-1_4_0.xll")
    os.path.walk("../QuantLibAddin/gensrc/metadata", visit, (zfile, None, "../QuantLibAddin/gensrc/"))
    zfile.write("../XL-Launcher/bin/Addin/Launcher.xla", ROOT_DIR + "Launcher.xla")
    zfile.write("../XL-Launcher/bin/Addin/session_file.HKD.xml", ROOT_DIR + "session_file.xml")
    for fileName in glob.glob("../XL-Launcher/bin/Addin/session_file.*.xml"):
        baseName = os.path.basename(fileName)
        if -1 != baseName.find("-dev"): continue
        zfile.write("../XL-Launcher/bin/Addin/" + baseName, ROOT_DIR + baseName)
    for fileName in glob.glob("../XL-Launcher/bin/Addin/session_file.*.bat"):
        baseName = os.path.basename(fileName)
        if -1 != baseName.find("-dev"): continue
        zfile.write("../XL-Launcher/bin/Addin/" + baseName, ROOT_DIR + baseName)

    zfile.close()

def make_zip_source():
    zipFilePath = "zip/" + QLXL_VERSION + ".zip"
    zFile = zipfile.ZipFile(zipFilePath, "w", zipfile.ZIP_DEFLATED)
    for fileName in glob.glob("*.sln"):
        print fileName
        zFile.write(fileName, QLXL + "\\" + fileName)
    for fileName in glob.glob("*.txt"):
        if "goodpractice.txt" != fileName:
            print fileName
            zFile.write(fileName, QLXL + "\\" + fileName)
    s = Selector(
        inputPath = 'qlxl',
        zFile = zFile,
        zipRoot = QLXL,
        incDirs = None,
        excDirs = (
            re.compile('^build.*'),),
        incFiles = None,
        excFiles = (
            re.compile('^.gitignore$'),
            re.compile('^Makefile.am$'),
            re.compile('^.*\.user$'),
            re.compile('^.*\.filters$')),
    )
    zFile.close()

parser = argparse.ArgumentParser(description='zip up QuantLibXL')
parser.add_argument('-t','--target', help='target environment', required=True)
args = vars(parser.parse_args())

if 'nando' == args['target']:
    make_zip_nando()
elif 'source' == args['target']:
    make_zip_source()
else:
    print "error"

