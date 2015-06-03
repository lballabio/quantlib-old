
import sys
import os
import shutil
import datetime
import glob
import zipfile
import argparse
import re

QLXL = "QuantLibXL"
VERSION = "1.6.0"
QLXL_VERSION = QLXL + "-" + VERSION
ROOT_DIR = QLXL_VERSION + "\\"

class ZipFile:

    root = None
    zipFile = None

    def __init__(self, path, root):
        self.root = root
        self.zipFile = zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED)

    def __del__(self):
        self.zipFile.close()

    def zip(self, sourcePath, targetPath = None):
        print sourcePath
        if targetPath is None:
            targetPath = self.root + sourcePath
        self.zipFile.write(sourcePath, targetPath)

    def zipGlob(self, path, excludeFiles = None):
        for fileName in glob.glob(path):
            if excludeFiles is not None:
                for r in excludeFiles:
                    if r.match(fileName):
                        continue
            print fileName
            self.zip(fileName)

class Selector:

    zipFile = None
    inputPath = None
    incDirs = None
    excDirs = None
    incFiles = None
    excFiles = None

    def __init__(self, zipFile, inputPath, incDirs=None, excDirs=None, incFiles=None, excFiles=None):
        self.zipFile = zipFile
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
                    continue
            for f in files:
                if self.includeFile(f):
                    self.zipFile.zip(root + f)

    def excludeDir(self, d):
        if self.excDirs is not None:
            for r in self.excDirs:
                if r.match(d):
                    return True
        if self.incDirs is None:
            return False
        else:
            for r in self.incDirs:
                if r.match(d):
                    return False
        return True

    def includeFile(self, f):
        if self.excFiles is not None:
            for r in self.excFiles:
                if r.match(f):
                    return False
        if self.incFiles is None:
            return True
        else:
            for r in self.incFiles:
                if r.match(f):
                    return True
        return False

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

def makeZipStatic():
    zipFilePath = "zip/%s-%s.zip" % (QLXL_VERSION, datetime.datetime.now().strftime("%Y%m%d%H%M"))
    zfile = zipfile.ZipFile(zipFilePath, "w", zipfile.ZIP_DEFLATED)

    # Zip up some specific files from the QuantLibXL directory.
    #zfile.write("Docs/QuantLibXL-docs-1.6.0.chm", ROOT_DIR + "Docs/QuantLibXL-docs-1.6.0.chm")
    zfile.write("xll/QuantLibXL-vc120-mt-s-1_6_0.xll", ROOT_DIR + "xll/QuantLibXL-vc120-mt-s-1_6_0.xll")
    zfile.write("zip/README.txt", ROOT_DIR + "README.txt")
    # Recursively zip some subdirectories of the QuantLibXL directory.
    #os.path.walk("Data", visit, (zfile, ".gitignore", None))
    os.path.walk("Data2/XLS", visit, (zfile, ".gitignore", None))
    os.path.walk("framework", visit, (zfile, "ReadMe.txt", None))
    #os.path.walk("Workbooks", visit, (zfile, None, None))
    # Zip up some files from other projects in the repo.
    os.path.walk("../QuantLibAddin/gensrc/metadata", visit, (zfile, None, "../QuantLibAddin/gensrc/"))
    zfile.write("../XL-Launcher/bin/Addin/Launcher.xla", ROOT_DIR + "Launcher.xla")
    for fileName in glob.glob("../XL-Launcher/bin/Addin/session_file.*-s-*.xml"):
        baseName = os.path.basename(fileName)
        if -1 != baseName.find("-dev"): continue
        zfile.write("../XL-Launcher/bin/Addin/" + baseName, ROOT_DIR + baseName)
    for fileName in glob.glob("../XL-Launcher/bin/Addin/session_file.*-s-*.bat"):
        baseName = os.path.basename(fileName)
        if -1 != baseName.find("-dev"): continue
        zfile.write("../XL-Launcher/bin/Addin/" + baseName, ROOT_DIR + baseName)

    zfile.close()

def zipBinaryFiles(zipFile):
    zipFile.zip("zip\\README.txt", zipFile.root + "README.txt")
    zipFile.zip("xll\\QuantLibXL-vc120-mt-s-1_6_0.xll")
    zipFile.zip("xll\\QuantLibXL-vc120-x64-mt-s-1_6_0.xll")
    zipFile.zip("Docs\\QuantLibXL-docs-1.6.0.chm")
    Selector(
        inputPath = 'StandaloneExamples',
        zipFile = zipFile,
        incFiles = (
            re.compile('^.*\.xls$'),
            re.compile('^.*\.xlsx$'),),
    )

def zipFrameworkFiles(zipFile):
    zipFile.zip("../XL-Launcher/bin/Addin/Launcher.xla", zipFile.root + "Launcher.xla")
    zipFile.zip("../XL-Launcher/bin/Addin/session_file.public.live.xml", zipFile.root + "session_file.xml")
    zipFile.zip("../XL-Launcher/bin/Addin/session_file.public.live.bat", zipFile.root + "session_file.public.live.bat")
    zipFile.zip("../XL-Launcher/bin/Addin/session_file.public.live.xml", zipFile.root + "session_file.public.live.xml")
    Selector(
        inputPath = 'Data2',
        zipFile = zipFile,
    )
    Selector(
        inputPath = 'framework',
        zipFile = zipFile,
    )

def zipSourceFiles(zipFile):

    zipFile.zipGlob("*.sln")
    zipFile.zipGlob("*.txt", (re.compile("^goodpractice.txt$"),))

    zipFile.zip("Docs\\Makefile.vc")
    zipFile.zip("Docs\\quantlibxl.doxy")
    zipFile.zipGlob("Docs\\*.css")
    zipFile.zipGlob("Docs\\*.html")
    zipFile.zipGlob("Docs\\*.vcproj")
    zipFile.zipGlob("Docs\\*.vcxproj")
    zipFile.zipGlob("Docs\\images\\*.bmp")
    zipFile.zipGlob("Docs\\images\\*.ico")
    zipFile.zipGlob("Docs\\images\\*.jpg")
    zipFile.zipGlob("Docs\\images\\*.png")
    zipFile.zipGlob("Docs\\pages\\*.docs")

    Selector(
        inputPath = 'qlxl',
        zipFile = zipFile,
        excDirs = (
            re.compile('^build.*'),),
        excFiles = (
            re.compile('^.gitignore$'),
            re.compile('^Makefile.am$'),
            re.compile('^.*\.user$'),
            re.compile('^.*\.filters$')),
    )

def makeZipBinary():
    zipFile = ZipFile("zip/" + QLXL_VERSION + "-bin.zip", ROOT_DIR)
    zipBinaryFiles(zipFile)

def makeZipFramework():
    zipFile = ZipFile("zip/" + QLXL_VERSION + "-framework.zip", ROOT_DIR)
    zipBinaryFiles(zipFile)
    zipFrameworkFiles(zipFile)

def makeZipSource():
    zipFile = ZipFile("zip/" + QLXL_VERSION + ".zip", QLXL + "\\")
    zipSourceFiles(zipFile)

parser = argparse.ArgumentParser(description='zip up QuantLibXL')
parser.add_argument('-t','--target', help='target environment', required=True)
args = vars(parser.parse_args())

if 'binary' == args['target']:
    makeZipBinary()
elif 'framework' == args['target']:
    makeZipFramework()
elif 'source' == args['target']:
    makeZipSource()
elif 'static' == args['target']:
    makeZipStatic()
else:
    print "Error - unsupported target : " + args['target']

