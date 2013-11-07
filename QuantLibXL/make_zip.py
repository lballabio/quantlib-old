
import sys
import os
import shutil
import glob
import zipfile

QLXL_VERSION = "QuantLibXL-1.3.0"
ROOT_DIR = QLXL_VERSION + "/"

def prompt_exit(msg='', status=0):
    if msg:
        print msg
    if sys.platform == 'win32':
        raw_input('press any key to exit')
    sys.exit(status)

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
        #print name
        zfile.write(sourcePath, ROOT_DIR + targetPath, zipfile.ZIP_DEFLATED)

zfile = zipfile.ZipFile("zip/" + QLXL_VERSION + ".zip", "w")

os.path.walk("Data", visit, (zfile, ".gitignore", None))
zfile.write("Docs/QuantLibXL-docs-1.3.0.chm", ROOT_DIR + "Docs/QuantLibXL-docs-1.3.0.chm", zipfile.ZIP_DEFLATED)
os.path.walk("framework", visit, (zfile, "ReadMe.txt", None))
os.path.walk("Workbooks", visit, (zfile, None, None))
zfile.write("xll/QuantLibXL-vc90-mt-s-1_3_0.xll", ROOT_DIR + "xll/QuantLibXL-vc90-mt-s-1_3_0.xll", zipfile.ZIP_DEFLATED)
zfile.write("zip/README.txt", ROOT_DIR + "README.txt", zipfile.ZIP_DEFLATED)
os.path.walk("../QuantLibAddin/gensrc/metadata", visit, (zfile, None, "../QuantLibAddin/gensrc/"))
zfile.write("../XL-Launcher/bin/Addin/Launcher.xla", ROOT_DIR + "Launcher.xla", zipfile.ZIP_DEFLATED)
zfile.write("../XL-Launcher/bin/Addin/session_file.xml.zipfile", ROOT_DIR + "session_file.xml", zipfile.ZIP_DEFLATED)

#for name in glob.glob("Data/*"):
#    print name
#    #zfile.write(name, os.path.basename(name), zipfile.ZIP_DEFLATED)

zfile.close()

