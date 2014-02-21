#!/bin/bash

# execute this script from the root of an uncompressed QuantLib-SWIG tarball

# get reference lists of distributed files (done with find; this is
# why this script should be run from an uncompressed tarball created
# with 'make dist', not from a working copy.)

find CSharp/csharp -name '*.cs' \
| awk -F'/' '{ print $3 }' \
| sort > csharp.ref.files

# extract file names from VC8 project.

grep -o -E 'Compile *Include *= *".*"' CSharp/csharp/NQuantLib_vc8.csproj \
| awk -F'"' '{ print $2 }' \
| sort > csharp.vc8.files

# 4) Same for VC9...

grep -o -E 'Compile *Include *= *".*"' CSharp/csharp/NQuantLib_vc9.csproj \
| awk -F'"' '{ print $2 }' \
| sort > csharp.vc9.files

# 5) ...and VC10.

grep -o -E 'Compile *Include *= *".*"' CSharp/csharp/NQuantLib_vc10.csproj \
| awk -F'"' '{ print $2 }' \
| sort > csharp.vc10.files

# 6) write out differences...

echo 'Visual Studio 8:' > sync.report
diff -b csharp.vc8.files csharp.ref.files >> sync.report

echo '' >> sync.report
echo '' >> sync.report
echo 'Visual Studio 9:' >> sync.report
diff -b csharp.vc9.files csharp.ref.files >> sync.report

echo '' >> sync.report
echo '' >> sync.report
echo 'Visual Studio 10:' >> sync.report
diff -b csharp.vc10.files csharp.ref.files >> sync.report

# 7) and cleanup
rm -f csharp.*.files

cat sync.report

