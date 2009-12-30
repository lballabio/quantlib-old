#!/bin/bash

# execute this script from the root of an uncompressed QuantLib-SWIG tarball

# 1) get reference lists of distributed files (done with find; this is
# why this script should be run from an uncompressed tarball created
# with 'make dist', not from a working copy.)

find CSharp/csharp -name '*.cs' \
| awk -F'/' '{ print $3 }' \
| sort > csharp.ref.files

# 2) extract file names from VC7 project.

grep -o -E 'RelPath *= *".*"' CSharp/csharp/NQuantLib_vc7.csproj \
| awk -F'"' '{ print $2 }' \
| sort > csharp.vc7.files

# 3) Same for VC8...

grep -o -E 'Compile *Include *= *".*"' CSharp/csharp/NQuantLib_vc8.csproj \
| awk -F'"' '{ print $2 }' \
| sort > csharp.vc8.files

# 4) ...and VC9.

grep -o -E 'Compile *Include *= *".*"' CSharp/csharp/NQuantLib_vc9.csproj \
| awk -F'"' '{ print $2 }' \
| sort > csharp.vc9.files

# 6) write out differences...

echo 'Visual Studio 7:' > sync.report
diff -b csharp.vc7.files csharp.ref.files >> sync.report

echo '' >> sync.report
echo '' >> sync.report
echo 'Visual Studio 8:' >> sync.report
diff -b csharp.vc8.files csharp.ref.files >> sync.report

echo '' >> sync.report
echo '' >> sync.report
echo 'Visual Studio 9:' >> sync.report
diff -b csharp.vc9.files csharp.ref.files >> sync.report

# 7) and cleanup
rm -f csharp.*.files

cat sync.report

