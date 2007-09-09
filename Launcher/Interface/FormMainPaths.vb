
'Copyright (C) 2006, 2007 Eric Ehlers

'This file is part of QuantLib, a free-software/open-source library
'for financial quantitative analysts and developers - http://quantlib.org/

'QuantLib is free software: you can redistribute it and/or modify it
'under the terms of the QuantLib license.  You should have received a
'copy of the license along with this program; if not, please email
'<quantlib-dev@lists.sf.net>. The license is also available online at
'<http://quantlib.org/license.shtml>.

'This program is distributed in the hope that it will be useful, but WITHOUT
'ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
'FOR A PARTICULAR PURPOSE.  See the license for more details.

Imports System.IO
Imports System.Deployment.Application

Partial Public Class FormMain

    Private Function deriveDefaultDir(ByVal testPath As String, ByVal subDir As String) As String

        Try

            If dirExists(testPath) Then
                deriveDefaultDir = testPath
            ElseIf dirExists(qlxlDir_ & "\" & subDir) Then
                deriveDefaultDir = qlxlDir_ & "\" & subDir
            Else
                deriveDefaultDir = ""
            End If
            Exit Function

        Catch ex As Exception

            deriveDefaultDir = ""

        End Try

    End Function

    Private Function deriveDefaultFile(ByVal testFile As String, Optional ByVal relativePath As String = "") As String

        Try

            If fileExists(testFile) Then
                deriveDefaultFile = testFile
            ElseIf relativePath.Length > 0 And dirExists(qlxlDir_ & "\" & relativePath) Then
                deriveDefaultFile = qlxlDir_ & "\" & relativePath
            Else
                deriveDefaultFile = ""
            End If
            Exit Function

        Catch ex As Exception

            deriveDefaultFile = ""

        End Try

    End Function

End Class
