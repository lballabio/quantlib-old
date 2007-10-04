
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

Module Utilities

    Private Const QUANTLIBXL_CONFIG_PATH As String = "QUANTLIBXL_CONFIG_PATH"
    Private Const EXCEL_10_PATH As String = "C:\Program Files\Microsoft Office\OFFICE10\EXCEL.EXE"
    Private Const EXCEL_11_PATH As String = "C:\Program Files\Microsoft Office\OFFICE11\EXCEL.EXE"

    Function fileExists(ByVal PathName As String) As Boolean

        Try
            Dim fileAttr As FileAttribute
            fileAttr = GetAttr(PathName)
            fileExists = ((fileAttr And FileAttribute.Normal) = FileAttribute.Normal) _
                And ((fileAttr And FileAttribute.Directory) <> FileAttribute.Directory)
            Exit Function
        Catch ex As Exception
        End Try

    End Function

    Function dirExists(ByVal PathName As String) As Boolean

        Try
            Dim fileAttr As FileAttribute
            fileAttr = GetAttr(PathName)
            dirExists = ((fileAttr And FileAttribute.Directory) = FileAttribute.Directory)
            Exit Function
        Catch ex As Exception
        End Try

    End Function

    Function configPath() As String

        configPath = Environ(QUANTLIBXL_CONFIG_PATH)

        If Len(configPath) < 1 Then
            Throw New Exception("Error: environment variable " & _
            QUANTLIBXL_CONFIG_PATH & " is not set.  Please set it " & _
            "equal to the location of the development environment " & _
            "for the QuantLibXL launcher, e.g:" & _
            vbCrLf & vbCrLf & QUANTLIBXL_CONFIG_PATH & _
            "=C:\projects\trunk\Launcher")
        End If

        If Not dirExists(configPath) Then
            Throw New Exception("Error: environment variable " & _
            QUANTLIBXL_CONFIG_PATH & " has the following value:" & _
            vbCrLf & vbCrLf & configPath & vbCrLf & vbCrLf & _
            "This is not a valid path.  Please set QUANTLIBXL_CONFIG_PATH " & _
            "equal to" & vbCrLf & vbCrLf & "the location of the " & _
            "development environment for the QuantLibXL launcher, e.g:" & _
            vbCrLf & vbCrLf & QUANTLIBXL_CONFIG_PATH & _
            "=C:\path\to\trunk\Launcher")
        End If

    End Function

    Function deriveDefaultExcelPath() As String

        If fileExists(EXCEL_11_PATH) Then
            deriveDefaultExcelPath = EXCEL_11_PATH
        ElseIf fileExists(EXCEL_10_PATH) Then
            deriveDefaultExcelPath = EXCEL_10_PATH
        Else
            deriveDefaultExcelPath = ""
        End If

    End Function

End Module
