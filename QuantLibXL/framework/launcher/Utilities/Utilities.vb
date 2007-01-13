
'Copyright (C) 2006 Eric Ehlers

'This file is part of QuantLib, a free-software/open-source library
'for financial quantitative analysts and developers - http://quantlib.org/

'QuantLib is free software: you can redistribute it and/or modify it
'under the terms of the QuantLib license.  You should have received a
'copy of the license along with this program; if not, please email
'<quantlib-dev@lists.sf.net>. The license is also available online at
'<http://quantlib.org/reference/license.html>.

'This program is distributed in the hope that it will be useful, but WITHOUT
'ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
'FOR A PARTICULAR PURPOSE.  See the license for more details.

Module Utilities

    Public Const QUANTLIBXL_DIR As String = "QUANTLIBXL_DIR"
    Public Const EXCEL_PATH As String = "C:\Program Files\Microsoft Office\Office10\EXCEL.EXE"
    Private Const QUANTLIBXL_CONFIG_PATH = "QUANTLIBXL_CONFIG_PATH"

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
            "=C:\projects\QuantLibXL\framework\launcher")
        End If

    End Function

End Module
