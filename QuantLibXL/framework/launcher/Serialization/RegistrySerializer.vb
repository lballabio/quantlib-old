
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

Imports System
Imports Microsoft.Win32

Namespace QuantLibXL

    Public Class RegistrySerializer

        ''''''''''''''''''''''''''''''''''''''''''
        ' private members
        ''''''''''''''''''''''''''''''''''''''''''

        Protected currentKey_ As RegistryKey
        Protected currentKeyName_ As String
        Protected keyStack_ As New Stack(Of RegistryKey)
        Protected keyNameStack_ As New Stack(Of String)

        ''''''''''''''''''''''''''''''''''''''''''
        ' initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub pushKey(ByVal keyName As String)
            keyStack_.Push(currentKey_)
            keyNameStack_.Push(currentKeyName_)
            currentKeyName_ = currentKeyName_ & "\" & keyName
            currentKey_ = currentKey_.CreateSubKey(keyName)
            If currentKey_ Is Nothing Then
                Throw New Exception("unable to access registry key " & _
                    currentKeyName_)
            End If
        End Sub

        Protected Sub popKey()
            currentKey_ = keyStack_.Pop
            currentKeyName_ = keyNameStack_.Pop
        End Sub

        Public Sub deleteKey(ByVal keyName As String)
            currentKey_.DeleteSubKeyTree(keyName)
        End Sub

        Public Sub New()
            currentKey_ = Registry.CurrentUser.OpenSubKey("Software", True)
            If currentKey_ Is Nothing Then
                Throw New Exception("unable to access registry key " & _
                "HKEY_CURRENT_USER\Software")
            End If
            currentKeyName_ = "HKEY_CURRENT_USER\Software"
            pushKey("QuantLibXL")
        End Sub

    End Class

End Namespace
