
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

    Public Class RegistryEditor

        ''''''''''''''''''''''''''''''''''''''''''
        ' private members
        ''''''''''''''''''''''''''''''''''''''''''

        Private rootKey_ As RegistryKey
        Private Const ROOT_KEY_NAME = "HKEY_CURRENT_USER\Software"

        Public Function keyExists(ByVal keyName As String) As Boolean
            keyExists = rootKey_.OpenSubKey(keyName, True) IsNot Nothing
        End Function

        Public Function valueExists(ByVal keyName As String, ByVal name As String) As Boolean
            Dim key As RegistryKey = rootKey_.OpenSubKey(keyName, True)
            If key Is Nothing Then Exit Function
            For Each testName As String In key.GetValueNames()
                If name = testName Then
                    valueExists = True
                    Exit Function
                End If
            Next
        End Function

        Public Function getValue(ByVal keyName As String, ByVal name As String) As String
            Dim key As RegistryKey = openKey(keyName)
            Try
                getValue = key.GetValue(name)
            Catch ex As Exception
                Throw New Exception("error processing registry key """ _
                    & ROOT_KEY_NAME & keyName _
                    & """ - could not get value of child """ & name & """")
            End Try
        End Function

        Public Function createKey(ByVal keyName As String) As RegistryKey
            createKey = rootKey_.CreateSubKey(keyName)
            If createKey Is Nothing Then
                Throw New Exception("unable to create registry key " & _
                    ROOT_KEY_NAME & keyName)
            End If
        End Function

        Public Sub setValue(ByVal keyName As String, ByVal name As String, ByVal value As Object)
            Dim key As RegistryKey = openKey(keyName)
            Try
                key.SetValue(name, value)
            Catch ex As Exception
                Throw New Exception("error processing registry key """ _
                    & ROOT_KEY_NAME & keyName _
                    & """ - could not set child """ & name _
                    & """ to value """ & value & """")
            End Try
        End Sub

        Public Sub deleteKey(ByVal keyName As String)
            rootKey_.DeleteSubKeyTree(keyName)
        End Sub

        Public Sub moveKey(ByVal sourceKeyName As String, ByVal targetKeyName As String)
            createKey(targetKeyName)
            Dim sourceKey As RegistryKey = openKey(sourceKeyName)
            Dim targetKey As RegistryKey = createKey(targetKeyName)
            For Each subKeyName As String In sourceKey.GetSubKeyNames()
                Dim sourceKey2 As RegistryKey = sourceKey.OpenSubKey(subKeyName, True)
                Dim targetKey2 As RegistryKey = targetKey.CreateSubKey(subKeyName)
                Dim value As String
                For Each name As String In sourceKey2.GetValueNames
                    value = sourceKey2.GetValue(name)
                    targetKey2.SetValue(name, value)
                Next
            Next
            deleteKey(sourceKeyName)
        End Sub

        Public Function subKeyNames(ByVal keyName As String) As String()
            Dim key As RegistryKey = openKey(keyName)
            subKeyNames = key.GetSubKeyNames()
        End Function

        Public Sub renameValue(ByVal keyName As String, ByVal oldValName As String, ByVal newValName As String)
            Dim key As RegistryKey = openKey(keyName)
            Try
                Dim value As Object = key.GetValue(oldValName)
                key.SetValue(newValName, value)
                key.DeleteValue(oldValName)
            Catch ex As Exception
                Throw New Exception("error processing registry key """ _
                    & ROOT_KEY_NAME & keyName _
                    & """ - could not rename value from """ & oldValName _
                    & """ to value """ & newValName & """")
            End Try
        End Sub

        Private Function openKey(ByVal keyName As String) As RegistryKey
            openKey = rootKey_.OpenSubKey(keyName, True)
            If openKey Is Nothing Then
                Throw New Exception("unable to open registry key " & _
                    ROOT_KEY_NAME & keyName)
            End If
        End Function

        Public Sub New()
            rootKey_ = Registry.CurrentUser.OpenSubKey("Software", True)
            If rootKey_ Is Nothing Then
                Throw New Exception("unable to access registry key " _
                    & ROOT_KEY_NAME)
            End If
        End Sub

    End Class

End Namespace
