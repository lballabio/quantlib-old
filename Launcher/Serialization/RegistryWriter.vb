
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

Imports System
Imports Microsoft.Win32

Namespace QuantLibXL


    Public Class RegistryWriter
        Inherits RegistrySerializer
        Implements ISerializer

        ''''''''''''''''''''''''''''''''''''''''''
        ' properties
        ''''''''''''''''''''''''''''''''''''''''''

        Private Sub SetValue(ByVal name As String, ByVal value As String)

            Try
                currentKey_.SetValue(name, value)
            Catch ex As Exception
                Throw New Exception("error processing windows registry key """ & currentKeyName_ _
                    & """ - could not set child """ & name _
                    & """ to value """ & value & """")
            End Try

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' public serializer interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serializeObject(ByRef serializable As ISerializable, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObject

            pushKey(className)
            serializable.serialize(Me, versionNumber)
            popKey()

        End Sub

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectCollection

            For Each serializable As ISerializable In serializableCollection
                serializeObject(serializable, serializable.Name, versionNumber)
            Next

        End Sub

        Public Sub serializeObjectCollection2(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectCollection2

            For Each serializable As ISerializable In serializableCollection
                serializable.serialize2(Me, versionNumber)
            Next

        End Sub

        Public Sub serializeObjectList(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectList

            Dim i As Integer = 0
            For Each serializable As ISerializable In serializableCollection
                serializeObject(serializable, className & CStr(i), versionNumber)
                i = i + 1
            Next

        End Sub

        Public Sub serializeAttribute(ByRef attr As String, ByVal tag As String) Implements ISerializer.serializeAttribute

            SetValue(tag, attr)

        End Sub

        Public Sub serializePropertyList(ByRef attr() As String, ByVal listTag As String, ByVal itemTag As String) Implements ISerializer.serializePropertyList

            pushKey(listTag)

            If attr IsNot Nothing Then
                For i As Integer = 0 To UBound(attr)
                    SetValue(itemTag & CStr(i), attr(i))
                Next
            End If

            popKey()

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String, ByVal tag As String) Implements ISerializer.serializeProperty

            SetValue(tag, prop)

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Boolean, ByVal tag As String) Implements ISerializer.serializeProperty

            SetValue(tag, prop)

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Integer, ByVal tag As String) Implements ISerializer.serializeProperty

            SetValue(tag, prop)

        End Sub

        Sub close() Implements ISerializer.close

        End Sub

    End Class

End Namespace
