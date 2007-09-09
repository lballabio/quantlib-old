
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

    Public Class TagException
        Inherits ApplicationException

        Public Sub New(ByVal currentTag As String, ByVal expectedChildTag As String)

            MyBase.New("the key """ & currentTag & _
                """ has no child named """ & expectedChildTag & """")

        End Sub

    End Class

    Public Class RegistryReader
        Inherits RegistrySerializer
        Implements ISerializer

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Private ReadOnly Property CurrentKeyHasValue(ByVal tag As String) As Boolean

            Get
                CurrentKeyHasValue = Not currentKey_.GetValue(tag) Is Nothing
            End Get

        End Property

        Private Overloads ReadOnly Property CurrentValueString(ByVal tag As String) As String

            Get
                If currentKey_.GetValue(tag) Is Nothing Then
                    Throw New TagException(currentKeyName_, tag)
                Else
                    CurrentValueString = currentKey_.GetValue(tag).ToString
                End If
            End Get

        End Property

        Private Overloads ReadOnly Property CurrentValueBoolean(ByVal tag As String) As Boolean

            Get
                Dim valStr As String = CurrentValueString(tag)
                Try
                    CurrentValueBoolean = CBool(valStr)
                Catch ex As Exception
                    Throw New Exception("error processing key """ & currentKeyName_ _
                        & """ with child """ & tag & """ - could not convert value """ _
                        & valStr & """ to boolean")
                End Try
            End Get

        End Property

        Private Overloads ReadOnly Property CurrentValueInteger(ByVal tag As String) As Integer

            Get
                Dim valStr As String = CurrentValueString(tag)
                Try
                    CurrentValueInteger = CInt(valStr)
                Catch ex As Exception
                    Throw New Exception("error processing key """ & currentKeyName_ _
                        & """ with child """ & tag & """ - could not convert value """ _
                        & valStr & """ to integer")
                End Try
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' public serializer interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serializeObject(ByRef serializable As ISerializable, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObject

            serializable = Factory.make(className)
            pushKey(serializable.Name)
            serializable.serialize(Me, versionNumber)
            popKey()

        End Sub

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectCollection

            serializableCollection = New Collection
            Dim serializable As ISerializable
            For Each keyName As String In currentKey_.GetSubKeyNames
                serializable = Factory.make(className)
                serializable.Name = keyName
                pushKey(keyName)
                serializable.serialize(Me, versionNumber)
                popKey()
                serializableCollection.Add(serializable, keyName)
            Next

        End Sub

        Public Sub serializeObjectCollection2(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectCollection2

            serializableCollection = New Collection
            Dim serializable As ISerializable
            For Each keyName As String In currentKey_.GetValueNames
                serializable = Factory.make(className)
                serializable.Name = keyName
                serializable.serialize2(Me, versionNumber)
                serializableCollection.Add(serializable, keyName)
            Next

        End Sub

        Public Sub serializeObjectList(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectList

            serializeObjectCollection(serializableCollection, className, versionNumber)

        End Sub

        Public Sub serializeAttribute(ByRef attr As String, ByVal tag As String) Implements ISerializer.serializeAttribute

            attr = CurrentValueString(tag)

        End Sub

        Public Sub serializePropertyList(ByRef attr() As String, ByVal listTag As String, ByVal itemTag As String) Implements ISerializer.serializePropertyList

            pushKey(listTag)
            ReDim attr(currentKey_.ValueCount - 1)
            Dim itemNames() As String = currentKey_.GetValueNames
            Array.Sort(itemNames)
            Dim i As Long = 0
            For Each itemName As String In itemNames
                attr(i) = currentKey_.GetValue(itemName).ToString
                i = i + 1
            Next
            popKey()

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String, ByVal tag As String) Implements ISerializer.serializeProperty

            If CurrentKeyHasValue(tag) Then
                prop = CurrentValueString(tag)
            Else
                Throw New TagException(currentKeyName_, tag)
            End If

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Boolean, ByVal tag As String) Implements ISerializer.serializeProperty

            If CurrentKeyHasValue(tag) Then
                prop = CurrentValueBoolean(tag)
            Else
                Throw New TagException(currentKeyName_, tag)
            End If

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Integer, ByVal tag As String) Implements ISerializer.serializeProperty

            If CurrentKeyHasValue(tag) Then
                prop = CurrentValueInteger(tag)
            Else
                Throw New TagException(currentKeyName_, tag)
            End If

        End Sub

        Sub close() Implements ISerializer.close

        End Sub

    End Class

End Namespace
