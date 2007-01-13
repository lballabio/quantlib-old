
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
        ' properties
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

        Public Sub serializeObject(ByRef serializable As ISerializable, ByVal className As String) Implements ISerializer.serializeObject
            serializable = Factory.make(className)
            pushKey(serializable.Name)
            serializable.serialize(Me)
            popKey()
        End Sub

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, ByVal className As String) Implements ISerializer.serializeObjectCollection
            serializableCollection = New Collection
            Dim serializable As ISerializable
            For Each keyName As String In currentKey_.GetSubKeyNames
                serializable = Factory.make(className)
                pushKey(keyName)
                serializable.serialize(Me)
                serializableCollection.Add(serializable, keyName)
                popKey()
            Next
        End Sub

        Public Sub serializeAttribute(ByRef attr As String, ByVal tag As String) Implements ISerializer.serializeAttribute
            attr = CurrentValueString(tag)
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String, ByVal tag As String) Implements ISerializer.serializeProperty
            If CurrentKeyHasValue(tag) Then
                prop = CurrentValueString(tag)
            Else
                Throw New TagException(currentKeyName_, tag)
            End If
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop() As String, ByVal tag As String) Implements ISerializer.serializeProperty
            ReDim prop(0)
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
