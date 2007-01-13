
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

        Public Sub serializeObject(ByRef serializable As ISerializable, ByVal className As String) Implements ISerializer.serializeObject
            pushKey(className)
            serializable.serialize(Me)
            popKey()
        End Sub

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, ByVal className As String) Implements ISerializer.serializeObjectCollection
            For Each serializable As ISerializable In serializableCollection
                serializeObject(serializable, serializable.Name)
            Next
        End Sub

        Public Sub serializeAttribute(ByRef attr As String, ByVal tag As String) Implements ISerializer.serializeAttribute
            SetValue(tag, attr)
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String, ByVal tag As String) Implements ISerializer.serializeProperty
            SetValue(tag, prop)
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String(), ByVal tag As String) Implements ISerializer.serializeProperty
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
