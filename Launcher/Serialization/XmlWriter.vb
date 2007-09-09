
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
Imports System.Xml
Imports System.IO

Namespace QuantLibXL

    Public Class XmlWriter
        Implements ISerializer

        ''''''''''''''''''''''''''''''''''''''''''
        ' private members
        ''''''''''''''''''''''''''''''''''''''''''

        Private doc_ As XmlTextWriter

        ''''''''''''''''''''''''''''''''''''''''''
        ' initialization/finalization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New(ByVal path As String)

            doc_ = New XmlTextWriter(New StreamWriter(path))
            doc_.Formatting = Formatting.Indented

        End Sub

        Public Sub close() Implements ISerializer.close

            doc_.Close()

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' public serializer interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serializeObject(ByRef serializable As ISerializable, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObject

            doc_.WriteStartElement(className)
            serializable.serialize(Me, versionNumber)
            doc_.WriteEndElement()

        End Sub

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectCollection

            For Each serializable As ISerializable In serializableCollection
                serializeObject(serializable, className, versionNumber)
            Next

        End Sub

        Public Sub serializeObjectCollection2(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectCollection2

            serializeObjectCollection(serializableCollection, className, versionNumber)

        End Sub

        Public Sub serializeObjectList(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectList

            serializeObjectCollection(serializableCollection, className, versionNumber)

        End Sub

        Public Sub serializeAttribute(ByRef attr As String, ByVal tag As String) Implements ISerializer.serializeAttribute

            doc_.WriteAttributeString(tag, attr)

        End Sub

        Public Sub serializePropertyList(ByRef attr() As String, ByVal listTag As String, ByVal itemTag As String) Implements ISerializer.serializePropertyList

            doc_.WriteStartElement(listTag)
            For i As Integer = 0 To UBound(attr)
                serializeProperty(attr(i), itemTag)
            Next
            doc_.WriteEndElement()

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String, ByVal tag As String) Implements ISerializer.serializeProperty

            doc_.WriteStartElement(tag)
            doc_.WriteString(prop)
            doc_.WriteEndElement()

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Boolean, ByVal tag As String) Implements ISerializer.serializeProperty

            doc_.WriteStartElement(tag)
            doc_.WriteString(prop)
            doc_.WriteEndElement()

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Integer, ByVal tag As String) Implements ISerializer.serializeProperty

            doc_.WriteStartElement(tag)
            doc_.WriteString(prop)
            doc_.WriteEndElement()

        End Sub

    End Class

End Namespace
