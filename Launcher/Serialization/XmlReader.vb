
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

Namespace QuantLibXL

    Public Class XmlReader
        Implements ISerializer

        ''''''''''''''''''''''''''''''''''''''''''
        ' private members
        ''''''''''''''''''''''''''''''''''''''''''

        Private doc_ As XmlDocument
        Private currentNode_ As XmlNode

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Private ReadOnly Property NodeHasItem(ByVal tag As String) As Boolean

            Get
                NodeHasItem = currentNode_.Item(tag) IsNot Nothing
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' initialization/finalization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New(ByVal path As String, ByVal rootNodeName As String)

            doc_ = New XmlDocument()
            doc_.Load(path)
            currentNode_ = doc_
            setCurrentNode(rootNodeName)

        End Sub

        Public Sub close() Implements ISerializer.close

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' public serializer interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serializeObject(ByRef serializable As ISerializable, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObject

            serializable = Factory.make(className)
            setCurrentNode(className)
            serializable.serialize(Me, versionNumber)
            currentNode_ = currentNode_.ParentNode

        End Sub

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectCollection

            Dim node As XmlNode, currentNode As XmlNode
            Dim serializable As ISerializable
            currentNode = currentNode_
            For Each node In currentNode_.ChildNodes

                If node.Name <> className Then Continue For

                serializable = Factory.make(className)
                currentNode_ = node
                serializable.serialize(Me, versionNumber)

                If serializableCollection.Contains(serializable.Name) Then
                    Throw New Exception("node '" & className & "' contains multiple children" _
                    & " with tag '" & serializable.Name & "'.")
                End If

                serializableCollection.Add(serializable, serializable.Name)

            Next
            currentNode_ = currentNode

        End Sub

        Public Sub serializeObjectCollection2(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectCollection2

            serializeObjectCollection(serializableCollection, className, versionNumber)

        End Sub

        Public Sub serializeObjectList(ByRef serializableCollection As Collection, ByVal className As String, ByVal versionNumber As Integer) Implements ISerializer.serializeObjectList

            Dim node As XmlNode, currentNode As XmlNode
            Dim serializable As ISerializable
            Dim i As Integer = 0
            currentNode = currentNode_
            For Each node In currentNode_.ChildNodes

                If node.Name <> className Then Continue For

                serializable = Factory.make(className)
                currentNode_ = node
                serializable.serialize(Me, versionNumber)

                serializable.Name = className & CStr(i)
                i = i + 1
                serializableCollection.Add(serializable, serializable.Name)

            Next
            currentNode_ = currentNode

        End Sub

        Public Sub serializeAttribute(ByRef attr As String, ByVal tag As String) Implements ISerializer.serializeAttribute

            If currentNode_.Attributes.ItemOf(tag) Is Nothing Then
                throwNodeException(tag)
            Else
                attr = currentNode_.Attributes.ItemOf(tag).InnerText
            End If

        End Sub

        Public Sub serializePropertyList(ByRef attr() As String, ByVal listTag As String, ByVal itemTag As String) Implements ISerializer.serializePropertyList

            If Not NodeHasItem(listTag) Then throwNodeException(listTag)

            Dim listNode As XmlNode = currentNode_.Item(listTag)

            ReDim attr(listNode.ChildNodes.Count - 1)
            Dim i As Long = 0

            For Each node As XmlNode In listNode.ChildNodes

                If node.Name <> itemTag Then
                    Throw New Exception("error processing node" & vbCrLf _
                        & "    """ & listTag & """" & vbCrLf _
                        & "expected all children to have tag" & vbCrLf _
                        & "    """ & itemTag & """" & vbCrLf _
                        & "but detected a child with tag" & vbCrLf _
                        & "    """ & node.Name & """")
                End If

                attr(i) = node.InnerText
                i = i + 1

            Next

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String, ByVal tag As String) Implements ISerializer.serializeProperty

            If Not NodeHasItem(tag) Then throwNodeException(tag)
            prop = currentNode_.Item(tag).InnerText

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Boolean, ByVal tag As String) Implements ISerializer.serializeProperty

            If Not NodeHasItem(tag) Then throwNodeException(tag)

            Dim valStr As String = ""
            serializeProperty(valStr, tag)

            Try
                prop = CBool(valStr)
            Catch ex As Exception
                Throw New Exception("error processing node """ & currentNode_.Name _
                    & """ with child """ & tag & """ - could not convert value """ _
                    & valStr & """ to boolean")
            End Try

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Integer, ByVal tag As String) Implements ISerializer.serializeProperty

            If Not NodeHasItem(tag) Then throwNodeException(tag)

            Dim valStr As String = ""
            serializeProperty(valStr, tag)
            Try
                prop = CInt(valStr)
            Catch ex As Exception
                Throw New Exception("error processing node """ & currentNode_.Name _
                    & """ with child """ & tag & """ - could not convert value """ _
                    & valStr & """ to integer")
            End Try

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' private utility functions
        ''''''''''''''''''''''''''''''''''''''''''

        Private Sub setCurrentNode(ByVal tag As String)

            If Not NodeHasItem(tag) Then throwNodeException(tag)
            currentNode_ = currentNode_.Item(tag)

        End Sub

        Private Sub throwNodeException(ByVal tag As String)

            Throw New Exception("the node """ & currentNode_.Name & _
                """ has no child named """ & tag & """")

        End Sub

    End Class

End Namespace
