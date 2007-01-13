
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
        ' properties
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

        Public Sub serializeObject(ByRef serializable As ISerializable, ByVal className As String) Implements ISerializer.serializeObject
            serializable = Factory.make(className)
            setCurrentNode(className)
            serializable.serialize(Me)
            currentNode_ = currentNode_.ParentNode
        End Sub

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, ByVal className As String) Implements ISerializer.serializeObjectCollection
            Dim node As XmlNode
            Dim serializable As ISerializable
            For Each node In currentNode_.ChildNodes
                If node.Name <> className Then Continue For
                serializable = Factory.make(className)
                currentNode_ = node
                serializable.serialize(Me)
                serializableCollection.Add(serializable, serializable.Name)
                currentNode_ = currentNode_.ParentNode
            Next
        End Sub

        Public Sub serializeAttribute(ByRef attr As String, ByVal tag As String) Implements ISerializer.serializeAttribute
            If currentNode_.Attributes.ItemOf(tag) Is Nothing Then
                throwNodeException(tag)
            Else
                attr = currentNode_.Attributes.ItemOf(tag).InnerText
            End If
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String, ByVal tag As String) Implements ISerializer.serializeProperty
            If NodeHasItem(tag) Then
                prop = currentNode_.Item(tag).InnerText
            Else
                throwNodeException(tag)
            End If
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String(), ByVal tag As String) Implements ISerializer.serializeProperty
            Dim tagPlural = tag + "s"
            If NodeHasItem(tagPlural) Then
                setCurrentNode(tagPlural)
                Dim node As XmlNode
                ReDim prop(currentNode_.ChildNodes.Count - 1)
                Dim i As Integer = 0
                For Each node In currentNode_.ChildNodes
                    If node.Name <> tag Then
                        Throw New Exception("error processing node" & vbcrlf _
                            & "    """ & currentNode_.Name & """" & vbcrlf _
                            & "expected all children to have tag" & vbcrlf _
                            & "    """ & tag & """" & vbCrLf _
                            & "but detected a child with tag" & vbCrLf _
                            & "    """ & node.Name & """")
                    End If
                    prop(i) = node.InnerText
                    i = i + 1
                Next
                currentNode_ = currentNode_.ParentNode
            Else
                throwNodeException(tagPlural)
            End If
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Boolean, ByVal tag As String) Implements ISerializer.serializeProperty
            If NodeHasItem(tag) Then
                Dim valStr As String = ""
                serializeProperty(valStr, tag)
                Try
                    prop = CBool(valStr)
                Catch ex As Exception
                    Throw New Exception("error processing node """ & currentNode_.Name _
                        & """ with child """ & tag & """ - could not convert value """ _
                        & valStr & """ to boolean")
                End Try
            Else
                throwNodeException(tag)
            End If
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Integer, ByVal tag As String) Implements ISerializer.serializeProperty
            If NodeHasItem(tag) Then
                Dim valStr As String = ""
                serializeProperty(valStr, tag)
                Try
                    prop = CInt(valStr)
                Catch ex As Exception
                    Throw New Exception("error processing node """ & currentNode_.Name _
                        & """ with child """ & tag & """ - could not convert value """ _
                        & valStr & """ to integer")
                End Try
            Else
                throwNodeException(tag)
            End If
        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' private utility functions
        ''''''''''''''''''''''''''''''''''''''''''

        Private Sub setCurrentNode(ByVal tag As String)
            If NodeHasItem(tag) Then
                currentNode_ = currentNode_.Item(tag)
            Else
                throwNodeException(tag)
            End If
        End Sub

        Private Sub throwNodeException(ByVal tag As String)
            Throw New Exception("the node """ & currentNode_.Name & _
                """ has no child named """ & tag & """")
        End Sub

    End Class

End Namespace
