
Imports System
Imports System.Xml

Namespace XL_Launcher

    ''' <summary>
    ''' A concrete instance of an ISerializer for loading an XML stream.
    ''' </summary>
    ''' <remarks></remarks>
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

        Public Sub New(ByVal path As String)

            doc_ = New XmlDocument()
            doc_.Load(path)
            currentNode_ = doc_

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

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, ByVal className As String, _
            ByVal versionNumber As Integer, Optional ByVal nodeRequired As Boolean = True, _
            Optional ByVal autoName As Boolean = False, Optional ByVal collectionName As String = Nothing, _
            Optional ByVal descend As Boolean = True) Implements ISerializer.serializeObjectCollection

            Dim classNames(1) As String
            classNames(0) = className
            serializeObjectCollection(serializableCollection, className, classNames, versionNumber, nodeRequired, _
                autoName, collectionName, descend)

        End Sub

        Private Function ArrayContainsString(ByVal a As Array, ByVal s As String) As Boolean

            Dim l As IList = a
            Return l.Contains(s)

        End Function

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, ByVal nodeName As String, _
            ByVal classNames() As String, ByVal versionNumber As Integer, Optional ByVal nodeRequired As Boolean = True, _
            Optional ByVal autoName As Boolean = False, Optional ByVal collectionName As String = Nothing, _
            Optional ByVal descend As Boolean = True) Implements ISerializer.serializeObjectCollection

            If collectionName Is Nothing Then collectionName = nodeName & "s"

            If descend Then

                If Not NodeHasItem(collectionName) Then

                    If nodeRequired Then

                        throwNodeException(collectionName)

                    Else

                        Exit Sub

                    End If

                End If

            Else

                If Not currentNode_.Name = collectionName Then

                    If nodeRequired Then

                        throwNodeException(collectionName)

                    Else

                        Exit Sub

                    End If

                End If

            End If

            Dim currentNode As XmlNode = currentNode_
            Dim serializable As ISerializable
            If descend Then setCurrentNode(collectionName)
            Dim i As Integer = 0

            For Each node As XmlNode In currentNode_.ChildNodes

                If XmlNodeType.Comment = node.NodeType Then Continue For

                If Not ArrayContainsString(classNames, node.Name) Then
                    Throw New Exception("Unsupported node name: '" & node.Name & "'")
                End If

                serializable = Factory.make(node.Name)
                currentNode_ = node
                serializable.serialize(Me, versionNumber)

                If autoName Then
                    serializable.Name = serializable.nodeName & CStr(i)
                    i = i + 1
                End If

                If serializableCollection.Contains(serializable.Name) Then
                    Throw New Exception("node '" & collectionName & "' contains multiple children" _
                        & " with name '" & serializable.Name & "'.")
                End If

                serializableCollection.Add(serializable, serializable.Name)

            Next

            currentNode_ = currentNode

        End Sub

        Public Sub serializeAttribute(ByRef attr As String, ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeAttribute

            If currentNode_.Attributes.ItemOf(tag) Is Nothing Then
                If defaultValue Is Nothing Then
                    throwNodeException(tag)
                Else
                    attr = defaultValue
                End If
            Else
                attr = currentNode_.Attributes.ItemOf(tag).InnerText
            End If

        End Sub

        Public Sub serializeAttribute(ByRef attr As Boolean, ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeAttribute

            If currentNode_.Attributes.ItemOf(tag) Is Nothing Then

                If defaultValue Is Nothing Then

                    throwNodeException(tag)

                Else

                    Try
                        attr = CBool(defaultValue)
                    Catch ex As Exception
                        Throw New Exception("error processing node """ & currentNode_.Name _
                            & """ with attribute """ & tag & """ - could not convert default value """ _
                            & defaultValue & """ to boolean")
                    End Try

                End If
            Else

                Dim valStr As String
                valStr = currentNode_.Attributes.ItemOf(tag).InnerText

                Try
                    attr = CBool(currentNode_.Attributes.ItemOf(tag).InnerText)
                Catch ex As Exception
                    Throw New Exception("error processing node """ & currentNode_.Name _
                        & """ with attribute """ & tag & """ - could not convert value """ _
                        & valStr & """ to boolean")
                End Try

            End If

        End Sub

        Public Sub serializeAttribute(ByRef attr As Integer, ByVal tag As String) Implements ISerializer.serializeAttribute

            If currentNode_.Attributes.ItemOf(tag) Is Nothing Then
                attr = False
            Else
                Dim valStr As String
                valStr = currentNode_.Attributes.ItemOf(tag).InnerText

                Try
                    attr = CInt(currentNode_.Attributes.ItemOf(tag).InnerText)
                Catch ex As Exception
                    Throw New Exception("error processing node """ & currentNode_.Name _
                        & """ with attribute """ & tag & """ - could not convert value """ _
                        & valStr & """ to integer")
                End Try
            End If

        End Sub

        Public Sub serializeAttribute(ByRef attr As Date, ByVal tag As String) Implements ISerializer.serializeAttribute

            If currentNode_.Attributes.ItemOf(tag) Is Nothing Then
                attr = System.DateTime.Today
            Else
                Dim valStr As String
                valStr = currentNode_.Attributes.ItemOf(tag).InnerText

                Try
                    attr = CDate(currentNode_.Attributes.ItemOf(tag).InnerText)
                Catch ex As Exception
                    Throw New Exception("error processing node """ & currentNode_.Name _
                        & """ with attribute """ & tag & """ - could not convert value """ _
                        & valStr & """ to date")
                End Try
            End If

        End Sub

        Public Sub serializePropertyList(ByRef attr() As String, ByVal listTag As String, ByVal itemTag As String) Implements ISerializer.serializePropertyList

            If Not NodeHasItem(listTag) Then throwNodeException(listTag)

            Dim listNode As XmlNode = currentNode_.Item(listTag)

            ReDim attr(listNode.ChildNodes.Count - 1)
            Dim i As Long = 0

            For Each node As XmlNode In listNode.ChildNodes

                If XmlNodeType.Comment = node.NodeType Then Continue For

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

        Public Overloads Sub serializeProperty(ByRef prop As String, ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeProperty

            If NodeHasItem(tag) Then

                prop = currentNode_.Item(tag).InnerText

            Else

                If defaultValue Is Nothing Then

                    throwNodeException(tag)

                Else

                    prop = defaultValue

                End If

            End If

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Boolean, ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeProperty

            If NodeHasItem(tag) Then

                Dim valStr As String = ""
                serializeProperty(valStr, tag)

                Try
                    prop = CBool(valStr)
                Catch ex As Exception
                    Throw New Exception("error processing node """ & currentNode_.Name _
                        & """ with property """ & tag & """ - could not convert value """ _
                        & valStr & """ to boolean")
                End Try

            Else

                If defaultValue Is Nothing Then

                    throwNodeException(tag)

                Else

                    Try
                        prop = CBool(defaultValue)
                    Catch ex As Exception
                        Throw New Exception("error processing node """ & currentNode_.Name _
                            & """ with property """ & tag & """ - could not convert default value """ _
                            & defaultValue & """ to boolean")
                    End Try

                End If

            End If

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Integer, ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeProperty

            If NodeHasItem(tag) Then

                Dim valStr As String = ""
                serializeProperty(valStr, tag)

                Try
                    prop = CInt(valStr)
                Catch ex As Exception
                    Throw New Exception("error processing node """ & currentNode_.Name _
                        & """ with property """ & tag & """ - could not convert value """ _
                        & valStr & """ to integer")
                End Try

            Else

                If defaultValue Is Nothing Then

                    throwNodeException(tag)

                Else

                    Try
                        prop = CInt(defaultValue)
                    Catch ex As Exception
                        Throw New Exception("error processing node """ & currentNode_.Name _
                            & """ with property """ & tag & """ - could not convert default value """ _
                            & defaultValue & """ to integer")
                    End Try

                End If

            End If

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
