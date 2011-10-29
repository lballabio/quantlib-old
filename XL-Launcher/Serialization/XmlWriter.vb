
Imports System
Imports System.Xml
Imports System.IO

Namespace XL_Launcher

    ''' <summary>
    ''' A concrete instance of an ISerializer for generating an XML stream.
    ''' </summary>
    ''' <remarks></remarks>
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

            doc_.WriteStartElement(serializable.nodeName)
            serializable.serialize(Me, versionNumber)
            doc_.WriteEndElement()

        End Sub

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, ByVal className As String, _
            ByVal versionNumber As Integer, Optional ByVal nodeRequired As Boolean = True, _
            Optional ByVal autoName As Boolean = False, Optional ByVal collectionName As String = Nothing, _
            Optional ByVal descend As Boolean = True) Implements ISerializer.serializeObjectCollection

            If collectionName Is Nothing Then collectionName = className & "s"
            If descend Then doc_.WriteStartElement(collectionName)
            For Each serializable As ISerializable In serializableCollection
                serializeObject(serializable, serializable.nodeName, versionNumber)
            Next
            If descend Then doc_.WriteEndElement()

        End Sub

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, ByVal nodeName As String, _
            ByVal classNames() As String, ByVal versionNumber As Integer, Optional ByVal nodeRequired As Boolean = True, _
            Optional ByVal autoName As Boolean = False, Optional ByVal collectionName As String = Nothing, _
            Optional ByVal descend As Boolean = True) Implements ISerializer.serializeObjectCollection

            serializeObjectCollection(serializableCollection, nodeName, versionNumber, nodeRequired, autoName, collectionName, descend)

        End Sub

        Public Sub serializeAttribute(ByRef attr As String, ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeAttribute

            doc_.WriteAttributeString(tag, attr)

        End Sub

        Public Sub serializeAttribute(ByRef attr As Boolean, ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeAttribute

            serializeAttribute(CStr(attr), tag)

        End Sub

        Public Sub serializeAttribute(ByRef attr As Integer, ByVal tag As String) Implements ISerializer.serializeAttribute

            serializeAttribute(CStr(attr), tag)

        End Sub

        Public Sub serializeAttribute(ByRef attr As Date, ByVal tag As String) Implements ISerializer.serializeAttribute

            serializeAttribute(CStr(attr), tag)

        End Sub

        Public Sub serializePropertyList(ByRef attr() As String, ByVal listTag As String, ByVal itemTag As String) Implements ISerializer.serializePropertyList

            doc_.WriteStartElement(listTag)
            For i As Integer = 0 To UBound(attr)
                serializeProperty(attr(i), itemTag)
            Next
            doc_.WriteEndElement()

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String, ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeProperty

            doc_.WriteStartElement(tag)
            doc_.WriteString(prop)
            doc_.WriteEndElement()

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Boolean, ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeProperty

            doc_.WriteStartElement(tag)
            doc_.WriteString(prop)
            doc_.WriteEndElement()

        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Integer, ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeProperty

            doc_.WriteStartElement(tag)
            doc_.WriteString(prop)
            doc_.WriteEndElement()

        End Sub

    End Class

End Namespace
