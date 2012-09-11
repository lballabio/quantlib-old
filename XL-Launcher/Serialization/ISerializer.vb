
Namespace XL_Launcher

    ''' <summary>
    ''' Abstract interface for any object capable of reading or writing a stream.
    ''' </summary>
    ''' <remarks></remarks>
    Public Interface ISerializer

        Sub serializeObject(ByRef serializable As ISerializable, ByVal className As String, ByVal versionNumber As Integer)
        Sub serializeObjectCollection(ByRef serializableList As Collection, ByVal className As String, _
            ByVal versionNumber As Integer, Optional ByVal nodeRequired As Boolean = True, _
            Optional ByVal autoName As Boolean = False, Optional ByVal collectionName As String = Nothing, _
            Optional ByVal descend As Boolean = True)
        Sub serializeObjectCollection(ByRef serializableList As Collection, ByVal nodeName As String, _
            ByVal classNames() As String, ByVal versionNumber As Integer, Optional ByVal nodeRequired As Boolean = True, _
            Optional ByVal autoName As Boolean = False, Optional ByVal collectionName As String = Nothing, _
            Optional ByVal descend As Boolean = True)
        Sub serializeAttribute(ByRef attr As String, ByVal tag As String, Optional ByVal defaultValue As String = Nothing)
        ' Default value declared as String rather than Boolean so that the presence
        ' of the optional parameter can be tested with "If defaultValue Is Nothing"
        Sub serializeAttribute(ByRef attr As Boolean, ByVal tag As String, Optional ByVal defaultValue As String = Nothing)
        Sub serializeAttribute(ByRef attr As Integer, ByVal tag As String)
        Sub serializeAttribute(ByRef attr As Date, ByVal tag As String)
        Sub serializePropertyList(ByRef attr() As String, ByVal listTag As String, ByVal itemTag As String)
        Sub serializeProperty(ByRef prop As String, ByVal tag As String, Optional ByVal defaultValue As String = Nothing)
        Sub serializeProperty(ByRef prop As Boolean, ByVal tag As String, Optional ByVal defaultValue As String = Nothing)
        Sub serializeProperty(ByRef prop As Integer, ByVal tag As String, Optional ByVal defaultValue As String = Nothing)
        Sub close()

    End Interface

End Namespace
