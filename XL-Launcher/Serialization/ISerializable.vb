
Namespace XL_Launcher

    ''' <summary>
    ''' Abstract interface for any object capable of being serialized.
    ''' </summary>
    ''' <remarks></remarks>
    Public Interface ISerializable

        ''' <summary>
        ''' Name of the node that hosts this object in XML or other stream.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        ReadOnly Property nodeName() As String

        ''' <summary>
        ''' Display name of this object, also used as object's unique key in a collection.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Property Name() As String
        Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer)

    End Interface

End Namespace
